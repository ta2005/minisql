open Ast
open Types
open Storage
open Eval

exception EngineError of string

(** Format a row as a string *)
let string_of_row (r : row) : string =
  let vals = List.map string_of_value r in
  "| " ^ String.concat " | " vals ^ " |"

(** Execute a single statement against the database, returning the new state of the db
    and an optional string result (like the output of a SELECT statement). *)
let execute_stmt (db : db) (stmt : statement) : db * string option =
  match stmt with
  | CreateTable { table_name; columns } ->
      let db' = create_table db table_name columns in
      (db', Some (Printf.sprintf "Table '%s' created." table_name))

  | DropTable table_name ->
      let db' = drop_table db table_name in
      (db', Some (Printf.sprintf "Table '%s' dropped." table_name))

  | Insert { into_table; values } ->
      let db' = insert_row db into_table values in
      (db', Some "1 row inserted.")

  | Delete { from_table; where_clause} ->
      let table = get_table db from_table in
      let kept_rows =
        match where_clause with
        | None -> table.rows
        | Some expr ->
            List.filter (fun row ->
              match eval_expr expr row table.schema with
              | VBool true -> false
              | VBool false | VNull -> true
              | _ -> raise (EngineError "WHERE clause must evaluate to a boolean")
            ) table.rows
      in 
      let new_table = { table with rows = kept_rows } in
      let db' = (from_table, new_table)::List.remove_assoc from_table db in
      (db',Some "table delted")

  | Select { select_list; from_table; where_clause } ->
      let table = get_table db from_table in
      
      (* Filter rows based on WHERE clause *)
      let filtered_rows =
        match where_clause with
        | None -> table.rows
        | Some expr ->
            List.filter (fun row ->
              match eval_expr expr row table.schema with
              | VBool true -> true
              | VBool false | VNull -> false
              | _ -> raise (EngineError "WHERE clause must evaluate to a boolean")
            ) table.rows
      in

      (* Project rows based on SELECT list (only handle "*" for now) *)
      let projected_rows =
        if select_list = ["*"] then
          filtered_rows
        else
          raise (EngineError "Only SELECT * is currently supported")
      in

      (* Format output *)
      let header = "| " ^ String.concat " | " (List.map (fun c -> c.name) table.schema) ^ " |" in
      let separator = String.make (String.length header) '-' in
      let row_strings = List.map string_of_row (List.rev projected_rows) in (* Reverse because we prepend on insert *)
      
      let output = String.concat "\n" (header :: separator :: row_strings) in
      let footer = Printf.sprintf "\n(%d rows)" (List.length projected_rows) in
      (db, Some (output ^ footer))

(** Execute a query string *)
let execute_query (db : db) (query : string) : db * string option =
  try
    let stmt = Parser.parse query in
    execute_stmt db stmt
  with
  | Lexer.LexError msg -> (db, Some ("Lex Error: " ^ msg))
  | Parser.ParseError msg -> (db, Some ("Parse Error: " ^ msg))
  | Eval.EvalError msg -> (db, Some ("Eval Error: " ^ msg))
  | Storage.StorageError msg -> (db, Some ("Storage Error: " ^ msg))
  | EngineError msg -> (db, Some ("Engine Error: " ^ msg))
  | e -> (db, Some ("Unknown Error: " ^ Printexc.to_string e))
