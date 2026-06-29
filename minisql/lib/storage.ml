open Types

exception StorageError of string

(** A table is a schema and a list of rows *)
type table = {
  schema : schema;
  rows : row list;
}

(** The database is a mapping from table names to tables. *)
type db = (string * table) list

(** Create a new empty database *)
let create_db () : db = []

(** Get a table from the database *)
let get_table (db : db) (name : string) : table =
  try List.assoc name db
  with Not_found -> raise (StorageError (Printf.sprintf "Table '%s' not found" name))

(** Add a new table to the database *)
let create_table (db : db) (name : string) (schema : schema) : db =
  if List.mem_assoc name db then
    raise (StorageError (Printf.sprintf "Table '%s' already exists" name))
  else
    (name, { schema; rows = [] }) :: db

(** Drop a table from the database *)
let drop_table (db : db) (name : string) : db =
  if not (List.mem_assoc name db) then
    raise (StorageError (Printf.sprintf "Table '%s' not found" name))
  else
    List.remove_assoc name db

let check_type (a: value) (b: column_def) =
    match b.data_type, a with 
    | TInt  ,VInt _-> true
    | TString  ,VString _-> true
    | TBool  ,VBool _-> true
    | _ -> false 


(** Insert a row into a table *)
let insert_row (db : db) (name : string) (row : row) : db =
  let t = get_table db name in
  if List.length t.schema <> List.length row then
    raise (StorageError "Row length does not match schema")
  else if not( List.for_all2 check_type row t.schema) then
    raise (StorageError "Type mismatch")
  else 
    (* Prepend row. In a real DB we'd append or insert into an index *)
    let new_table = { t with rows = row :: t.rows } in
    (name, new_table) :: List.remove_assoc name db

