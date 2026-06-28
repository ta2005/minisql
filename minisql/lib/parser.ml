open Lexer
open Ast
open Types

exception ParseError of string

(** Helper function to consume a specific token *)
let expect (tok : token) (tokens : token list) : token list =
  match tokens with
  | t :: ts when t = tok -> ts
  | t :: _ -> raise (ParseError "Unexpected token")
  | [] -> raise (ParseError "Unexpected end of input")

(** Parses an identifier *)
let parse_ident (tokens : token list) : string * token list =
  match tokens with
  | Ident id :: ts -> (id, ts)
  | _ -> raise (ParseError "Expected identifier")

(** Parses an expression. For simplicity, we just parse basic left-associative binary ops and literals without full operator precedence. *)
let rec parse_expr (tokens : token list) : expr * token list =
  let (lhs, tokens') = parse_primary tokens in
  parse_binop_rhs lhs tokens'

and parse_primary (tokens : token list) : expr * token list =
  match tokens with
  | IntLit i :: ts -> (Literal (VInt i), ts)
  | StringLit s :: ts -> (Literal (VString s), ts)
  | True :: ts -> (Literal (VBool true), ts)
  | False :: ts -> (Literal (VBool false), ts)
  | Null :: ts -> (Literal VNull, ts)
  | Ident id :: ts -> (Column id, ts)
  | LParen :: ts ->
      let (e, ts') = parse_expr ts in
      let ts'' = expect RParen ts' in
      (e, ts'')
  | _ -> raise (ParseError "Expected expression")

and parse_binop_rhs (lhs : expr) (tokens : token list) : expr * token list =
  match tokens with
  | Eq :: ts -> let (rhs, ts') = parse_primary ts in parse_binop_rhs (BinOp (lhs, Eq, rhs)) ts'
  | Neq :: ts -> let (rhs, ts') = parse_primary ts in parse_binop_rhs (BinOp (lhs, Neq, rhs)) ts'
  | Lt :: ts -> let (rhs, ts') = parse_primary ts in parse_binop_rhs (BinOp (lhs, Lt, rhs)) ts'
  | Lte :: ts -> let (rhs, ts') = parse_primary ts in parse_binop_rhs (BinOp (lhs, Lte, rhs)) ts'
  | Gt :: ts -> let (rhs, ts') = parse_primary ts in parse_binop_rhs (BinOp (lhs, Gt, rhs)) ts'
  | Gte :: ts -> let (rhs, ts') = parse_primary ts in parse_binop_rhs (BinOp (lhs, Gte, rhs)) ts'
  | And :: ts -> let (rhs, ts') = parse_primary ts in parse_binop_rhs (BinOp (lhs, And, rhs)) ts'
  | Or :: ts -> let (rhs, ts') = parse_primary ts in parse_binop_rhs (BinOp (lhs, Or, rhs)) ts'
  | Plus :: ts -> let (rhs, ts') = parse_primary ts in parse_binop_rhs (BinOp (lhs, Add, rhs)) ts'
  | Minus :: ts -> let (rhs, ts') = parse_primary ts in parse_binop_rhs (BinOp (lhs, Sub, rhs)) ts'
  | Mul :: ts -> let (rhs, ts') = parse_primary ts in parse_binop_rhs (BinOp (lhs, Mul, rhs)) ts'
  | Div :: ts -> let (rhs, ts') = parse_primary ts in parse_binop_rhs (BinOp (lhs, Div, rhs)) ts'
  | _ -> (lhs, tokens) (* No more binary operators, return the LHS *)

(** Parse a WHERE clause (optional) *)
let parse_where (tokens : token list) : expr option * token list =
  match tokens with
  | Where :: ts ->
      let (e, ts') = parse_expr ts in
      (Some e, ts')
  | _ -> (None, tokens)

(** Parse a SELECT statement *)
let parse_select (tokens : token list) : statement * token list =
  (* For simplicity, we just parse "SELECT *" or "SELECT col1, col2" *)
  let rec parse_select_list ts acc =
    match ts with
    | Star :: ts' -> (["*"], ts')
    | Ident id :: Comma :: ts' -> parse_select_list ts' (id :: acc)
    | Ident id :: ts' -> (List.rev (id :: acc), ts')
    | _ -> raise (ParseError "Invalid select list")
  in
  let (select_list, ts1) = parse_select_list tokens [] in
  let ts2 = expect From ts1 in
  let (from_table, ts3) = parse_ident ts2 in
  let (where_clause, ts4) = parse_where ts3 in
  (Select { select_list; from_table; where_clause }, ts4)

(** Parse a list of literal values for INSERT *)
let rec parse_values ts acc =
  let (e, ts1) = parse_primary ts in
  let v = match e with
    | Literal v -> v
    | _ -> raise (ParseError "Only literal values allowed in INSERT")
  in
  match ts1 with
  | Comma :: ts2 -> parse_values ts2 (v :: acc)
  | RParen :: ts2 -> (List.rev (v :: acc), ts2)
  | _ -> raise (ParseError "Expected ',' or ')' in VALUES clause")

(** Parse an INSERT statement *)
let parse_insert (tokens : token list) : statement * token list =
  let ts1 = expect Into tokens in
  let (into_table, ts2) = parse_ident ts1 in
  let ts3 = expect Values ts2 in
  let ts4 = expect LParen ts3 in
  let (values, ts5) = parse_values ts4 [] in
  (Insert { into_table; values }, ts5)

(** Parse a column definition for CREATE TABLE *)
let parse_column_def (tokens : token list) : column_def * token list =
  let (name, ts1) = parse_ident tokens in
  let (data_type, ts2) =
    match ts1 with
    | TInt :: ts -> (TInt, ts)
    | TString :: ts -> (TString, ts)
    | TBool :: ts -> (TBool, ts)
    | _ -> raise (ParseError "Expected data type (INT, STRING, or BOOL)")
  in
  ({ name; data_type }, ts2)

(** Parse a list of column definitions *)
let rec parse_columns ts acc =
  let (col, ts1) = parse_column_def ts in
  match ts1 with
  | Comma :: ts2 -> parse_columns ts2 (col :: acc)
  | RParen :: ts2 -> (List.rev (col :: acc), ts2)
  | _ -> raise (ParseError "Expected ',' or ')' in column definitions")

(** Parse a CREATE TABLE statement *)
let parse_create (tokens : token list) : statement * token list =
  let ts1 = expect Table tokens in
  let (table_name, ts2) = parse_ident ts1 in
  let ts3 = expect LParen ts2 in
  let (columns, ts4) = parse_columns ts3 [] in
  (CreateTable { table_name; columns }, ts4)

(** Parse a DROP TABLE statement *)
let parse_drop (tokens : token list) : statement * token list =
  let ts1 = expect Table tokens in
  let (table_name, ts2) = parse_ident ts1 in
  (DropTable table_name, ts2)


(** Parse a Delete From stmt *)
let parse_delete tokens : statement * token list =
    let ts1 = expect From tokens in
    let (from_table,ts2) = parse_ident ts1 in
    let (where_clause, ts4) = parse_where ts2 in
    (Delete { from_table; where_clause }, ts4)



(** Parse a single statement *)
let parse_statement (tokens : token list) : statement * token list =
  match tokens with
  | Select :: ts -> parse_select ts
  | Insert :: ts -> parse_insert ts
  | Create :: ts -> parse_create ts
  | Drop :: ts -> parse_drop ts
  | Delete :: ts -> parse_delete ts
  | EOF :: _ -> raise (ParseError "Unexpected EOF")
  | _ -> raise (ParseError "Expected statement (SELECT, INSERT, CREATE, DROP)")

(** Parses a string into a statement *)
let parse (input : string) : statement =
  let tokens = tokenize input in
  let (stmt, remaining) = parse_statement tokens in
  match remaining with
  | EOF :: _ | Semi :: EOF :: _ | [] -> stmt
  | Semi :: remaining' ->
      (match remaining' with
       | EOF :: _ | [] -> stmt
       | _ -> raise (ParseError "Expected EOF after statement (multiple statements not supported)"))
  | _ -> raise (ParseError "Expected EOF after statement")
