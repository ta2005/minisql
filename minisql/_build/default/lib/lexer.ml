type token =
  | Ident of string
  | IntLit of int
  | StringLit of string
  | Select | From | Where
  | Insert | Into | Values
  | Create | Table | Drop 
  | TInt | TString | TBool
  | And | Or | Not
  | Delete
  | True | False | Null
  | Eq | Neq | Lt | Lte | Gt | Gte
  | Plus | Minus | Mul | Div
  | LParen | RParen | Comma | Semi
  | Star
  | EOF

exception LexError of string

let keywords = [
  ("select", Select); ("from", From); ("where", Where);
  ("insert", Insert); ("into", Into); ("values", Values);
  ("create", Create); ("table", Table); ("drop", Drop);
  ("int", TInt); ("string", TString); ("bool", TBool);
  ("and", And); ("or", Or); ("not", Not);
  ("true", True); ("false", False); ("null", Null);
  ("delete", Delete)
]

let is_alpha c =
  match c with
  | 'a'..'z' | 'A'..'Z' | '_' -> true
  | _ -> false

let is_digit c =
  match c with
  | '0'..'9' -> true
  | _ -> false

let is_alphanum c = is_alpha c || is_digit c

let tokenize (input : string) : token list =
  let len = String.length input in
  let rec lex pos acc =
    if pos >= len then List.rev (EOF :: acc)
    else
      match input.[pos] with
      | ' ' | '\t' | '\n' | '\r' -> lex (pos + 1) acc
      | '(' -> lex (pos + 1) (LParen :: acc)
      | ')' -> lex (pos + 1) (RParen :: acc)
      | ',' -> lex (pos + 1) (Comma :: acc)
      | ';' -> lex (pos + 1) (Semi :: acc)
      | '*' -> lex (pos + 1) (Star :: acc)
      | '+' -> lex (pos + 1) (Plus :: acc)
      | '-' -> lex (pos + 1) (Minus :: acc)
      | '/' -> lex (pos + 1) (Div :: acc)
      | '=' -> lex (pos + 1) (Eq :: acc)
      | '!' when pos + 1 < len && input.[pos + 1] = '=' -> lex (pos + 2) (Neq :: acc)
      | '<' when pos + 1 < len && input.[pos + 1] = '=' -> lex (pos + 2) (Lte :: acc)
      | '<' -> lex (pos + 1) (Lt :: acc)
      | '>' when pos + 1 < len && input.[pos + 1] = '=' -> lex (pos + 2) (Gte :: acc)
      | '>' -> lex (pos + 1) (Gt :: acc)
      | '\'' -> lex_string (pos + 1) (pos + 1) acc
      | c when is_digit c -> lex_int pos pos acc
      | c when is_alpha c -> lex_ident pos pos acc
      | c -> raise (LexError (Printf.sprintf "Unexpected character '%c' at position %d" c pos))
  
  and lex_string pos start acc =
    if pos >= len then raise (LexError "Unterminated string literal")
    else if input.[pos] = '\'' then
      let str = String.sub input start (pos - start) in
      lex (pos + 1) (StringLit str :: acc)
    else
      lex_string (pos + 1) start acc

  and lex_int pos start acc =
    if pos < len && is_digit input.[pos] then
      lex_int (pos + 1) start acc
    else
      let str = String.sub input start (pos - start) in
      lex pos (IntLit (int_of_string str) :: acc)

  and lex_ident pos start acc =
    if pos < len && is_alphanum input.[pos] then
      lex_ident (pos + 1) start acc
    else
      let str = String.sub input start (pos - start) in
      let lower_str = String.lowercase_ascii str in
      let tok =
        try List.assoc lower_str keywords
        with Not_found -> Ident str
      in
      lex pos (tok :: acc)
  in
  lex 0 []
