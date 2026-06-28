(** Binary operations used in expressions *)
type binop =
  | Add | Sub | Mul | Div
  | Eq | Neq | Lt | Lte | Gt | Gte
  | And | Or

(** Expressions that can be evaluated to a value *)
type expr =
  | Literal of Types.value
  | Column of string
  | BinOp of expr * binop * expr
  | Not of expr

(** SQL Statements *)
type statement =
  | Select of {
      select_list: string list; (* "*" means all columns, we'll represent it as ["*"] for simplicity *)
      from_table: string;
      where_clause: expr option;
    }
  | Insert of {
      into_table: string;
      values: Types.value list;
    }
  | CreateTable of {
      table_name: string;
      columns: Types.column_def list;
    }
  | DropTable of string

(** Convert a binary operator to string *)
let string_of_binop = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
  | Eq -> "=" | Neq -> "!=" | Lt -> "<" | Lte -> "<=" | Gt -> ">" | Gte -> ">="
  | And -> "AND" | Or -> "OR"
