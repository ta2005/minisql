(** Data types supported by MiniSQL *)
type data_type =
  | TInt
  | TString
  | TBool

(** Values that can be stored in the database *)
type value =
  | VInt of int
  | VString of string
  | VBool of bool
  | VNull

(** A column definition in a schema *)
type column_def = {
  name: string;
  data_type: data_type;
}

(** A schema is a list of column definitions *)
type schema = column_def list

(** A row is a list of values, corresponding to the schema *)
type row = value list

(** Converts a value to its string representation *)
let string_of_value = function
  | VInt i -> string_of_int i
  | VString s -> Printf.sprintf "'%s'" s
  | VBool b -> string_of_bool b
  | VNull -> "NULL"

(** Converts a data type to its string representation *)
let string_of_data_type = function
  | TInt -> "INT"
  | TString -> "STRING"
  | TBool -> "BOOL"
