open Ast
open Types

exception EvalError of string

(** Evaluate an expression against a given row and schema *)
let rec eval_expr (e : expr) (row : row) (schema : schema) : value =
  match e with
  | Literal v -> v
  | Column name ->
      let rec find_col cols vals =
        match cols, vals with
        | {name=n; _} :: _, v :: _ when n = name -> v
        | _ :: cs, _ :: vs -> find_col cs vs
        | [], [] -> raise (EvalError (Printf.sprintf "Column '%s' not found" name))
        | _ -> raise (EvalError "Schema and row length mismatch")
      in
      find_col schema row
  | Not e1 ->
      (match eval_expr e1 row schema with
       | VBool b -> VBool (not b)
       | _ -> raise (EvalError "NOT operator requires a boolean operand"))
  | BinOp (e1, op, e2) ->
      let v1 = eval_expr e1 row schema in
      let v2 = eval_expr e2 row schema in
      eval_binop v1 op v2

and eval_binop v1 op v2 =
  match v1, op, v2 with
  (* Arithmetic *)
  | VInt i1, Add, VInt i2 -> VInt (i1 + i2)
  | VInt i1, Sub, VInt i2 -> VInt (i1 - i2)
  | VInt i1, Mul, VInt i2 -> VInt (i1 * i2)
  | VInt i1, Div, VInt i2 -> if i2 = 0 then raise (EvalError "Division by zero") else VInt (i1 / i2)
  
  (* Equality *)
  | VInt i1, Eq, VInt i2 -> VBool (i1 = i2)
  | VString s1, Eq, VString s2 -> VBool (s1 = s2)
  | VBool b1, Eq, VBool b2 -> VBool (b1 = b2)
  | _, Eq, VNull | VNull, Eq, _ -> VBool false

  | VInt i1, Neq, VInt i2 -> VBool (i1 <> i2)
  | VString s1, Neq, VString s2 -> VBool (s1 <> s2)
  | VBool b1, Neq, VBool b2 -> VBool (b1 <> b2)
  
  (* Comparisons *)
  | VInt i1, Lt, VInt i2 -> VBool (i1 < i2)
  | VInt i1, Lte, VInt i2 -> VBool (i1 <= i2)
  | VInt i1, Gt, VInt i2 -> VBool (i1 > i2)
  | VInt i1, Gte, VInt i2 -> VBool (i1 >= i2)

  (* Logic *)
  | VBool b1, And, VBool b2 -> VBool (b1 && b2)
  | VBool b1, Or, VBool b2 -> VBool (b1 || b2)

  | _ -> raise (EvalError "Type mismatch in binary operation")
