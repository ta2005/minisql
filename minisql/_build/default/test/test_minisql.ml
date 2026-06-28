open OUnit2
open Minisql
open Minisql.Ast
open Minisql.Types

let test_lexer _ =
  let tokens = Lexer.tokenize "SELECT * FROM users;" in
  assert_equal [Lexer.Select; Lexer.Star; Lexer.From; Lexer.Ident "users"; Lexer.Semi; Lexer.EOF] tokens

let test_parser _ =
  let stmt = Parser.parse "SELECT * FROM users WHERE active = true;" in
  match stmt with
  | Select { select_list; from_table; where_clause } ->
      assert_equal ["*"] select_list;
      assert_equal "users" from_table;
      assert_equal (Some (BinOp (Column "active", Eq, Literal (VBool true)))) where_clause
  | _ -> assert_failure "Expected Select statement"

let test_engine _ =
  let db = Storage.create_db () in
  let (db1, _) = Engine.execute_query db "CREATE TABLE users (id INT, name STRING, active BOOL);" in
  let (db2, _) = Engine.execute_query db1 "INSERT INTO users VALUES (1, 'Alice', true);" in
  let (db3, _) = Engine.execute_query db2 "INSERT INTO users VALUES (2, 'Bob', false);" in
  let (_, out) = Engine.execute_query db3 "SELECT * FROM users WHERE active = true;" in
  
  match out with
  | Some s ->
      assert_bool "Output should contain Alice" (String.contains s 'A');
      assert_bool "Output should not contain Bob" (not (String.contains s 'B'))
  | None -> assert_failure "Expected output"

let suite =
  "MiniSQL Test Suite" >::: [
    "test_lexer" >:: test_lexer;
    "test_parser" >:: test_parser;
    "test_engine" >:: test_engine;
  ]

let () =
  run_test_tt_main suite
