open Minisql.Storage
open Minisql.Engine

let print_welcome () =
  print_endline "Welcome to MiniSQL!";
  print_endline "Type your SQL-like queries below. Type 'exit' or 'quit' to exit.";
  print_endline "Examples:";
  print_endline "  CREATE TABLE users (id INT, name STRING, active BOOL);";
  print_endline "  INSERT INTO users VALUES (1, 'Alice', true);";
  print_endline "  SELECT * FROM users WHERE active = true;"

let rec repl db =
  print_string "minisql> ";
  flush stdout;
  try
    let input = input_line stdin in
    let trimmed = String.trim input in
    if trimmed = "exit" || trimmed = "quit" || trimmed = "exit;" || trimmed = "quit;" then
      print_endline "Bye!"
    else if trimmed = "" then
      repl db
    else
      let (db', output_opt) = execute_query db trimmed in
      (match output_opt with
       | Some out -> print_endline out
       | None -> ());
      repl db'
  with
  | End_of_file -> print_endline "\nBye!"
  | Sys.Break -> print_endline "\nInterrupted."; repl db

let () =
  Sys.catch_break true;
  print_welcome ();
  repl (create_db ())
