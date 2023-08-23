let eval src =
  let buff = Lexing.from_string src in
  let expr = Tiger.Parser.program Tiger.Lexer.token buff in
  Tiger.Eval.f expr

(* 整数リテラル *)
let () =
  let src = "4649" in
  print_string "result: ";
  print_int (eval src);
  print_newline ()

(* 足し算 *)
let () =
  let src = "10 + 20 + 30" in
  print_string "result: ";
  print_int (eval src);
  print_newline ()

(* 引き算 *)
let () =
  let src = "50 - 20 - 10" in
  print_string "result: ";
  print_int (eval src);
  print_newline ()

(* 掛け算 *)
let () =
  let src = "1 + 2 * 3 - 4" in
  print_string "result: ";
  print_int (eval src);
  print_newline ()

(* 割り算 *)
let () =
  let src = "98 + 20 / 2 / 5" in
  print_string "result: ";
  print_int (eval src);
  print_newline ()
