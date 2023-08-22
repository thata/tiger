(* 標準入力から受け取ったTiger言語を評価する *)
let () =
  let buff = Lexing.from_channel stdin in
  let expr = Tiger.Parser.program Tiger.Lexer.token buff in
  let result = Tiger.Eval.f expr in
  print_string "Result: ";
  print_int result;
  print_newline ()
