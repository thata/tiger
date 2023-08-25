let eval_with_env src env =
  let buff = Lexing.from_string src in
  let expr = Tiger.Parser.program Tiger.Lexer.token buff in
  Tiger.Eval.f expr env

let eval src =
  eval_with_env src []

(* 整数リテラル *)
let () =
  let src = "4649" in
  print_string "result: ";
  Tiger.Eval.print_val (eval src);
  print_newline ()

(* 足し算 *)
let () =
  let src = "10 + 20 + 30" in
  print_string "result: ";
  Tiger.Eval.print_val (eval src);
  print_newline ()

(* 引き算 *)
let () =
  let src = "50 - 20 - 10" in
  print_string "result: ";
  Tiger.Eval.print_val (eval src);
  print_newline ()

(* 掛け算 *)
let () =
  let src = "1 + 2 * 3 - 4" in
  print_string "result: ";
  Tiger.Eval.print_val (eval src);
  print_newline ()

(* 割り算 *)
let () =
  let src = "98 + 20 / 2 / 5" in
  print_string "result: ";
  Tiger.Eval.print_val (eval src);
  print_newline ()

(* 文字列リテラル *)
let () =
  let src = "\"A\"" in
  print_string "result: ";
  Tiger.Eval.print_val (eval src);
  print_newline ()

(* 整数 = *)
let () =
  let src = "0 = 0" in
  print_string "result: ";
  Tiger.Eval.print_val (eval src);
  print_newline ()

let () =
  let src = "0 = 1" in
  print_string "result: ";
  Tiger.Eval.print_val (eval src);
  print_newline ()

(* 整数 <> *)
let () =
  let src = "0 <> 0" in
  print_string "result: ";
  Tiger.Eval.print_val (eval src);
  print_newline ()

let () =
  let src = "0 <> 1" in
  print_string "result: ";
  Tiger.Eval.print_val (eval src);
  print_newline ()

(* 整数 < *)
let () =
  let src = "0 < 0" in
  print_string "result: ";
  Tiger.Eval.print_val (eval src);
  print_newline ()

let () =
  let src = "0 < 1" in
  print_string "result: ";
  Tiger.Eval.print_val (eval src);
  print_newline ()

let () =
  let src = "1 < 0" in
  print_string "result: ";
  Tiger.Eval.print_val (eval src);
  print_newline ()

(* 整数 > *)
let () =
  let src = "0 > 0" in
  print_string "result: ";
  Tiger.Eval.print_val (eval src);
  print_newline ()

let () =
  let src = "0 > 1" in
  print_string "result: ";
  Tiger.Eval.print_val (eval src);
  print_newline ()

let () =
  let src = "1 > 0" in
  print_string "result: ";
  Tiger.Eval.print_val (eval src);
  print_newline ()

(* 整数 <= *)
let () =
  let src = "0 <= 0" in
  print_string "result: ";
  Tiger.Eval.print_val (eval src);
  print_newline ()

let () =
  let src = "0 <= 1" in
  print_string "result: ";
  Tiger.Eval.print_val (eval src);
  print_newline ()

let () =
  let src = "1 <= 0" in
  print_string "result: ";
  Tiger.Eval.print_val (eval src);
  print_newline ()

(* 整数 >= *)
let () =
  let src = "0 >= 0" in
  print_string "result: ";
  Tiger.Eval.print_val (eval src);
  print_newline ()

let () =
  let src = "0 >= 1" in
  print_string "result: ";
  Tiger.Eval.print_val (eval src);
  print_newline ()

let () =
  let src = "1 >= 0" in
  print_string "result: ";
  Tiger.Eval.print_val (eval src);
  print_newline ()

(* 変数の参照 *)
let () =
  let src = "foo" in
  print_string "result: ";
  Tiger.Eval.print_val (eval_with_env src [("foo", Tiger.Eval.IntVal(4649))]);
  print_newline ()
