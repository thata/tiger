let eval_with_env src env =
  let buff = Lexing.from_string src in
  let expr = Tiger.Parser.program Tiger.Lexer.token buff in
  let result, _ = Tiger.Eval.f expr env in
  result

let eval src =
  eval_with_env src [
    (* 組み込み関数 print(s: string) *)
    ("print", Tiger.Eval.BuiltInFunction(
      fun args _ ->
        match args with
        | [Tiger.Eval.StringVal(s)] ->
          print_string s;
          (* NOTE: 本当は unit を返したいけどまだ未実装なので 0 を返してる *)
          Tiger.Eval.IntVal(0)
        | _ -> failwith "invalid arguments"
    ))
  ]

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

(* let による変数宣言 *)
let () =
  let src = {|
    let
      var a := 10
      var b := 20
    in
      a + b
    end
  |}
  in print_string "result: ";
  Tiger.Eval.print_val (eval src);
  print_newline ()

(* 空の let *)
let () =
  let src = {|
    let
    in
      99
    end
  |}
  in print_string "result: ";
  Tiger.Eval.print_val (eval src);
  print_newline ()

(* 引数なしの関数の呼び出し *)
let () =
  let src = {|
    let
      function foo() = 5678
    in
      foo ()
    end
  |}
  in
  print_string "result: ";
  Tiger.Eval.print_val (eval src);
  print_newline ()

(* 引数ありの関数の呼び出し *)
let () =
  let src = {|
    let
      function square(n:int) = n * n
    in
      square(10)
    end
  |}
  in
  print_string "result: ";
  Tiger.Eval.print_val (eval src);
  print_newline ()

(* 組み込み関数 double の呼び出し *)
let () =
  let src = {|
    let
    in
      double(10)
    end
  |}
  in
  print_string "result: ";
  let env = [
    ("double", Tiger.Eval.BuiltInFunction (
      fun args (_:Tiger.Eval.table) ->
        match args with
        | [Tiger.Eval.IntVal(n)] -> Tiger.Eval.IntVal(n * 2)
        | _ -> failwith "invalid arguments"
    ));
  ] in
  Tiger.Eval.print_val (eval_with_env src env);
  print_newline ()

(* 組み込み関数 foo の呼び出し *)
let () =
  let src = {|
    let
    in
      foo(4, 6, 4, 9)
    end
  |}
  in
  print_string "result: ";
  let env = [
    (* (args[0] * 1000) + (args[1] * 100) + (args[2] * 10) + args[3] *)
    ("foo", Tiger.Eval.BuiltInFunction (
      fun args (_:Tiger.Eval.table) ->
        match args with
        | [Tiger.Eval.IntVal(a); Tiger.Eval.IntVal(b); Tiger.Eval.IntVal(c); Tiger.Eval.IntVal(d)] ->
          Tiger.Eval.IntVal(a * 1000 + b * 100 + c * 10 + d)
        | _ -> failwith "invalid arguments"
    ));
  ] in
  Tiger.Eval.print_val (eval_with_env src env);
  print_newline ()

(* 組み込み関数 print(s: string) の呼び出し *)
let () =
  let src = {|
    let in
      print("Hello World!!")
    end
  |}
  in
  ignore(eval src);
  print_newline ()
