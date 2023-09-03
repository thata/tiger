(* 標準入力から受け取ったTiger言語を評価する *)
let () =
  (* 引数で指定したファイル、または stdin から入力を受け付ける *)
  let buff = match Array.length Sys.argv with
    | 1 -> Lexing.from_channel stdin
    | 2 -> Lexing.from_channel (open_in Sys.argv.(1))
    | _ -> failwith "invalid arguments"
  in
  let expr = Tiger.Parser.program Tiger.Lexer.token buff in
  let built_in_functions = [
    (* 組み込み関数 print(s: string) *)
    ("print", Tiger.Eval.BuiltInFunction(
      fun args _ ->
        match args with
        | [Tiger.Eval.StringVal(s)] ->
          print_string s;
          Tiger.Eval.UnitVal
        | _ -> failwith "invalid arguments"
    ))
  ] in
  let env = built_in_functions in
  let result, _ = Tiger.Eval.f expr env in
  print_string "#=> ";
  Tiger.Eval.print_val result;
  print_newline ()
