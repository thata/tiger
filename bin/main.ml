(* 標準入力から受け取ったTiger言語を評価する *)
let () =
  let buff = Lexing.from_channel stdin in
  let expr = Tiger.Parser.program Tiger.Lexer.token buff in
  let result, _ = Tiger.Eval.f expr [
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
  ] in
  print_string "#=> ";
  Tiger.Eval.print_val result;
  print_newline ()
