{
    (* 補助的な変数、関数、型などを定義 *)
}

(* 正規表現の略記 *)
let space = [' ' '\t' '\r' '\n']
let digit = ['0'-'9']

(* 字句解析の規則 *)
rule token = parse
    space+ { token lexbuf } (* 空白は読み飛ばす *)
  | digit+ { Parser.INT(int_of_string(Lexing.lexeme lexbuf)) }
  | "+" { Parser.PLUS }
  | "-" { Parser.MINUS }
  | "*" { Parser.TIMES }
  | "/" { Parser.DIVIDE }
  | eof { Parser.EOF }
  | _ { failwith ("invalid character " ^ (Lexing.lexeme lexbuf)) }
