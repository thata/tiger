{
    (* 補助的な変数、関数、型などを定義 *)
}

(* 正規表現の略記 *)
let space = [' ' '\t' '\r' '\n']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let letter = lower | upper
let symbol = ['!' '#' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '_' '`' '|' '~']
let printable = symbol | letter | digit

(* 字句解析の規則 *)
rule token = parse
    space+ { token lexbuf } (* 空白は読み飛ばす *)
  | "let" { Parser.LET }
  | "in" { Parser.IN }
  | "end" { Parser.END }
  | "var" { Parser.VAR }
  | "+" { Parser.PLUS }
  | "-" { Parser.MINUS }
  | "*" { Parser.TIMES }
  | "/" { Parser.DIVIDE }
  | "=" { Parser.EQ }
  | "<>" { Parser.NEQ }
  | "<" { Parser.LT }
  | "<=" { Parser.LE }
  | ">" { Parser.GT }
  | ">=" { Parser.GE }
  | ":=" { Parser.ASSIGN }
  | digit+ { Parser.INT(int_of_string(Lexing.lexeme lexbuf)) }
  | letter (letter|digit)* { Parser.ID(Lexing.lexeme lexbuf) }
  | "\"" (printable | space)* "\"" {
      (* 文字列リテラル *)
      let str = String.sub (Lexing.lexeme lexbuf) 1 ((String.length (Lexing.lexeme lexbuf)) - 2) in
      Parser.STRING(str)
    }
  | eof { Parser.EOF }
  | _ { failwith ("invalid character " ^ (Lexing.lexeme lexbuf)) }
