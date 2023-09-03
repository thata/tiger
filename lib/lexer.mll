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
  | "/*" { comment lexbuf } (* コメントは読み飛ばす *)
  | "let" { Parser.LET }
  | "in" { Parser.IN }
  | "end" { Parser.END }
  | "var" { Parser.VAR }
  | "function" { Parser.FUNCTION }
  | "if" { Parser.IF }
  | "then" { Parser.THEN }
  | "else" { Parser.ELSE }
  | "+" { Parser.PLUS }
  | "-" { Parser.MINUS }
  | "*" { Parser.TIMES }
  | "/" { Parser.DIVIDE }
  | "&" { Parser.AND }
  | "|" { Parser.OR }
  | "=" { Parser.EQ }
  | "<>" { Parser.NEQ }
  | "<" { Parser.LT }
  | "<=" { Parser.LE }
  | ">" { Parser.GT }
  | ">=" { Parser.GE }
  | ":=" { Parser.ASSIGN }
  | "(" { Parser.LPAREN }
  | ")" { Parser.RPAREN }
  | "," { Parser.COMMA }
  | ":" { Parser.COLON }
  | ";" { Parser.SEMICOLON }
  | digit+ { Parser.INT(int_of_string(Lexing.lexeme lexbuf)) }
  | letter (letter|digit)* { Parser.ID(Lexing.lexeme lexbuf) }
  | "\"" { string_literal (Buffer.create 0) lexbuf }
  | eof { Parser.EOF }
  | _ { failwith ("invalid character " ^ (Lexing.lexeme lexbuf)) }

and comment = parse
    "*/" { token lexbuf }
  | _ { comment lexbuf }

and string_literal buf = parse
    "\\t" { Buffer.add_string buf "\x09"; string_literal buf lexbuf }
  | "\\n" { Buffer.add_string buf "\x0A"; string_literal buf lexbuf }
  | "\\\\" { Buffer.add_string buf "\x5C"; string_literal buf lexbuf }
  | "\\\"" { Buffer.add_string buf "\x22"; string_literal buf lexbuf }
  | (printable | ' ')+ { Buffer.add_string buf (Lexing.lexeme lexbuf); string_literal buf lexbuf }
  | "\"" { Parser.STRING(Buffer.contents buf) }
  | _ { string_literal buf lexbuf }
