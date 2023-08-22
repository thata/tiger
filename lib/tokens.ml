type t =
    TYPE
  | VAR
  | FUNCTION
  | BREAK
  | OF
  | END
  | IN
  | NIL
  | LET
  | DO
  | TO
  | FOR
  | WHILE
  | ELSE
  | THEN
  | IF
  | ARRAY
  | ASSIGN
  | OR
  | AND
  | GE
  | GT
  | LE
  | LT
  | NEQ
  | EQ
  | DIVIDE
  | TIMES
  | MINUS
  | PLUS
  | DOT
  | RBRACE
  | LBRACE
  | RBRACK
  | LBRACK
  | RPAREN
  | LPAREN
  | SEMICOLON
  | COLON
  | COMMA
  | STRING of string
  | INT of int
  | ID of string
  | EOF

  let string_of_token token =
    match token with
      TYPE -> "TYPE"
    | VAR -> "VAR"
    | FUNCTION -> "FUNCTION"
    | BREAK -> "BREAK"
    | OF -> "OF"
    | END -> "END"
    | IN -> "IN"
    | NIL -> "NIL"
    | LET -> "LET"
    | DO -> "DO"
    | TO -> "TO"
    | FOR -> "FOR"
    | WHILE -> "WHILE"
    | ELSE -> "ELSE"
    | THEN -> "THEN"
    | IF -> "IF"
    | ARRAY -> "ARRAY"
    | ASSIGN -> "ASSIGN"
    | OR -> "OR"
    | AND -> "AND"
    | GE -> "GE"
    | GT -> "GT"
    | LE -> "LE"
    | LT -> "LT"
    | NEQ -> "NEQ"
    | EQ -> "EQ"
    | DIVIDE -> "DIVIDE"
    | TIMES -> "TIMES"
    | MINUS -> "MINUS"
    | PLUS -> "PLUS"
    | DOT -> "DOT"
    | RBRACE -> "RBRACE"
    | LBRACE -> "LBRACE"
    | RBRACK -> "RBRACK"
    | LBRACK -> "LBRACK"
    | RPAREN -> "RPAREN"
    | LPAREN -> "LPAREN"
    | SEMICOLON -> "SEMICOLON"
    | COLON -> "COLON"
    | COMMA -> "COMMA"
    | STRING s -> "STRING " ^ s
    | INT i -> "INT " ^ string_of_int i
    | ID s -> "ID " ^ s
    | EOF -> "EOF"

let print token = print_string (string_of_token token)
