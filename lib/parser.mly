%{
%}

// トークンの定義
%token EOF
%token <int> INT
%token <string> STRING
%token <string> ID
%token PLUS MINUS TIMES DIVIDE
%token EQ NEQ LT LE GT GE
%token LET IN END IF THEN ELSE
%token VAR FUNCTION ASSIGN
%token LPAREN RPAREN COMMA COLON SEMICOLON
%token EOF

// あとで使う
// %token COMMA COLON SEMICOLON LPAREN RPAREN LBRACK RBRACK
// %token LBRACE RBRACE DOT
// %token AND OR
// %token ARRAY WHILE FOR TO DO OF
// %token BREAK NIL
// %token FUNCTION TYPE

// エントリーボイントの定義
%start program

%type <Syntax.t> program

// see: https://kenichi-asai.github.io/lex-yacc/#else-%E3%81%AE%E3%81%AA%E3%81%84%E6%9D%A1%E4%BB%B6%E6%96%87
%nonassoc THEN
%nonassoc ELSE
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc EQ NEQ LT LE GT GE

// 文法規則の定義
%%
program: exp EOF { $1 }

exp:
  INT { Syntax.IntExp($1) }
| STRING { Syntax.StringExp($1) }
| ID { Syntax.VarExp($1) }
| LPAREN RPAREN { Syntax.UnitExp }
| LPAREN exps RPAREN { Syntax.SeqExp($2) }
| exp PLUS exp { Syntax.OpExp { left = $1; op = Syntax.PlusOp; right = $3} }
| exp MINUS exp { Syntax.OpExp { left = $1; op = Syntax.MinusOp; right = $3} }
| exp TIMES exp { Syntax.OpExp { left = $1; op = Syntax.TimesOp; right = $3} }
| exp DIVIDE exp { Syntax.OpExp { left = $1; op = Syntax.DivideOp; right = $3} }
| exp EQ exp { Syntax.OpExp { left = $1; op = Syntax.EqOp; right = $3} }
| exp NEQ exp { Syntax.OpExp { left = $1; op = Syntax.NeqOp; right = $3} }
| exp LT exp { Syntax.OpExp { left = $1; op = Syntax.LtOp; right = $3} }
| exp LE exp { Syntax.OpExp { left = $1; op = Syntax.LeOp; right = $3} }
| exp GT exp { Syntax.OpExp { left = $1; op = Syntax.GtOp; right = $3} }
| exp GE exp { Syntax.OpExp { left = $1; op = Syntax.GeOp; right = $3} }
| MINUS exp { Syntax.OpExp { left = Syntax.IntExp(0); op = Syntax.MinusOp; right = $2} }
| LET decs IN exp END { Syntax.LetExp { decs = $2; body = $4 } }
| ID LPAREN args RPAREN { Syntax.CallExp { id = $1; args = $3 } }
| IF exp THEN exp { Syntax.IfExp { test = $2; then' = $4; else' = None } }
| IF exp THEN exp ELSE exp { Syntax.IfExp { test = $2; then' = $4; else' = Some ($6) } }

exps:
  exp { [$1] } // 空の場合
| exps SEMICOLON exp { $1 @ [$3] }

args:
  { [] } // 空の場合
| exp { [$1] }
| args COMMA exp { $1 @ [$3] }

tyfield:
  ID COLON ID { Syntax.Field($1, $3) }

tyfields:
  { [] } // 空の場合
| tyfield { [$1] }
| tyfields COMMA tyfield { $1 @ [$3] }

dec:
  VAR ID ASSIGN exp { Syntax.VarDec($2, $4) }
| FUNCTION ID LPAREN tyfields RPAREN EQ exp { Syntax.FunctionDec($2, $4, $7) }

decs:
  { [] } // 空の場合
| decs dec { $1 @ [$2] }
