%{
%}

// トークンの定義
%token EOF
%token <int> INT
%token <string> STRING
%token <string> ID
%token PLUS MINUS TIMES DIVIDE
%token EQ NEQ LT LE GT GE
%token LET IN END
%token VAR FUNCTION ASSIGN
%token LPAREN RPAREN COMMA COLON
%token EOF

// あとで使う
// %token COMMA COLON SEMICOLON LPAREN RPAREN LBRACK RBRACK
// %token LBRACE RBRACE DOT
// %token AND OR
// %token ARRAY IF THEN ELSE WHILE FOR TO DO OF
// %token BREAK NIL
// %token FUNCTION TYPE

// エントリーボイントの定義
%start program

%type <Syntax.t> program

%left EQ NEQ LT LE GT GE
%left PLUS MINUS
%left TIMES DIVIDE

// 文法規則の定義
%%
program: exp EOF { $1 }

exp:
  INT { Syntax.IntExp($1) }
| STRING { Syntax.StringExp($1) }
| ID { Syntax.VarExp($1) }
| exp PLUS exp { Syntax.OpExp($1, Syntax.PlusOp, $3) }
| exp MINUS exp { Syntax.OpExp($1, Syntax.MinusOp, $3) }
| exp TIMES exp { Syntax.OpExp($1, Syntax.TimesOp, $3) }
| exp DIVIDE exp { Syntax.OpExp($1, Syntax.DivideOp, $3) }
| exp EQ exp { Syntax.OpExp($1, Syntax.EqOp, $3) }
| exp NEQ exp { Syntax.OpExp($1, Syntax.NeqOp, $3) }
| exp LT exp { Syntax.OpExp($1, Syntax.LtOp, $3) }
| exp LE exp { Syntax.OpExp($1, Syntax.LeOp, $3) }
| exp GT exp { Syntax.OpExp($1, Syntax.GtOp, $3) }
| exp GE exp { Syntax.OpExp($1, Syntax.GeOp, $3) }
| LET decs IN exp END { Syntax.LetExp($2, $4) }
| ID LPAREN args RPAREN { Syntax.CallExp($1, $3) }

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
