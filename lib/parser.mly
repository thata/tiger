%{
%}

// トークンの定義
%token EOF
%token <int> INT
%token <string> STRING
%token PLUS MINUS TIMES DIVIDE
%token EQ NEQ LT LE GT GE
%token EOF

// あとで使う
// %token <string> ID
// %token COMMA COLON SEMICOLON LPAREN RPAREN LBRACK RBRACK
// %token LBRACE RBRACE DOT
// %token AND OR ASSIGN
// %token ARRAY IF THEN ELSE WHILE FOR TO DO LET IN END OF
// %token BREAK NIL
// %token FUNCTION VAR TYPE

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
