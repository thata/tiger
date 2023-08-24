%{
%}

// トークンの定義
%token EOF
%token <int> INT
%token <string> STRING
%token PLUS MINUS TIMES DIVIDE
%token EOF

// エントリーボイントの定義
%start program

%type <Syntax.t> program

%left PLUS MINUS
%left TIMES DIVIDE

// 文法規則の定義
%%
program: exp EOF { $1 }

exp:
  INT { Syntax.IntExp($1) }
| STRING { Syntax.StringExp($1) }
| exp PLUS exp { Syntax.OpExp($1, Syntax.Plus, $3) }
| exp MINUS exp { Syntax.OpExp($1, Syntax.Minus, $3) }
| exp TIMES exp { Syntax.OpExp($1, Syntax.Times, $3) }
| exp DIVIDE exp { Syntax.OpExp($1, Syntax.Divide, $3) }
