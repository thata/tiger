%{
%}

// トークンの定義
%token EOF
%token <string> ID
%token <int> INT
%token <string> STRING
%token COMMA COLON SEMICOLON LPAREN RPAREN LBRACK RBRACK
%token LBRACE RBRACE DOT
%token PLUS MINUS TIMES DIVIDE EQ NEQ LT LE GT GE
%token AND OR ASSIGN
%token ARRAY IF THEN ELSE WHILE FOR TO DO LET IN END OF
%token BREAK NIL
%token FUNCTION VAR TYPE
%token EOF

// エントリーボイントの定義
%start program

%type <Syntax.t> program

// 文法規則の定義
%%
program: exp EOF { $1 }

exp: INT { Syntax.IntExp($1) }
