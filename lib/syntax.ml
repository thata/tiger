type symbol = string

and op_t =
  | PlusOp
  | MinusOp
  | TimesOp
  | DivideOp
  | AndOp
  | OrOp
  | EqOp
  | NeqOp
  | LtOp
  | LeOp
  | GtOp
  | GeOp

  and t =
  | IntExp of int (* 整数 *)
  | StringExp of string (* 文字列 *)
  | UnitExp (* ユニット *)
  | LetExp of { decs: dec_t list; body: t }
  | VarExp of string
  | CallExp of { id: string; args: t list } (* 関数呼び出し *)
  | OpExp of { left: t; op: op_t; right: t } (* 二項演算子 *)
  | IfExp of { test: t; then': t; else': t option } (* if文 *)
  | SeqExp of t list (* 式の逐次実行 *)

and dec_t =
  | VarDec of symbol * t (* 変数宣言 *)
  | FunctionDec of symbol * field_t list * t (* 関数宣言 *)

(* 関数の引数 *)
and field_t = Field of symbol * symbol
