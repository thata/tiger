type symbol = string

and op_t =
  | PlusOp
  | MinusOp
  | TimesOp
  | DivideOp
  | EqOp
  | NeqOp
  | LtOp
  | LeOp
  | GtOp
  | GeOp

  and t =
  | IntExp of int (* 整数 *)
  | StringExp of string (* 文字列 *)
  | LetExp of dec_t list * t
  | VarExp of string
  | CallExp of string * t list (* 関数呼び出し *)
  | OpExp of t * op_t * t (* 二項演算子 *)

and dec_t =
  | VarDec of symbol * t (* 変数宣言 *)
  | FunctionDec of symbol * t (* 関数宣言 *)


