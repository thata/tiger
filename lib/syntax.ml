type symbol = string

type t =
  | IntExp of int (* 整数 *)
  | StringExp of string (* 文字列 *)
  | LetExp of dec_t * t
  | IdExp of string
  | OpExp of t * op_t * t (* 二項演算子 *)

and dec_t =
  | VarDec of symbol * t (* 変数宣言 *)

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

