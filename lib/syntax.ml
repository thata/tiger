type op_t =
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

type t =
  | IntExp of int (* 整数 *)
  | StringExp of string (* 文字列 *)
  | IdExp of string
  | OpExp of t * op_t * t (* 二項演算子 *)
