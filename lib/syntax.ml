type op_t = Plus | Minus | Times | Divide

type t =
    IntExp of int (* 整数 *)
  | StringExp of string (* 文字列 *)
  | OpExp of t * op_t * t (* 二項演算子 *)
