type val_t = IntVal of int | StringVal of string

let rec f expr =
  match expr with
    Syntax.IntExp n -> IntVal(n)
  | Syntax.OpExp(e1, op, e2) ->
      let v1 = match (f e1) with
        IntVal(n) -> n
      | _ -> failwith "integer value expected" in
      let v2 = match (f e2) with
        IntVal(n) -> n
      | _ -> failwith "integer value expected" in
      (match op with
        Syntax.Plus -> IntVal(v1 + v2)
      | Syntax.Minus -> IntVal(v1 - v2)
      | Syntax.Times -> IntVal(v1 * v2)
      | Syntax.Divide -> IntVal(v1 / v2))

let string_of_val v = match v with
    IntVal(n) -> string_of_int n
  | StringVal(s) -> s

let print_val v = print_string (string_of_val v)
