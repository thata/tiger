type val_t = IntVal of int | StringVal of string

let compare op v1 v2 =
  match (v1, v2) with
  | IntVal n1, IntVal n2 -> (
      match op with
      | Syntax.EqOp -> IntVal (if n1 = n2 then 1 else 0)
      | Syntax.NeqOp -> IntVal (if n1 <> n2 then 1 else 0)
      | Syntax.LtOp -> IntVal (if n1 < n2 then 1 else 0)
      | Syntax.GtOp -> IntVal (if n1 > n2 then 1 else 0)
      | Syntax.LeOp -> IntVal (if n1 <= n2 then 1 else 0)
      | Syntax.GeOp -> IntVal (if n1 >= n2 then 1 else 0)
      | _ -> failwith "invalid value")
  | StringVal s1, StringVal s2 -> (
      match op with
      | Syntax.EqOp -> IntVal (if s1 = s2 then 1 else 0)
      | Syntax.NeqOp -> IntVal (if s1 <> s2 then 1 else 0)
      | Syntax.LtOp -> IntVal (if s1 < s2 then 1 else 0)
      | Syntax.GtOp -> IntVal (if s1 > s2 then 1 else 0)
      | Syntax.LeOp -> IntVal (if s1 <= s2 then 1 else 0)
      | Syntax.GeOp -> IntVal (if s1 >= s2 then 1 else 0)
      | _ -> failwith "invalid value")
  | _ -> failwith "invalid value"

let rec f expr =
  match expr with
  | Syntax.IntExp n -> IntVal n
  | Syntax.StringExp s -> StringVal s
  | Syntax.OpExp (e1, op, e2) -> (
      let getInt intVal =
        match intVal with
        | IntVal n -> n
        | _ -> failwith "integer value expected"
      in
      match op with
      | Syntax.PlusOp -> IntVal (getInt (f e1) + getInt (f e2))
      | Syntax.MinusOp -> IntVal (getInt (f e1) - getInt (f e2))
      | Syntax.TimesOp -> IntVal (getInt (f e1) * getInt (f e2))
      | Syntax.DivideOp -> IntVal (getInt (f e1) / getInt (f e2))
      | Syntax.EqOp | Syntax.NeqOp | Syntax.LtOp | Syntax.GtOp | Syntax.LeOp
      | Syntax.GeOp ->
          compare op (f e1) (f e2))

let string_of_val v =
  match v with IntVal n -> string_of_int n | StringVal s -> s

let print_val v = print_string (string_of_val v)
