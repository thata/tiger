type val_t = IntVal of int | StringVal of string
type id = string
type table = (id * val_t) list

let rec f expr env: val_t * table =
  match expr with
  | Syntax.IntExp n -> (IntVal n, env)
  | Syntax.StringExp s -> (StringVal s, env)
  | Syntax.VarExp s -> (List.assoc s env, env)
  | Syntax.LetExp (decs, body) ->
      let new_env = List.fold_left
                      (fun env dec -> eval_dec dec env)
                      env
                      decs
      in f body new_env
  | Syntax.OpExp (e1, op, e2) ->
      match op with
      | Syntax.PlusOp | Syntax.MinusOp | Syntax.TimesOp | Syntax.DivideOp ->
          calc op e1 e2 env
      | Syntax.EqOp | Syntax.NeqOp | Syntax.LtOp | Syntax.GtOp | Syntax.LeOp | Syntax.GeOp ->
          compare op e1 e2 env
and calc op e1 e2 env =
  let v1, env = f e1 env in
  let v2, env = f e2 env in
  match op, v1, v2 with
  | Syntax.PlusOp, IntVal v1, IntVal v2 -> IntVal(v1 + v2), env
  | Syntax.MinusOp, IntVal v1, IntVal v2 -> IntVal(v1 - v2), env
  | Syntax.TimesOp, IntVal v1, IntVal v2 -> IntVal(v1 * v2), env
  | Syntax.DivideOp, IntVal v1, IntVal v2 -> IntVal(v1 / v2), env
  | _ -> failwith "type error"
and compare op e1 e2 env =
  let v1, env = f e1 env in
  let v2, env = f e2 env in
  match op, v1, v2 with
  | Syntax.EqOp, IntVal v1, IntVal v2 -> if v1 = v2 then IntVal(1), env else IntVal(0), env
  | Syntax.NeqOp, IntVal v1, IntVal v2 -> if v1 <> v2 then IntVal(1), env else IntVal(0), env
  | Syntax.LtOp, IntVal v1, IntVal v2 -> if v1 < v2 then IntVal(1), env else IntVal(0), env
  | Syntax.GtOp, IntVal v1, IntVal v2 -> if v1 > v2 then IntVal(1), env else IntVal(0), env
  | Syntax.LeOp, IntVal v1, IntVal v2 -> if v1 <= v2 then IntVal(1), env else IntVal(0), env
  | Syntax.GeOp, IntVal v1, IntVal v2 -> if v1 >= v2 then IntVal(1), env else IntVal(0), env
  | Syntax.EqOp, StringVal s1, StringVal s2 -> if s1 = s2 then IntVal(1), env else IntVal(0), env
  | Syntax.NeqOp, StringVal s1, StringVal s2 -> if s1 <> s2 then IntVal(1), env else IntVal(0), env
  | _ -> failwith "type error"
and eval_dec dec env =
    match dec with
    | Syntax.VarDec (id, e) ->
        let (v, _) = f e env in
        (id, v) :: env
and string_of_val v =
  match v with IntVal n -> string_of_int n | StringVal s -> s
and print_val v = print_string (string_of_val v)
