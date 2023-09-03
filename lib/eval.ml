type id = string
and table = (id * val_t) list
and val_t =
    IntVal of int
  | StringVal of string
  | UnitVal
  | FunctionDec of id list * Syntax.t
  | BuiltInFunction of (val_t list -> table -> val_t)

let rec f expr env: val_t * table =
  match expr with
  | Syntax.IntExp n -> (IntVal n, env)
  | Syntax.StringExp s -> (StringVal s, env)
  | Syntax.UnitExp -> (UnitVal, env)
  | Syntax.VarExp s -> (List.assoc s env, env)
  | Syntax.LetExp { decs; body } ->
      let new_env = List.fold_left
                      (fun env dec -> eval_dec dec env)
                      env
                      decs
      in f body new_env
  | Syntax.IfExp { test; then'; else' } ->
      let v, env = f test env in
      (match v with
        | IntVal 1 -> f then' env
        | IntVal 0 ->
            (match else' with
              | Some else' -> f else' env
              | None -> (UnitVal, env))
        | _ -> failwith "type error")
  | Syntax.SeqExp exprs ->
    (match exprs with
    | [] -> (UnitVal, env)
    | expr::[] -> f expr env
    | expr::exprs ->
        let _, env = f expr env in
        f (Syntax.SeqExp exprs) env)
  | Syntax.CallExp { id; args } ->
      (let func = List.assoc id env in
      match func with
        | FunctionDec (field_names, body) ->
            let rec get_args (args:Syntax.t list) (env:table) (field_names:id list) =
              match args, field_names with
              | [], [] -> env
              | arg::args, field_name::field_names ->
                  let v, env = f arg env in
                  get_args args ((field_name, v)::env) field_names
              | _ -> failwith "type error"
            in
            let new_env = get_args args env field_names in
            let result, _ = f body new_env
            in result, env
        | BuiltInFunction builtin_func ->
            (let rec eval_args args env =
              match args with
              | [] -> ([], env)
              | arg::args ->
                  let v, env = f arg env in
                  let vs, env = eval_args args env in
                  (v::vs, env)
            in
            let args, env = eval_args args env in
            (builtin_func args env, env))
        | _ -> failwith "type error")
    | Syntax.OpExp { left; op; right } ->
      match op with
      | Syntax.PlusOp | Syntax.MinusOp | Syntax.TimesOp | Syntax.DivideOp ->
          calc op left right env
      | Syntax.EqOp | Syntax.NeqOp | Syntax.LtOp | Syntax.GtOp | Syntax.LeOp | Syntax.GeOp ->
          compare op left right env

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
    | Syntax.FunctionDec (id, fields, body) ->
        let rec get_field_names (fields:Syntax.field_t list) =
          match fields with
          | [] -> []
          | Field (id, _) :: rest -> id :: get_field_names rest
        in
        let field_names = get_field_names fields in
        let func = FunctionDec (field_names, body) in
        (id, func) :: env

and string_of_val v =
  match v with
    IntVal n -> string_of_int n
  | StringVal s -> s
  | UnitVal -> "()"
  | FunctionDec _ -> "<fun>"
  | BuiltInFunction _ -> "<builtin>"

and print_val v = print_string (string_of_val v)
