open Ast

module Hashtbl_p = struct
  type ('k, 's) t = ('k, 's) Hashtbl.t

  let pp pp_key pp_value ppf values =
    Format.fprintf ppf "[["
    |> fun () ->
    Hashtbl.iter
      (fun key data ->
        Format.fprintf ppf "@[<1>%a@ ->@ %a@]@\n@." pp_key key pp_value data )
      values
    |> fun () -> Format.fprintf ppf "]]@\n"
end

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( >> ) : 'a t -> 'b t -> 'b t
end

module type MONADERROR = sig
  include MONAD

  val error : string -> 'a t
end

module Result = struct
  type 'a t = ('a, string) Result.t

  let ( >>= ) = Result.bind

  let return = Result.ok

  let error = Result.error

  let ( >> ) x f = x >>= fun _ -> f
end

module Eval (M : MONADERROR) = struct
  open M

  type variables = (ident, expression) Hashtbl_p.t
  [@@deriving show {with_path= false}]

  type jump_statement = Default | Return | Break
  [@@deriving show {with_path= false}]

  type environment =
    { vars: variables
    ; last_value: expression
    ; is_loop: bool
    ; jump_stmt: jump_statement }
  [@@deriving show {with_path= false}]

  type env_lst = environment list
  
  let arg_to_num arg1 arg2 msg =
    let is_num x =
      try
        float_of_string x |> ignore ;
        true
      with Failure _ -> false
    in
    match (arg1, arg2) with
    | Exp_number arg1, Exp_number arg2 ->
        return (arg1, arg2)
    | Exp_number arg1, Exp_string arg2 when is_num arg2 ->
        return (arg1, float_of_string arg2)
    | Exp_string arg1, Exp_number arg2 when is_num arg1 ->
        return (float_of_string arg1, arg2)
    | _ ->
        error msg

  let compare lhs rhs op =
    let get_op = function
      | Op_lt ->
          return ( < )
      | Op_le ->
          return ( <= )
      | Op_eq ->
          return ( = )
      | _ ->
          return ( <> )
    in
    match op with
    | Op_lt | Op_le -> (
      match (lhs, rhs) with
      | Exp_number l, Exp_number r ->
          get_op op
          >>= fun op -> if op l r then return Exp_true else return Exp_false
      | Exp_string l, Exp_string r ->
          get_op op
          >>= fun op -> if op l r then return Exp_true else return Exp_false
      | _, _ ->
          error "Can't compare not the same types" )
    | Op_eq | Op_neq -> (
      match (lhs, rhs) with
      | Exp_number l, Exp_number r ->
          get_op op
          >>= fun op -> if op l r then return Exp_true else return Exp_false
      | Exp_string l, Exp_string r ->
          get_op op
          >>= fun op -> if op l r then return Exp_true else return Exp_false
      | _ ->
          return Exp_true )
    | _ ->
        return Exp_true

  let is_true = function Exp_true -> true | _ -> false

  let is_global = function Nonlocal -> true | _ -> false

  let rec find_var varname = function
    | [] ->
        return Exp_nil
    | hd_env :: tl -> (
      match Hashtbl.find_opt hd_env.vars varname with
      | Some v ->
          return v
      | None ->
          find_var varname tl )

  let rec eval_expr env_lst = function
    | Exp_number v ->
        return @@ Exp_number v
    | Exp_false ->
        return Exp_false
    | Exp_true ->
        return Exp_true
    | Exp_nil ->
        return Exp_nil
    | Exp_string s ->
        return (Exp_string s)
    | Exp_op (op, lhs, rhs) -> (
        eval_expr env_lst lhs
        >>= fun l ->
        eval_expr env_lst rhs
        >>= fun r ->
        match op with
        | Op_le | Op_lt | Op_eq ->
            compare l r op
        | Op_add | Op_mul | Op_sub | Op_div -> (
            let get_op = function
              | Op_add ->
                  (( +. ), "Error: Unsupported operands type for (+)")
              | Op_sub ->
                  (( -. ), "Error: Unsupported operands type for (-)")
              | Op_mul ->
                  (( *. ), "Error: Unsupported operands type for (*)")
              | Op_div ->
                  (( /. ), "Error: Unsupported operands type for (/)")
              | Op_mod ->
                let ( %% ) x y =
                  let int_part = x /. y in
                  x -. (int_part *. y) in 
                  (( %% ), "Error: Unsupported operands type for (%)")
              | _ ->
                  (( +. ), "Unreachable")
            in
            match get_op op with
            | op, err_msg ->
                arg_to_num l r err_msg
                >>= fun (x, y) -> return (Exp_number (op x y)) )
        | _ ->
            return Exp_nil )
    | Exp_lhs l ->
        find_var l env_lst
    | Exp_function (ids, bl) ->
        return (Exp_function (ids, bl))
    | _ ->
        error "Error: Unexpected expression"

  and set_hd_last_value last_value = function
    | [] ->
        error
          "Error: Can't modify environment head with <last_value>. Head is \
           absent!"
    | hd :: tl ->
        return @@ ({hd with last_value} :: tl)

  and modify_hd_vars value = function
    | [] ->
        error "Error: Can't modify environment head variables. Head is absent!"
    | hd :: _ -> (
      match value with name, vle -> return @@ Hashtbl.replace hd.vars name vle )

  and get_cur_env = function
    | [] ->
        error "Error: Current environment is absent!"
    | hd :: _ ->
        return hd

  and eval_stmt env_lst = function
    | Stat_assign (loc, lhs, exp) ->
        eval_vardec loc env_lst (lhs, exp)
    | Stat_return _ ->
        error "Error: Unexpected return statement"
    | Stat_break ->
        error "Error: Unexpected break statement"
    | Stat_while (e, bal) ->
        set_hd_is_loop env_lst >>= fun env_lst -> eval_while e bal env_lst
    | Stat_if (body, tail) ->
        let rec evalif env_lst tail = function
          | [] -> (
            match tail with
            | None ->
                return []
            | Some block ->
                eval_block env_lst block )
          | [body] -> (
            match body with
            | pred, st ->
                eval_expr env_lst pred
                >>= fun res ->
                if is_true res then eval_block env_lst st else return [] )
          | bd :: tl -> (
            match bd with
            | pred, st ->
                eval_expr env_lst pred
                >>= fun res ->
                if is_true res then eval_block env_lst st
                else evalif env_lst tail tl )
        in
        evalif env_lst tail body
    | _ ->
        error "sad:("

  and eval_while e bal env_lst =
    eval_expr env_lst e
    >>= fun predicate ->
    if is_true predicate then
      eval_block env_lst bal
      >>= fun env_lst ->
      get_cur_env env_lst
      >>= fun cur_env ->
      match cur_env.jump_stmt with
      | Break ->
          set_hd_jump_stmt Default env_lst
      | Return ->
          set_hd_jump_stmt Return env_lst
      | _ ->
          eval_while e bal env_lst
    else set_parents_is_loop env_lst

  and set_parents_is_loop env_lst =
    let prev_env = get_prev_env env_lst in
    let prev_is_loop =
      match prev_env with [] -> false | _ -> (List.hd prev_env).is_loop
    in
    set_hd_is_loop ~is_loop:prev_is_loop env_lst

  and set_hd_is_loop ?(is_loop = true) = function
    | [] ->
        error
          "Error: Can't modify environment head with <is_loop>. Head is absent!"
    | hd :: tl ->
        return @@ ({hd with is_loop} :: tl)

  and eval_vardec local_flag env_lst (lhs, exp) =
    eval_expr env_lst exp >>= fun v -> assign lhs v local_flag env_lst

  and eval_return env_lst e =
    eval_expr env_lst e
    >>= fun v ->
    let prev_env = get_prev_env env_lst in
    set_hd_last_value v prev_env
    >>= fun prev_env -> set_hd_jump_stmt Return prev_env

  and set_hd_jump_stmt jump_stmt = function
    | [] ->
        error
          "Error: Can't modify environment head with <jump_stmt>. Head is \
           absent!"
    | hd :: tl ->
        return @@ ({hd with jump_stmt} :: tl)

  and get_prev_env = function [] -> [] | _ :: tl -> tl

  and create_next_env = function
    | [] ->
        [ { vars= Hashtbl.create 16
          ; last_value= Exp_nil
          ; is_loop= false
          ; jump_stmt= Default } ]
    | hd_env :: tl ->
        {hd_env with vars= Hashtbl.create 16} :: hd_env :: tl

  and assign n v loc env_lst =
    let rec set_global n v = function
      | [] ->
          ()
      | [hd] ->
          Hashtbl.replace hd.vars n v
      | hd :: tl -> (
        match Hashtbl.find_opt hd.vars n with
        | None ->
            set_global n v tl
        | Some _ ->
            Hashtbl.replace hd.vars n v )
    in
    if is_global loc then (set_global n v env_lst ; return env_lst)
    else modify_hd_vars (n, v) env_lst >>= fun _ -> return env_lst

  and eval_block env_lst = function
    | [] ->
        return @@ get_prev_env env_lst
    | [tl] -> (
        get_cur_env env_lst
        >>= fun cur_env ->
        match tl with
        | Stat_break when cur_env.is_loop ->
            eval_break env_lst
        | Stat_break ->
            error "Error: Unexpected break statement"
        | Stat_return _ ->
            error "Error: Unexpected return statement"
        | _ -> (
            eval_stmt env_lst tl
            >>= fun env_lst ->
            get_cur_env env_lst
            >>= fun cur_env ->
            match cur_env.jump_stmt with
            | Return ->
                let prev_env = get_prev_env env_lst in
                set_hd_last_value cur_env.last_value prev_env
                >>= fun prev_env -> set_hd_jump_stmt Return prev_env
            | Break ->
                eval_break env_lst
            | _ -> (
                let prev_env = get_prev_env env_lst in
                match prev_env with
                | [] ->
                    return env_lst
                | _ ->
                    return prev_env ) ) )
    | hd :: tl -> (
        eval_stmt env_lst hd
        >>= fun env_lst ->
        get_cur_env env_lst
        >>= fun cur_env ->
        match cur_env.jump_stmt with
        | Return ->
            let prev_env = get_prev_env env_lst in
            set_hd_last_value cur_env.last_value prev_env
            >>= fun prev_env -> set_hd_jump_stmt Return prev_env
        | Break ->
            eval_break env_lst
        | _ ->
            eval_block env_lst tl )

  and eval_break env_lst =
    let prev_env = get_prev_env env_lst in
    set_hd_jump_stmt Break prev_env >>= fun env -> set_parents_is_loop env

  and eval_prog block = eval_block (create_next_env []) block
end

open Eval (Result)

let eval parsed_prog =
  match eval_prog parsed_prog with
  | Ok res -> (
    match res with
    | hd :: _ ->
        print_endline @@ show_environment hd
    | [] ->
        print_endline "[]" )
  | Error msg ->
      print_endline msg

let%expect_test "fact_interpreter" =
  eval
  @@ Parser.parse_exn
       {| k = 10 
       i = 1 
       n = 1 
       while i <= k do 
        n = n * i 
        i = i + 1 
      end |} ;
  [%expect
    {|
    { vars = [["n" -> (Exp_number 3628800.)

    "k" -> (Exp_number 10.)

    "i" -> (Exp_number 11.)

    ]]
    ; last_value = Exp_nil; is_loop = false; jump_stmt = Default
    } |}]

let%expect_test "func_decl" =
  eval
    (Parser.parse_exn
       {| function fact(n)
  if n == 0 then return 1 else return n * fact(n - 1) end
end |} ) ;
  [%expect
    {|
    { vars =
      [["fact" ->
         (Exp_function (["n"],
            [(Stat_if (
                [((Exp_op (Op_eq, (Exp_lhs "n"), (Exp_number 0.))),
                  [(Stat_return [(Exp_number 1.)])])],
                (Some [(Stat_return
                          [(Exp_op (Op_mul, (Exp_lhs "n"),
                              (Exp_call
                                 (Call ((Exp_lhs "fact"),
                                    [(Exp_op (Op_sub, (Exp_lhs "n"),
                                        (Exp_number 1.)))
                                      ]
                                    )))
                              ))
                            ])
                        ])
                ))
              ]
            ))

    ]]
    ; last_value = Exp_nil; is_loop = false; jump_stmt = Default
    } |}]

let%expect_test "woeworwo" =
  eval (Parser.parse_exn "x = 15 while x > 10 do x = x - 4 end") ;
  [%expect
    {|
    { vars = [["x" -> (Exp_number 7.)

    ]]
    ; last_value = Exp_nil; is_loop = false; jump_stmt = Default
    } |}]

let%expect_test "how" =
  eval
    [ Stat_assign (Nonlocal, "x", Exp_number 100.)
    ; Stat_if
        ( [ ( Exp_op (Op_eq, Exp_lhs "x", Exp_number 100.)
            , [Stat_assign (Nonlocal, "x", Exp_number 10.)] ) ]
        , None ) ] ;
  [%expect
    {|
    { vars = [["x" -> (Exp_number 10.)

    ]]
    ; last_value = Exp_nil; is_loop = false; jump_stmt = Default
    } |}]

let%expect_test "help" =
  eval
    [ Stat_assign (Nonlocal, "x", Exp_number 15.)
    ; Stat_while
        ( Exp_op (Op_lt, Exp_number 10., Exp_lhs "x")
        , [ Stat_assign
              (Nonlocal, "x", Exp_op (Op_sub, Exp_lhs "x", Exp_number 4.)) ] )
    ] ;
  [%expect
    {|
      { vars = [["x" -> (Exp_number 7.)

      ]]
      ; last_value = Exp_nil; is_loop = false; jump_stmt = Default
      } |}]

let%expect_test "abc" =
  eval [Stat_assign (Nonlocal, "x", Exp_number 42.)] ;
  [%expect
    {|
    { vars = [["x" -> (Exp_number 42.)

    ]]
    ; last_value = Exp_nil; is_loop = false; jump_stmt = Default
    } |}]

let%expect_test "abc" =
  eval [Stat_assign (Nonlocal, "x", Exp_string "whennnnnn")] ;
  [%expect
    {|
    { vars = [["x" -> (Exp_string "whennnnnn")

    ]]
    ; last_value = Exp_nil; is_loop = false; jump_stmt = Default
    } |}]

let%expect_test "abc" =
  eval [Stat_while (Exp_false, [Stat_assign (Nonlocal, "x", Exp_number 1.)])] ;
  [%expect
    {|
    { vars = [[]]
      ; last_value = Exp_nil; is_loop = false; jump_stmt = Default } |}]
