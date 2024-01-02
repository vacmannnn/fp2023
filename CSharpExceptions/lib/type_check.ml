(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
(* open Base *)

(*
   Идея:
   (runtime_ctx, error) Result.t <-| это штука, которая является контекстом
   * runtime_ctx.code -- это хэшмапа, где колючи - имена классов / исключения + вся информация о них
   * runtim_ctx.local_env -- сюда добавляются всякие вычисления (создается новая при заходе в функцию + параметры)
   * runtime_ctx.self -- здесь хранится состояние того класса внутри которого сейчас исполняется код

   (instance) <-| описывает экземпляр класса т.е. его состояние (набор полей)
   * Copy -- эмулирует передачуу по ссылке, означает, что определение надо искать в локальном окружении
   * State -- это оригинальный надор полей в локальном окруженииy

   (после этапа )
*)

(*
   TODO:

   1) все конструкторы добавляются в локальное окружение (в любом локальном окружении есть конструкторы)

   2) добавить в глобальное/локальное окружение дополнительные штуки (типо base_lib)
   * Exception - базовое исключение, которое можно создать
   * метод print (local)
   * метод open_file + close_file + write_file(string)

   3) сделать проверку внутри класса на поля конструктора (крч, если у нас поля есть, они неопределены, и нет конструктора, то фэйл)
   * все конструкторы добавляются в класс автоматически
*)

module Ident = struct
  type t = ident

  let compare = compare
end

(* TODO: переписать красиво *)
type code_ident = Code_ident of ident
type env_ident = Env_ident of ident

module Env_id = struct
  type t = env_ident

  let compare = compare
end

module Code_id = struct
  type t = code_ident

  let compare = compare
end

module IdentMap = Stdlib.Map.Make (Ident)
module CodeMap = Stdlib.Map.Make (Code_id)
module EnvMap = Stdlib.Map.Make (Env_id)

type excaption = Exception of ident

type code_ctx =
  | Exception_ctx of class_decl
    (* TODO: переделывать классы с наследованием в этот тип, так как нормального наследования у меня нет *)
  | Class_ctx of class_decl (* Сюда просто перепихнуть инфу *)

type instance =
  (* TODO: Очень осторожно работать с этим... *)
  | Copy of env_ident (* ident - имя instance в локальном окружении *)
  | State of state
(* Генерируется при создании экземпляра *)

and state = code_ident * env_value EnvMap.t

and env_value =
  | VInstance of instance
  | Value of value_

type env = env_value EnvMap.t
type text = code_ctx CodeMap.t

type runtime_ctx =
  { code : text
  ; local_env : env
  ; self : state
  }

type t_env_value =
  | Metod_sig of method_sign
  | Constructor_sig of constructor_sign
  | Value_sig of var_type
  | Fild_sig of fild_sign

type t_loc_env = t_env_value IdentMap.t
type type_check_ctx = text * t_loc_env

(* type type_ =
   | Mtype_ of meth_type
   | Vtype_ of var_type *)

type error =
  [ `Not_find_ident of ident
  | `Type_mismatch
  | `Double_definition
  | `Other_error of string
  | `Method_not_find
  | `User_exception of excaption
  | `Access_error of string
  ]

module Type_checker = struct
  type ctx_env = type_check_ctx
  type ('st, 'a) t = 'st -> 'st * ('a, error) Result.t
  type 'a tt = (ctx_env, 'a) t

  let return : 'a -> 'a tt = fun x st -> st, Result.ok x
  let fail : error -> 'a tt = fun er st -> st, Result.error er

  let ( >>= ) : 'a tt -> ('a -> 'b tt) -> 'b tt =
    fun x f st ->
    let st1, x1 = x st in
    match x1 with
    | Result.Ok x -> f x st1
    | Result.Error s -> st1, Result.error s
  ;;

  let save_local : t_loc_env -> unit tt =
    fun l_env c_env ->
    match c_env with
    | code, _ -> (code, l_env), Result.ok ()
  ;;

  let save_local_el : ident -> t_env_value -> unit tt =
    fun id t_val c_env ->
    match c_env with
    | _, local_env ->
      let new_l = IdentMap.add id t_val local_env in
      save_local new_l c_env
  ;;

  let save_global : text -> unit tt =
    fun g_env c_env ->
    match c_env with
    | _, local_env -> (g_env, local_env), Result.ok ()
  ;;

  let save_global_el : code_ident -> code_ctx -> unit tt =
    fun id ctx c_env ->
    match c_env with
    | code, _ ->
      let new_l = CodeMap.add id ctx code in
      save_global new_l c_env
  ;;

  let read_local : ident -> t_env_value tt =
    fun id c_env ->
    match c_env with
    | _, local_env ->
      (match IdentMap.find_opt id local_env with
       | Some x -> return x c_env
       | None -> fail (`Not_find_ident id) c_env)
  ;;

  let read_global : code_ident -> code_ctx tt =
    fun id c_env ->
    let id_ =
      match id with
      | Code_ident x -> x
    in
    match c_env with
    | code, _ ->
      (match CodeMap.find_opt id code with
       | Some x -> return x c_env
       | None -> fail (`Not_find_ident id_) c_env)
  ;;

  let run : 'a tt -> ctx_env * ('a, error) Result.t =
    fun f -> f (CodeMap.empty, IdentMap.empty)
  ;;

  let continue : 'a tt -> ctx_env -> ctx_env * ('a, error) Result.t =
    fun f c_env -> f c_env
  ;;

  let lift2 : ('a -> 'b -> 'c) -> 'a tt -> 'b tt -> 'c tt =
    fun f a b st -> (a >>= fun a1 -> b >>= fun b1 -> return (f a1 b1)) st
  ;;

  let ( *> ) : 'a tt -> 'b tt -> 'b tt = fun a b st -> (lift2 (fun _ x -> x) a b) st
  let ( <* ) : 'a tt -> 'b tt -> 'a tt = fun a b st -> (lift2 (fun x _ -> x) a b) st

  let ( <|> ) : 'a tt -> 'a tt -> 'a tt =
    fun a1 a2 st ->
    let st, x = a1 st in
    match x with
    | Result.Ok x -> return x st
    | Result.Error _ -> a2 st
  ;;

  let choice : 'a tt list -> 'a tt =
    fun l ->
    match l with
    | [] -> fail (`Other_error "Empty choice")
    | h :: tl -> List.fold_left ( <|> ) h tl
  ;;

  let ( >>| ) : 'a tt -> ('a -> 'b) -> 'b tt =
    fun x f -> x >>= fun x_res -> return (f x_res)
  ;;

  let map_left : ('a -> 'b tt) -> 'a list -> 'b list tt =
    fun custom_f mlst ->
    let f acc cur = acc >>= fun tl -> custom_f cur >>= fun x -> return (x :: tl) in
    List.fold_left f (return []) mlst >>| List.rev
  ;;

  let iter_left : ('a -> unit tt) -> 'a list -> unit tt =
    fun custom_f mlst ->
    let f acc cur = acc >>= fun _ -> custom_f cur >>= fun _ -> return () in
    List.fold_left f (return ()) mlst
  ;;
end

open Type_checker

let to_atype = function
  | Null -> None
  | VString _ -> Some (TNullable TString)
  | VInt _ -> Some (TNot_Nullable TInt)
  | VChar _ -> Some (TNot_Nullable TChar)
  | VBool _ -> Some (TNot_Nullable TBool)
;;

let to_var_type = function
  | Some x -> Some (TVar x)
  | None -> None
;;

let to_type_v = function
  | Some x -> Some (Value_sig x)
  | None -> None
;;

let to_type_m = function
  | Void -> Result.error "Not assignable type"
  | TReturn x -> Result.ok (Value_sig (TVar x))
;;

let to_assign_t = function
  | Value_sig (TVar a_tp) -> Result.ok a_tp
  | Metod_sig { m_modif = _; m_type; _ } ->
    (match m_type with
     | Void -> Result.error "Non-assignable type"
     | TReturn a_tp -> Result.ok a_tp)
  | Constructor_sig { con_modif = _; con_id; _ } -> Result.ok (TNullable (TClass con_id))
  | Fild_sig { f_modif = _; f_type = TVar tp; _ } -> Result.ok tp
;;

let opt_to_assign_t = function
  | None -> Result.ok None
  | Some a ->
    (match to_assign_t a with
     | Result.Ok a_tp -> Result.Ok (Some a_tp)
     | Result.Error x -> Result.error x)
;;

let compare a b f =
  let a_tp_res = opt_to_assign_t a in
  let b_tp_res = opt_to_assign_t b in
  match a_tp_res, b_tp_res with
  | Result.Error _, Result.Error _ | _, Result.Error _ | Result.Error _, _ ->
    Result.error "Non-comparable types"
  | Result.Ok a_tp_opt, Result.Ok b_tp_opt -> f (a_tp_opt, b_tp_opt)
;;

let compare_t_env_opt f a b =
  match f a b with
  | Result.Ok None -> return None
  | Result.Ok (Some x) -> return (Some (Value_sig (TVar x)))
  | Result.Error msg -> fail (`Other_error msg)
;;

let ( =!= ) a b =
  let helper (a_tp_opt, b_tp_opt) =
    match a_tp_opt, b_tp_opt with
    | Some a_tp, Some b_tp when equal_assignable_type a_tp b_tp -> Result.ok a_tp_opt
    | Some (TNullable _), None -> Result.ok a_tp_opt
    | None, Some (TNullable _) -> Result.ok b_tp_opt
    | None, None -> Result.ok None
    | _, _ -> Result.error "Types are not equal"
  in
  compare a b helper
;;

let eq_t_env_opt a b = compare_t_env_opt ( =!= ) a b

let ( =!> ) a b =
  match a =!= b with
  | Result.Ok a_tp -> Result.Ok a_tp
  | Result.Error _ ->
    let helper (a_tp_opt, b_tp_opt) =
      match a_tp_opt, b_tp_opt with
      | Some (TNullable (TBase a_tp)), Some (TNot_Nullable b_tp)
        when equal_base_type a_tp b_tp -> Result.ok a_tp_opt
      | _, _ -> Result.error "Types are not equal"
    in
    compare a b helper
;;

let greater_t_env_opt a b = compare_t_env_opt ( =!> ) a b
let params_check f prms = map_left f prms

let v_decl_to_type_v = function
  | Var_decl (tp, _) -> Value_sig tp
;;

let is_success_list lst =
  let f acc el =
    match acc, el with
    | true, Result.Ok _ -> true
    | _, _ -> false
  in
  List.fold_left f true lst
;;

let map2_opt f l1 l2 =
  let len1 = List.length l1 in
  let len2 = List.length l2 in
  match Int.equal len1 len2 with
  | true -> Result.ok (List.map2 f l1 l2)
  | false -> Result.error "len(list1) != len(list2)"
;;

let check_method tp args params =
  let compare_prms = map2_opt ( =!> ) in
  let to_env elem = Some (v_decl_to_type_v elem) in
  let args_env = List.map to_env args in
  let result =
    params
    >>| fun prms_env ->
    match compare_prms args_env prms_env with
    | Result.Ok ans -> is_success_list ans
    | Result.Error _ -> false
  in
  result
  >>= function
  | true -> return (Some tp)
  | false -> fail `Type_mismatch
;;

let check_invoke sign env_prms =
  match sign with
  | Some (Metod_sig { m_modif = _; m_type; m_id = _; m_args = Args args }) ->
    let tp = to_type_m m_type in
    (match tp with
     | Result.Error _ -> fail `Type_mismatch
     | Result.Ok tp -> check_method tp args env_prms)
  | Some (Constructor_sig { con_modif = _; con_id; con_args; base_params }) ->
    let tp = Value_sig (TVar (TNullable (TClass con_id))) in
    (match con_args, base_params with
     | Args args, None -> check_method tp args env_prms
     | _ -> fail (`Other_error "Inheritance with constructors is not supported"))
  | _ -> fail `Type_mismatch
;;

let get_class_decl = function
  | Exception_ctx exc -> exc
  | Class_ctx cl -> cl
;;

let get_sign id el =
  let is_fild s id =
    match s with
    | { f_modif = _; f_type = _; f_id } when equal_ident f_id id -> Some (Fild_sig s)
    | _ -> None
  in
  let is_method s id =
    match s with
    | { m_modif = _; m_type = _; m_id; _ } when equal_ident m_id id -> Some (Metod_sig s)
    | _ -> None
  in
  let is_constructor s id =
    match s with
    | { con_modif = _; con_id; _ } when equal_ident con_id id -> Some (Constructor_sig s)
    | _ -> None
  in
  match el with
  | Fild (sign, _) -> is_fild sign id
  | Method (sign, _) -> is_method sign id
  | Constructor (sign, _) -> is_constructor sign id
  | Main _ -> None
;;

let get_class_member_sign cl_d el_id =
  let f id acc el =
    match acc, el with
    | None, el -> get_sign id el
    | ret, _ -> ret
  in
  List.fold_left (f el_id) None cl_d.cl_mems
;;

let find_global id = read_global (Code_ident id) >>| fun x -> get_class_decl x

let is_public sign =
  match sign with
  | Fild_sig { f_modif = Some (FAccess MPublic); _ }
  | Metod_sig { m_modif = Some (MAccess MPublic); _ }
  | Constructor_sig { con_modif = Some MPublic; _ } -> return sign
  | _ -> fail (`Access_error "Attempt to get a private class member")
;;

let rec find_class_member expr cl_decl =
  let find_mem id =
    match get_class_member_sign cl_decl id with
    | Some x -> return x
    | None -> fail (`Other_error "Parsing error in EPoint_access")
  in
  let get_next_decl_id mem_id =
    find_mem mem_id
    >>= is_public
    >>= function
    | Fild_sig { f_modif = _; f_type = TVar (TNullable (TClass id)); _ } -> return id
    | Metod_sig { m_modif = _; m_type = TReturn (TNullable (TClass id)); _ } -> return id
    | Constructor_sig { con_modif = _; con_id; _ } -> return con_id
    | _ -> fail (`Access_error "Magic case")
  in
  match expr with
  | EIdentifier id -> find_mem id >>= is_public
  | EPoint_access (EIdentifier id, expr1) ->
    get_next_decl_id id >>= find_global >>= find_class_member expr1
  | _ -> fail (`Other_error "Parsing error in EPoint_access")
;;

let point_check e1 e2 =
  let helper =
    match e1 with
    | EIdentifier id ->
      let local_find = read_local id in
      let global_find = read_global (Code_ident id) >>| get_class_decl in
      local_find <|> (global_find >>= find_class_member e2 >>= is_public)
    | _ -> fail (`Other_error "Error during parsing in Epoint_access")
  in
  helper >>| fun x -> Some x
;;

let env_bool = Value_sig (TVar (TNot_Nullable TBool))
let env_bool_null = Value_sig (TVar (TNullable (TBase TBool)))
let env_int = Value_sig (TVar (TNot_Nullable TInt))
let env_int_null = Value_sig (TVar (TNullable (TBase TInt)))
let env_char = Value_sig (TVar (TNot_Nullable TChar))
let env_char_null = Value_sig (TVar (TNullable (TBase TChar)))
let env_string = Value_sig (TVar (TNullable TString))

let operands_eq oper1 oper2 =
  match oper1, oper2 with
  | None, None | None, _ | _, None ->
    fail (`Other_error "Using keyword Null with math operations")
  | _ -> eq_t_env_opt oper1 oper2
;;

let base_type_eq2 tp0 tp1 tp2 = tp0 >>= eq_t_env_opt tp1 <|> tp0 >>= eq_t_env_opt tp2

let check_bin_op op (env1, env2) =
  let is_operands_eq = operands_eq env1 env2 in
  let base_type_eq2 = base_type_eq2 is_operands_eq in
  (* _ *)
  let int_op = base_type_eq2 (Some env_int) (Some env_int_null) in
  let int_bool_op = int_op *> return (Some env_bool) in
  let bool_op = base_type_eq2 (Some env_bool) (Some env_bool_null) in
  let char_op = base_type_eq2 (Some env_char) (Some env_char_null) in
  let string_op = is_operands_eq >>= eq_t_env_opt (Some env_string) in
  let compare_op = choice [ int_op; bool_op; char_op; string_op ] in
  match op with
  | Asterisk | Plus | Minus | Division | Mod -> int_op
  | Less | LessOrEqual | More | MoreOrEqual -> int_bool_op
  | And | Or -> bool_op
  | Equal | NotEqual -> compare_op
  | Assign -> is_operands_eq
;;

let check_un_op op env1 =
  match op with
  | UMinus -> base_type_eq2 env1 (Some env_int) (Some env_int_null)
  | UNot -> base_type_eq2 env1 (Some env_bool) (Some env_bool_null)
  | New ->
    env1
    >>= (function
     | Some (Value_sig (TVar (TNullable (TClass x)))) ->
       return (Some (Value_sig (TVar (TNullable (TClass x)))))
     | _ -> fail `Type_mismatch)
;;

let check_expr exp =
  let rec helper e1 =
    match e1 with
    | EConst x ->
      (* TODO: отсюда берется None, потому что у Null нет типа *)
      return (to_type_v @@ to_var_type @@ to_atype x)
    | EIdentifier x -> read_local x >>| fun x -> Some x
    | EMethod_invoke (e2, Params prms) ->
      (* TODO: Юля должна себе придумать, как еще вызов конструктора проверять *)
      helper e2 (* TODO: ВСЕ КОНСТРУКТОРЫ ДОЛЖНЫ БЫТЬ В ЛОКАЛЬНОЙ ОБЛАСТИ ВИДИМОСТИ *)
      >>= fun x ->
      let env_val_prms = params_check helper prms in
      check_invoke x env_val_prms
    | EPoint_access (e1, e2) -> point_check e1 e2
    | EBin_op (op, e1, e2) ->
      lift2 (fun x y -> x, y) (helper e1) (helper e2) >>= check_bin_op op
    | EUn_op (op, e1) -> check_un_op op (helper e1)
  in
  helper exp
;;
