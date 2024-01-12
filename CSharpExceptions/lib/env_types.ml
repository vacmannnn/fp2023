(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

module Common_env = struct
  type code_ident = Code_ident of ident

  module Code_id = struct
    type t = code_ident

    let compare = compare
  end

  module CodeMap = Stdlib.Map.Make (Code_id)

  module Ident = struct
    type t = ident

    let compare = compare
  end

  module IdentMap = Stdlib.Map.Make (Ident)

  type address = Link of int [@@deriving show { with_path = false }]

  let incr_ (Link ad) = Link (ad + 1)
  let ln x = Link x

  module MemAddress = struct
    type t = address

    let compare = compare
  end

  module MemMap = Stdlib.Map.Make (MemAddress)

  type code_ctx =
    | Exception_ctx of class_decl
    | Class_ctx of class_decl

  let get_class_decl = function
    | Exception_ctx exc -> exc
    | Class_ctx cl -> cl
  ;;

  type text = code_ctx CodeMap.t
end

module Type_check_env = struct
  open Common_env

  type t_env_value =
    | Method_sig of method_sign
    | Constructor_sig of constructor_sign
    | Value_sig of var_type
    | Fild_sig of fild_sign
  [@@deriving show { with_path = false }]

  type t_loc_env = t_env_value IdentMap.t
  type type_check_ctx = text * t_loc_env * meth_type option * code_ident option
  type tp_checked = TP_Ok
end

module Eval_env = struct
  open Common_env

  type const =
    | IInstance of address
    | IBase_value of value_

  let to_inst x = IInstance x
  let to_val x = IBase_value x

  type instructions =
    | IMethod of method_sign * statement
    | IConstructor of constructor_sign * statement

  let to_meth sign body = IMethod (sign, body)
  let to_cons sign body = IConstructor (sign, body)

  type t_env_eval_const =
    | Init of const
    | Not_init

  let to_init x = Init x
  let to_not_init _ = Not_init

  type t_env_value =
    | IConst of t_env_eval_const
    | ICode of instructions

  let to_const x = IConst x
  let to_code x = ICode x

  let create_base_val x = to_const@@to_init@@to_val x
  let create_inst x = to_const@@to_init@@to_inst x

  type t_loc_env = address * t_env_value IdentMap.t

  (*  *)
  type t_global_env = text

  (*  *)
  type mem_el = code_ident * (t_env_value * fild_sign) IdentMap.t
  type memory = address * mem_el MemMap.t
  type interpret_ctx = t_global_env * t_loc_env * memory

  (* TODO: мб переписать эту штуку, встроить в signals *)
  type ('b, 'sys_err) sig_ = 
    | Return of 'b option
    | Exn of code_ident * address
    | Break
    | Error of 'sys_err

  type ('a, 'b, 'sys_err) eval_t =
    | Next of 'a
    (* | Signal of ('b, 'sys_err) sig_ *)
    | Return of 'b option
    | Exn of code_ident * address
    | Break
    | Error of 'sys_err

  let nsig x = Next x
  let rsig x = Return x
  let bsig = Break
  let esig id add = Exn (id, add)
  let error err = Error err
end
