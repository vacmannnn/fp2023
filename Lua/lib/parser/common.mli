(** Copyright 2023-2024, Alexandr Lekomtsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open! Base
open Angstrom
open Ast

val pp : (Format.formatter -> 'a -> unit) -> 'a t -> string -> unit
(**
  Run parser on string and pretty print the output using printer.
  Used for inline expect tests
*)

val ws : unit t
(** skip whitespaces *)

val parse_ident : ident t

val chainl1 : 'a t -> ('a -> 'a -> 'a) t -> 'a t
