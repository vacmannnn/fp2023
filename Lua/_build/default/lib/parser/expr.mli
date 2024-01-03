open! Base
open Angstrom
open Ast

val parse_expr : block t -> expression t

val parse_function: block t -> expression t