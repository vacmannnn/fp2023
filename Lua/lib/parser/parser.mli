open! Base
open Ast

val parse : string -> (block, string) result

val parse_exn : string -> block
