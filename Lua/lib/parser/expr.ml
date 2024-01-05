open! Base
open Angstrom
open Ast
open Common

let parse_true = string "true" *> return Exp_true

let parse_false = string "false" *> return Exp_false

let parse_nil = string "nil" *> return Exp_nil

let parse_number =
  take_while (function '0' .. '9' | '.' | '-' -> true | _ -> false)
  >>= fun s ->
  try return (Exp_number (Float.of_string s))
  with Invalid_argument _ -> fail "not a number"

let parse_explhs = parse_lhs >>| fun lhs -> Exp_lhs lhs

let parse_function parse_block =
  let parse_idents =
    string "(" *> sep_by (ws *> string "," *> ws) parse_ident <* string ")"
  in
  lift2
    (fun idents bl -> Exp_function (idents, bl))
    (ws *> string "function" *> ws *> parse_idents)
    (parse_block <* string "end")

let pmul = string "*" *> return (fun x y -> Exp_op (Op_mul, x, y))

let pdiv = string "/" *> return (fun x y -> Exp_op (Op_div, x, y))

let padd = string "+" *> return (fun x y -> Exp_op (Op_add, x, y))

let psub = string "-" *> return (fun x y -> Exp_op (Op_sub, x, y))

let pmod = string "%" *> return (fun x y -> Exp_op (Op_mod, x, y))

let ppow = string "^" *> return (fun x y -> Exp_op (Op_pow, x, y))

let pcon = string ".." *> return (fun x y -> Exp_op (Op_concat, x, y))

let peq = string "==" *> return (fun x y -> Exp_op (Op_eq, x, y))

let plt = string "<" *> return (fun x y -> Exp_op (Op_lt, x, y))

let ple = string "<=" *> return (fun x y -> Exp_op (Op_le, x, y))

let pand = string "and" *> return (fun x y -> Exp_op (Op_and, x, y))

let por = string "or" *> return (fun x y -> Exp_op (Op_or, x, y))

(** pow bad parsing *)
let parse_op pexpr =
  let pth = ws *> pexpr in
  let pep = chainl1 pth (ws *> (ppow <|> pmul <|> pdiv)) in
  let pep = chainl1 pep (ws *> (padd <|> psub)) in
  let pep = chainl1 pep (ws *> (peq <|> plt <|> ple <|> pand <|> por)) in
  pep

let parse_call pexpr =
  lift2
    (fun e1 e2 -> Exp_call (e1, e2))
    (ws *> parse_explhs <* ws <* string "(")
    (sep_by (string ",") (ws *> (parse_op pexpr <|> parse_explhs)) <* string ")")
(* (many (ws *> pexpr <* string ",") <* ws <* string ")") *)

let parse_expr pblock =
  fix (fun pexpr ->
      ws
      *> choice
           [ parse_call pexpr
           ; parse_function pblock
           ; parse_false
           ; parse_true
           ; parse_nil
           ; parse_number
           ; parse_explhs
           ; parse_op pexpr ] )
