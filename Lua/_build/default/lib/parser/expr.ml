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

(* let parse_opid pexpr =
   let parse_op =
     ws
     *> choice
          [ string "+" *> return Op_add
          ; string "-" *> return Op_sub
          ; string "<=" *> return Op_le ]
   in
   let parse_expr2 = option None (pexpr >>| Option.some) in
   lift3
     (fun exp1 opid exp2 -> Exp_op (opid, exp1, exp2))
     pexpr parse_op parse_expr2 *)

let parse_explhs = parse_lhs >>| fun lhs -> Exp_lhs lhs

let parse_function parse_block =
  let parse_idents =
    string "(" *> sep_by (ws *> string "," *> ws) parse_ident <* string ")"
  in
  lift2
    (fun idents bl -> Exp_function (idents, bl))
    (ws *> string "function" *> ws *> parse_idents)
    (parse_block <* string "end")
(*
   let chainl1 e op =
     let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
     e >>= fun init -> go init

   let rmul =
     ws *> string "*" *> ws *> return (fun e1 e2 -> Exp_op (Op_mul, e1, e2))

   let pmul pf = chainl1 pf rmul

   let rdiv =
     ws *> string "/" *> ws *> return (fun e1 e2 -> Exp_op (Op_div, e1, e2))

   let pdiv pf = chainl1 pf rdiv

   let radd =
     ws *> string "+" *> ws *> return (fun e1 e2 -> Exp_op (Op_add, e1, e2))

   let padd pf = chainl1 pf radd

   let rsub =
     ws *> string "-" *> ws *> return (fun e1 e2 -> Exp_op (Op_sub, e1, e2))

   let psub pf = chainl1 pf rsub

   let rmod =
     ws *> string "%" *> ws *> return (fun e1 e2 -> Exp_op (Op_mod, e1, e2))

   let pmod pf = chainl1 pf rmod

   let req =
     ws *> string "==" *> ws *> return (fun e1 e2 -> Exp_op (Op_eq, e1, e2))

   let peq pf = chainl1 pf req

   let rlt = ws *> string "<" *> ws *> return (fun e1 e2 -> Exp_op (Op_lt, e1, e2))

   let plt pf = chainl1 pf radd *)
(*
   let rle =
     ws *> string "<=" *> ws *> return (fun e1 e2 -> Exp_op (Op_le, e1, e2))

   let ple pf = chainl1 pf rle

   let parith p =
     let p = pmul p <|> p in
     let p = pdiv p <|> p in
     let p = padd p <|> p in
     let p = psub p <|> p in
     let p = pmod p <|> p in
     let p = peq p <|> p in
     let p = plt p <|> p in
     let p = ple p <|> p in
     p *)

let parse_expr pblock =
  ws
  *> choice
       [ parse_function pblock
       ; parse_true
       ; parse_false
       ; parse_nil
       ; parse_number
       ; parse_explhs ]
