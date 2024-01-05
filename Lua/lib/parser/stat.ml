open! Base
open Angstrom
open Ast
open Common
open Expr

let parse_function1 pblock =
  let parse_idents =
    string "(" *> sep_by (ws *> string "," *> ws) parse_ident <* string ")"
  in
  lift4
    (fun local fun_name idents bl ->
      Stat_assign (local, fun_name, Exp_function (idents, bl)) )
    (option Nonlocal (string "local" *> return Local))
    (ws *> string "function" *> ws *> parse_lhs)
    (ws *> parse_idents)
    (pblock <* string "end")

let parse_assign pblock =
  (let parse_flag = option Nonlocal (string "local" *> return Local) in
   lift3
     (fun flag lhs expr -> Stat_assign (flag, lhs, expr))
     parse_flag (ws *> parse_lhs)
     (ws *> char '=' *> ws *> parse_expr pblock) )
  <|> parse_function1 pblock

let parse_if pblock =
  let parse_case_first =
    both (string "if" *> parse_expr pblock) (ws *> string "then" *> pblock)
  in
  let parse_case_rest =
    many
      (both
         (ws *> string "elseif" *> parse_expr pblock)
         (ws *> string "then" *> pblock) )
  in
  let parse_else_block =
    option None (ws *> string "else" >>| Option.some)
    >>= function None -> return None | Some _ -> pblock >>| Option.some
  in
  lift3
    (fun case_first case_rest else_block ->
      Stat_if (case_first :: case_rest, else_block) )
    parse_case_first parse_case_rest parse_else_block
  <* ws <* string "end"

let parse_while pblock =
  let parse_while1 = string "while" *> parse_expr pblock in
  let parse_pblock = ws *> string "do" *> pblock <* ws <* string "end" in
  lift2 (fun exp block -> Stat_while (exp, block)) parse_while1 parse_pblock

let parse_do pblock =
  string "do" *> pblock <* ws <* string "end" >>| fun block -> Stat_do block

let parse_return pblock =
  string "return" *> sep_by (string ",") (parse_expr pblock)
  >>| fun exprs -> Stat_return exprs

let parse_break = string "break" *> return Stat_break

let parse_stat pblock =
  ws
  *> choice
       [ parse_assign pblock
       ; parse_if pblock
       ; parse_do pblock
       ; parse_while pblock
       ; parse_return pblock
       ; parse_break ]

let parse_block : block t = fix (fun pblock -> many (parse_stat pblock) <* ws)
(* TODO: retstat *)

(* ======= Tests ======= *)

let%expect_test "parse_while" =
  pp pp_block parse_block "while false do x = 1 end" ;
  [%expect
    {|
    [(Stat_while (Exp_false,
        [(Stat_assign (Nonlocal, (Lhs_ident "x"), (Exp_number 1.)))]))
      ] |}]

let%expect_test "parse_return" =
  pp pp_block parse_block "return 10, true, a" ;
  [%expect
    {| [(Stat_return [(Exp_number 10.); Exp_true; (Exp_lhs (Lhs_ident "a"))])] |}]

let%expect_test "parse_while" =
  pp pp_block parse_block "while 9 do x = 1 b = 2 end" ;
  [%expect
    {|
    [(Stat_while ((Exp_number 9.),
        [(Stat_assign (Nonlocal, (Lhs_ident "x"), (Exp_number 1.)));
          (Stat_assign (Nonlocal, (Lhs_ident "b"), (Exp_number 2.)))]
        ))
      ] |}]

let%expect_test "parse_do" =
  pp pp_block parse_block "do a = 1 b = 2 if true then c = 3 end end" ;
  [%expect
    {|
    [(Stat_do
        [(Stat_assign (Nonlocal, (Lhs_ident "a"), (Exp_number 1.)));
          (Stat_assign (Nonlocal, (Lhs_ident "b"), (Exp_number 2.)));
          (Stat_if (
             [(Exp_true,
               [(Stat_assign (Nonlocal, (Lhs_ident "c"), (Exp_number 3.)))])],
             None))
          ])
      ] |}]

let%expect_test "parse_if1" =
  pp pp_block parse_block "if true then a = 1 b = 2 end" ;
  [%expect
    {|
    [(Stat_if (
        [(Exp_true,
          [(Stat_assign (Nonlocal, (Lhs_ident "a"), (Exp_number 1.)));
            (Stat_assign (Nonlocal, (Lhs_ident "b"), (Exp_number 2.)))])
          ],
        None))
      ] |}]

let%expect_test "parse_if2" =
  pp pp_block parse_block
    {| if false then a = 1 
       elseif true then b = 2 c = 3
       elseif 42 then d = 4
       else e = 5 end |} ;
  [%expect
    {|
    [(Stat_if (
        [(Exp_false, [(Stat_assign (Nonlocal, (Lhs_ident "a"), (Exp_number 1.)))]);
          (Exp_true,
           [(Stat_assign (Nonlocal, (Lhs_ident "b"), (Exp_number 2.)));
             (Stat_assign (Nonlocal, (Lhs_ident "c"), (Exp_number 3.)))]);
          ((Exp_number 42.),
           [(Stat_assign (Nonlocal, (Lhs_ident "d"), (Exp_number 4.)))])
          ],
        (Some [(Stat_assign (Nonlocal, (Lhs_ident "e"), (Exp_number 5.)))])))
      ] |}]

let%expect_test "parse_if3" =
  pp pp_block parse_block "if nil then a = 1 else a = 2 end" ;
  [%expect
    {|
    [(Stat_if (
        [(Exp_nil, [(Stat_assign (Nonlocal, (Lhs_ident "a"), (Exp_number 1.)))])],
        (Some [(Stat_assign (Nonlocal, (Lhs_ident "a"), (Exp_number 2.)))])))
      ] |}]

let%expect_test "parse_if4" =
  pp pp_block parse_block "if nil then a = 1 elseif true then a = 2 end" ;
  [%expect
    {|
    [(Stat_if (
        [(Exp_nil, [(Stat_assign (Nonlocal, (Lhs_ident "a"), (Exp_number 1.)))]);
          (Exp_true, [(Stat_assign (Nonlocal, (Lhs_ident "a"), (Exp_number 2.)))])
          ],
        None))
      ] |}]

let%expect_test "parse fun" =
  pp pp_block parse_block "if x == 3 then x = 2 end" ;
  [%expect
    {|
     [(Stat_if (
         [((Exp_op (Op_eq, (Exp_lhs (Lhs_ident "x")), (Exp_number 3.))),
           [(Stat_assign (Nonlocal, (Lhs_ident "x"), (Exp_number 2.)))])],
         None))
       ] |}]

let%expect_test "parse_fun1" =
  pp pp_block parse_block "local function abc (x) return x end" ;
  [%expect
    {|
    [(Stat_assign (Local, (Lhs_ident "abc"),
        (Exp_function (["x"], [(Stat_return [(Exp_lhs (Lhs_ident "x"))])]))))
      ] |}]

let%expect_test "parse_fun" =
  pp pp_expression
    (parse_function parse_block)
    "function (x,y,z,v) return x    end" ;
  [%expect
    {|
    (Exp_function (["x"; "y"; "z"; "v"],
       [(Stat_return [(Exp_lhs (Lhs_ident "x"))])])) |}]

let%expect_test "parse if5" = pp pp_block parse_block "if true then a=2 end"

let%expect_test "parse exp" =
  pp pp_expression (parse_expr parse_block) "1 * 2 * 3 + 5 * 6" ;
  [%expect
    {|
     (Exp_op (Op_add,
        (Exp_op (Op_mul, (Exp_op (Op_mul, (Exp_number 1.), (Exp_number 2.))),
           (Exp_number 3.))),
        (Exp_op (Op_mul, (Exp_number 5.), (Exp_number 6.))))) |}]

let%expect_test "parse exp1" =
  pp pp_expression (parse_expr parse_block) "abc + 1" ;
  [%expect
    {| (Exp_op (Op_add, (Exp_lhs (Lhs_ident "abc")), (Exp_number 1.))) |}]

let%expect_test "parse exp2" =
  pp pp_expression (parse_expr parse_block) "abc - 1 == 3" ;
  [%expect
    {|
     (Exp_op (Op_eq,
        (Exp_op (Op_sub, (Exp_lhs (Lhs_ident "abc")), (Exp_number 1.))),
        (Exp_number 3.))) |}]

(* let%expect_test "parse_fun2" =
   pp pp_block parse_block
     "function fact (n) if n == 0 then return 1 else return n * fact(n-1) end \
      end" *)

let%expect_test "parse_call" =
  pp pp_expression (parse_expr parse_block) "fact(n-1)" ;
  [%expect
    {|
    (Exp_call ((Exp_lhs (Lhs_ident "fact")),
       [(Exp_op (Op_sub, (Exp_lhs (Lhs_ident "n")), (Exp_number 1.)))])) |}]

let%expect_test "parse_call2" =
  pp pp_expression (parse_expr parse_block) "n * fact(n-1)"

let%expect_test "parse_abc" = pp pp_expression (parse_expr parse_block) "1/1"
