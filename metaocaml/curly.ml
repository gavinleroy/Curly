(* Curly - a simplified Lisp DSL
 *
 * Gavin Gray
 * Aug, 2021
 * *)

open Runcode

exception Unbound_symbol

exception Expected_symbol

type expr =
  | Int of int
  | Symbol of string
  | Plus of expr * expr
  | Minus of expr * expr
  | IfZero of expr * expr * expr
  | Let of expr * expr * expr
  | Lambda of expr * expr
  | App of expr * expr

let empty_env _ = raise Unbound_symbol

let bind env sym v =
  fun y -> if sym=y then v else env y

let rec eval e env k =
  match e with
  | Int i -> k .<i>.
  | Symbol s -> k (env s)

  | Plus (lhs, rhs) ->
     eval lhs env (fun lhsv ->
          eval rhs env (fun rhsv ->
            k .<(.~lhsv + .~rhsv)>.))

  | Minus (lhs, rhs) ->
     eval lhs env (fun lhsv ->
          eval rhs env (fun rhsv ->
            k .<(.~lhsv - .~rhsv)>.))

  | IfZero (cnd, if_e, else_e) ->
     eval cnd env (fun cndv ->
         .<if .~cndv = 0 then
           .~(eval if_e env k)
         else .~(eval else_e env k)>.)

  | Let (Symbol sym, v, bdy) ->
     eval v env (fun v' ->
       .<.~(eval bdy (bind env sym v') k)>.)

  (* | App -> assert false
   * | Lambda -> assert false *)

  | _ -> assert false

let () =
  Printf.printf "Test prog : %d\n"
    (run (eval
       (Let (Symbol "x"
        , (Plus
            (Plus ((Int 1), (Int 2))
            , Int 3))
       , (Minus (Symbol "x", Int 1))))
       empty_env (fun x -> x)))
