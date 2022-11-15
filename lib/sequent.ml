(** Sequent calculus:

    This module contains functions related to producing proofs
    using sequent calculus. *)

open Typing

type sequent = expr list * expr list
type rule = And_L | And_R | Or_L | Or_R | Imp_L | Imp_R | Neg_L | Neg_R

type proof = None | Axiom of sequent | Node of (sequent * rule) * proof * proof

type prod = One of sequent | Branch of prod * prod

let from_theorem (assums, conseq) = (assums, [ conseq ])

(** Return the rule to apply for the given proposition. *)
let equiv_rule ~is_conseq = function
  | Not _ -> if is_conseq then Neg_R else Neg_L
  | And _ -> if is_conseq then And_R else And_L
  | Or _ -> if is_conseq then Or_R else Or_L
  | Implies _ -> if is_conseq then Imp_R else Imp_L
  | Var _ -> failwith "There is no rule for variables"

(** Check if the sequent is finished, i.e., 
    one of the assumptions equals one of the consequences. *)
let finished sequent =
  let assums, conseqs = sequent in
  match List.find_opt (fun x -> List.mem x conseqs) assums with
  | Some _ -> true
  | None -> false

(** Find an expression that can be broken down;
    prioritize consequences before assumptions. *)
let find_app sequent =
  let assums, conseqs = sequent in
  let fst_comp l =
    List.find_opt 
    (function Var _ -> false | Not x -> not (l = conseqs && List.length conseqs = 1) | _ -> true)
    l
  in
  match fst_comp conseqs with
  | Some chosen -> (chosen, equiv_rule ~is_conseq:true chosen)
  | _ -> (
      match fst_comp assums with
      | Some chosen -> (chosen, equiv_rule ~is_conseq:false chosen)
      | _ -> failwith "Application not found")

(** Print sequent *)
let print sequent =
  let assums, conseqs = sequent in
  let combine expr_list =
    List.fold_left
      (fun str e ->
        if str = "" then Parser.expr_to_str e
        else str ^ ", " ^ Parser.expr_to_str e)
      "" expr_list
  in
  print_endline (combine assums ^ " |- " ^ combine conseqs)

let apply sequent rule exp =
  let assums, conseqs = sequent in
  (* Create the new sequent by
      1. filtering out the old expression
      2. adding in the new, applied expression *)
  let filter list = List.filter (fun x -> x <> exp) list in
  match (rule, exp) with
  | And_L, And (e, e') -> One (e :: e' :: filter assums, conseqs)
  | Or_L, Or (e, e') ->
      let a = One (e :: filter assums, conseqs)
      and b = One (e' :: filter assums, conseqs) in
      Branch (a, b)
  | Imp_L, Implies (e, e') ->
      let a = One (filter assums, e :: conseqs)
      and b = One (e' :: filter assums, conseqs) in
      Branch (a, b)
  | Neg_L, Not e -> One (filter assums, e :: conseqs)
  | And_R, And (e, e') ->
      Branch
        (One (assums, e :: filter conseqs), One (assums, e' :: filter conseqs))
  | Or_R, Or (e, e') -> One (assums, e :: e' :: filter conseqs)
  | Imp_R, Implies (e, e') -> One (e :: assums, e' :: filter conseqs)
  | Neg_R, Not e -> One (e :: assums, filter conseqs)
  | _, _ -> failwith "apply wrong"

(** Produces the sequent calculus tree and returns
    the list of (rule, expr) applied, in preorder order. *)
let prove sequent =
  let rec next = function
    | One sequent ->
        if finished sequent then
          let _ = print sequent in
          []
        else
          let chosen, rule = find_app sequent in
          print sequent;
          (chosen, rule) :: next (apply sequent rule chosen)
    | Branch (sequent, sequent') -> next sequent @ next sequent'
  in
  next (One sequent)

let prove_str thm = Parser.convert thm |> from_theorem |> prove
