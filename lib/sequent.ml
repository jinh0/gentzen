(** Sequent calculus:

    This module contains functions related to producing proofs
    using sequent calculus. *)

open Typing

type sequent = expr list * expr list
type rule = And_L | And_R | Or_L | Or_R | Imp_L | Imp_R | Neg_L | Neg_R
type node = One of sequent | Branch of node * node

let from_theorem (assums, conseq) = assums, [conseq]

let equiv_rule ~is_conseq = function
  | Not _ -> if is_conseq then Neg_R else Neg_L
  | And _ -> if is_conseq then And_R else And_L
  | Or _ -> if is_conseq then Or_R else Or_L
  | Implies _ -> if is_conseq then Imp_R else Imp_L
  | Var _ -> failwith "There is no rule for variables"

let apply sequent rule exp =
  let assums, conseqs = sequent in
  let filter l = List.filter (fun x -> x <> exp) l in
  match rule, exp with
  | And_L, And (e, e') -> One (e :: e' :: filter assums, conseqs)
  | And_R, And (e, e') ->
      let a = One (assums, e :: filter conseqs)
      and b = One (assums, e' :: filter conseqs) in
      Branch (a, b)
  | Or_L, Or (e, e') -> 
      let a = One (e :: filter assums, conseqs)
      and b = One (e' :: filter assums, conseqs) in
      Branch (a, b)
  | Or_R, Or (e, e') -> One (assums, e :: e' :: filter conseqs)
  | Imp_L, Implies (e, e') ->
      let a = One (filter assums, e :: conseqs)
      and b = One (e' :: filter assums, conseqs) in
      Branch (a, b)
  | Imp_R, Implies (e, e') -> One (e :: assums, e' :: filter conseqs)
  | Neg_L, Not e -> One (filter assums, e :: conseqs)
  | Neg_R, Not e -> One (e :: assums, filter conseqs)
  | _, _ -> failwith "apply wrong"

let finished sequent =
  let assums, conseqs = sequent in
  match List.find_opt (fun x -> List.mem x conseqs) assums with
  | Some _ -> true
  | None -> false

let find_app sequent =
  let assums, conseqs = sequent in
  let fst_comp l = List.find_opt (function Var _ -> false | _ -> true) l in
  match fst_comp conseqs with
  | Some chosen -> (chosen, equiv_rule ~is_conseq:true chosen)
  | _ -> begin
        match fst_comp assums with
        | Some chosen -> (chosen, equiv_rule ~is_conseq:false chosen)
        | _ -> failwith "Application not found"
      end

let print sequent =
  let assums, conseqs = sequent in
  let combine expr_list =
    List.fold_left (fun str e -> if str = "" then Parser.expr_to_str e else str ^ ", " ^ Parser.expr_to_str e) "" expr_list
  in
  print_endline ((combine assums) ^ " |- " ^ (combine conseqs))

let prove sequent =
  let rec next = function
    | One sequent ->
        if finished sequent then
          let _ = print sequent in []
        else
          let rule, chosen = find_app sequent in
          print sequent; (rule, chosen) :: next (apply sequent chosen rule)
    | Branch (node, node') -> next node @ next node'
  in
  next (One sequent)

