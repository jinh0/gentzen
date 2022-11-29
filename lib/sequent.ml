open Typing

(* Utility type: either *)
type 'a either = One of 'a | Two of 'a * 'a

let bind f = function
  | One x -> One (f x) | Two (x, y) -> Two (f x, f y)

(* Main types *)
type sequent = expr list * expr list
type rule = And_L | And_R | Or_L | Or_R | Imp_L | Imp_R | Neg_L | Neg_R | Axiom
type direction = Left | Right | Neither
type proof = Proof of { sequent: sequent; applied: expr * rule; subproofs: proof either } | Axiom of sequent

let dir rule =
  match rule with
  | And_L | Or_L | Imp_L | Neg_L -> Left
  | And_R | Or_R | Imp_R | Neg_R -> Right
  | Axiom -> Neither

(** [finished sequent] Check if the sequent is finished decomposing,
    by checking if an assumption is found in the consequences *)
let finished sequent =
  let assums, conseqs = sequent in
  List.exists (fun x -> List.mem x conseqs) assums


(** [rule is_conseq] Return the rule to apply for the given proposition. *)
let rule is_conseq = function
  | Not _ -> if is_conseq then Neg_R else Neg_L
  | And _ -> if is_conseq then And_R else And_L
  | Or _ -> if is_conseq then Or_R else Or_L
  | Implies _ -> if is_conseq then Imp_R else Imp_L
  | Var _ -> failwith "There is no rule for variables"


(** [find_app sequent] Find applicable expression and rule in sequent *)
let find_app sequent =
  let assums, conseqs = sequent in
  let is_appli = function Var _ -> false | _ -> true in
  match List.find_opt is_appli conseqs with
  | Some chosen -> (chosen, rule true chosen)
  | None ->
      let chosen = List.find is_appli assums in
      (chosen, rule false chosen)


let apply (sequent: sequent) (chosen, rule) =
  let filter (list: expr list) = List.filter ((<>) chosen) list in
  let assums, conseqs = sequent in
  match chosen, rule with
  | And (e, e'), And_L -> One (e :: e' :: filter assums, conseqs)
  | And (e, e'), And_R ->
      Two ((assums, e :: filter conseqs), (assums, e' :: filter conseqs))
  | Or (e, e'), Or_L ->
      Two ((e :: filter assums, conseqs), (e' :: filter assums, conseqs))
  | Or (e, e'), Or_R -> One (assums, e :: e' :: filter conseqs)
  | Implies (e, e'), Imp_L ->
      Two ((filter assums, e :: conseqs), (e' :: filter assums, conseqs))
  | Implies (e, e'), Imp_R -> One (e :: assums, e' :: filter conseqs)
  | Not e, Neg_L -> One (filter assums, e :: conseqs)
  | Not e, Neg_R -> One (e :: assums, filter conseqs)
  | _, _ -> failwith "apply wrong"


let decompose sequent =
  let to_apply = find_app sequent in
  let decomposed = apply sequent to_apply in
  to_apply, decomposed


(** [prove sequent] Construct a proof tree of the sequent
    1. If finished, then return
    2. Find rule to apply
    3. Add to the proof tree the deconstructed sequents *)
let rec prove sequent =
  if finished sequent then Axiom sequent
  else
    let applied, decomposed = decompose sequent in
    Proof { sequent; applied; subproofs = bind prove decomposed }

