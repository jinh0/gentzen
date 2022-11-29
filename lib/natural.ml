(** Natural Deduction Calculus *)

open Typing

type rule = And_I | And_E | Or_I | Or_E | Imp_I | Imp_E | Neg_I | Neg_E

type nd_proof =
  | Assum of expr
  | Proof of { cons: expr; assums: nd_proof list; rule: rule }

let join chosen left right = failwith "err"

let rec convert tree top bottom =
  match tree with
  | Proof { applied = (prop, rule); subproofs } ->
      (* if right rule, then *)
        (* find applied prop in the leaves of the bottom trees *)
        (* then replace leaf with applied prop *)
      
      (* else, if left rule, then *)
        (* find applied prop in the root of the top trees *)
        (* then add the rule *)

      (* traverse down the subproofs *)
        (* One -> *)

        (* Two -> *)
          (* And_R ->  *)
      failwith "err"
  | Axiom (left, right) ->
      let chosen = find_finished left right in
      join chosen left right

let convert tree : nd_proof =
  let rec convert (tree : Sequent.proof) assums conseqs =
    match tree with
    | Proof { applied = (prop, rule); subproofs } ->
        let (assums', conseqs') =
          match rule with
          (* | And_R -> List.find (fun x -> x = prop) conseqs *)
          | _ -> failwith "err"
        in
        begin
          match subproofs with
          | One sub -> convert sub assums conseqs
          | _ -> failwith "err"
        end
    | Axiom (left, right) ->
        let chosen = List.find (fun x -> List.mem x right) left in
        join chosen left right
  in
  convert tree [] []
