(** Natural Deduction Calculus *)

open Typing

type rule = And_I | And_E | Or_I | Or_E | Imp_I | Imp_E | Neg_I | Neg_E

type proof =
  | Assum of expr
  | And_I of expr * proof * proof
  | And_E of expr * proof
  | Or_I of expr * proof
  | Or_E of expr * proof * proof * proof
  | Imp_I of expr * proof
  | Imp_E of expr * proof * proof
  | Neg_I of expr * proof
  | Neg_E of expr * proof * proof
  | Raa of expr * proof

let rec program_to_proof term =
  let open Lambda in
  match term with
  | Var x -> Assum (Var x)
  | Pair (x, y) as p ->
      And_I (get_type p, program_to_proof x, program_to_proof y)
  | Fst (Pair (x, _) as p) -> And_E (get_type x, program_to_proof p)
  | Snd (Pair (_, y) as p) -> And_E (get_type y, program_to_proof p)
  | Fun (input, output) -> Imp_I (get_type output, program_to_proof input)
  | App ((Fun (input, output) as f), input') ->
      Imp_E (get_type output, program_to_proof f, program_to_proof input')
  | Inl (x, _) as p -> Or_I (get_type p, program_to_proof x)
  | Inr (_, y) as p -> Or_I (get_type p, program_to_proof y)
  | Case ((Inl (x, y) as p), z) | Case ((Inr (x, y) as p), z) ->
      Or_E
        (get_type z, program_to_proof p, program_to_proof x, program_to_proof y)
  | _ -> failwith "TODO"
