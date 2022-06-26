open Typing
open Utils

let rec eval (e : expr) =
  match e with
  | Var (_, v) -> v
  | Not e' -> not (eval e')
  | And (e', e'') -> eval e' && eval e''
  | Or (e', e'') -> eval e' || eval e''
  | Implies (e', e'') -> not (eval e' && not (eval e''))
  | Contra -> raise Contradiction

(* TODO: make tail recursive *)
let get_assums proof =
  let rec aux acc = function
    | Hypo { prop; cancel } -> if cancel then acc else prop :: acc
    | Proof { proofs } -> List.fold_left aux acc proofs
  in
  aux [] proof

let rec cancel_hypo canc = function
  | Hypo { prop; cancel } -> Hypo { prop; cancel = cancel || prop = canc }
  | Proof { prop; rule; proofs } ->
      let proofs' = List.map (cancel_hypo canc) proofs in
      Proof { prop; rule; proofs = proofs' }

let apply_rule (proofs : proof list) (rule : rule) =
  match rule with
  | And_I -> (
      match proofs with
      | [ p; p' ] -> Proof { prop = And (prop p, prop p'); rule; proofs }
      | _ -> raise WrongProof)
  | And_E elim -> (
      match proofs with
      | [ p ] ->
          let prop =
            match prop p with
            | And (e, e') -> if e = elim then e' else e
            | _ -> raise WrongProof
          in
          Proof { prop; rule; proofs = [ p ] }
      | _ -> raise WrongProof)
  | Or_I intro -> (
      match proofs with
      | [ p ] -> Proof { prop = Or (prop p, intro); rule; proofs }
      | _ -> raise WrongProof)
  | Or_E cons -> (
      match proofs with
      | [ p ] -> raise NotImplemented
      | _ -> raise WrongProof)
  | Imp_I assum -> (
      match proofs with
      | [ (Proof { prop } as p) ] ->
          if List.mem assum (get_assums p) then
            let new_proof =
              Proof { prop = Implies (assum, prop); rule; proofs = [ p ] }
            in
            cancel_hypo assum new_proof
          else raise (MissingAssum assum)
      | _ -> raise WrongProof)
  | Imp_E -> (
      match proofs with
      | [ p; p' ] -> raise NotImplemented
        (* match prop p, prop p' with
        | Implies (e, e'), e'' -> if e != e'' then WrongProof
        | _ -> raise WrongProof *)
      | _ -> raise WrongProof)
  | _ -> raise NotImplemented

let is_equiv e e' =
  match (e, e') with
  | Var (v, b), Var (v', b') -> v = v' && b = b'
  | Not n, Not n' -> n = n'
  | And (a, b), And (a', b') | Or (a, b), Or (a', b') ->
      (a = a' && b = b') || (a = b' && a' = b)
  | Implies (a, b), Implies (a', b') -> a = a' && b = b'
  | _ -> false

let rec verify (proof : proof) : bool =
  match proof with
  | Hypo _ -> true
  | Proof { prop; proofs; rule } as p ->
      List.for_all verify proofs && p = apply_rule proofs rule
