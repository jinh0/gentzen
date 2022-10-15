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
  (* TODO: maybe refactor apply_rule to use valid_props? *)
  let fst = List.hd and snd ps = List.nth ps 1 in
  let valid_props proofs name n =
    if List.length proofs = n then List.map prop proofs
    else
      raise (WrongProof (name ^ " requires " ^ string_of_int n ^ " arguments"))
  in
  match rule with
  | And_I ->
      let props = valid_props proofs "And_I" 2 in
      Proof { prop = And (fst props, snd props); rule; proofs }
  | And_E elim ->
      let props = valid_props proofs "And_E" 1 in
      let prop =
        match fst props with
        | And (e, e') -> if e = elim then e' else e
        | _ -> raise (WrongProof "And_E requires one hypothesis of form A ^ B")
      in
      Proof { prop; rule; proofs }
  | Or_I intro ->
      let prop = fst (valid_props proofs "Or_I" 1) in
      Proof { prop = Or (prop, intro); rule; proofs }
  | Or_E cons -> (
      match proofs with
      | [ p ] -> raise NotImplemented
      | _ -> raise (WrongProof "Or_E requires one argument"))
  | Imp_I assum -> (
      match proofs with
      | [ (Proof { prop } as p) ] ->
          if List.mem assum (get_assums p) then
            let new_proof =
              Proof { prop = Implies (assum, prop); rule; proofs = [ p ] }
            in
            cancel_hypo assum new_proof
          else
            raise
              (MissingAssum (assum, "Missing the assumption " ^ to_str assum))
      | _ -> raise (WrongProof "Imp_I requires one argument"))
  | Imp_E -> (
      match proofs with
      | [ p; p' ] -> (
          match (prop p, prop p') with
          | Implies (e, e'), e'' ->
              if e != e'' then
                raise (WrongProof "Imp_E hypothesis does not match")
              else Proof { prop = e'; rule; proofs }
          | _ -> raise (WrongProof "Imp_E requires an Implies term"))
      | _ -> raise (WrongProof "Imp_E requires 2 arguments"))
  | Neg_I assum -> (
      match proofs with
      | [ p ] -> (
          match prop p with
          | Contra ->
              if List.mem assum (get_assums p) then
                cancel_hypo assum (Proof { prop = Not assum; rule; proofs })
              else
                raise
                  (MissingAssum (assum, "Missing the assumption " ^ to_str assum))
          | _ -> raise (WrongProof "Neg_I requires a contradiction"))
      | _ -> raise (WrongProof "Neg_I requires 1 argument"))
  | Neg_E -> (
      match proofs with
      | [ p; p' ] -> (
          match (prop p, prop p') with
          | e, Not e' | Not e, e' ->
              if e = e' then Proof { prop = Contra; rule; proofs }
              else raise (WrongProof "Neg_I terms do not cause a contradiction")
          | _ -> raise (WrongProof "Neg_I terms do not cause a contradiction"))
      | _ -> raise (WrongProof "Neg_I requires one argument"))
  | Raa assum -> (
      match proofs with
      | [ p ] -> (
          match (prop p, assum) with
          | Contra, Not e ->
              if List.mem assum (get_assums p) then
                cancel_hypo assum (Proof { prop = e; rule; proofs })
              else
                raise
                  (MissingAssum (assum, "Missing the assumption " ^ to_str assum))
          | _ -> raise (WrongProof "RAA requires a contradiction"))
      | _ -> raise (WrongProof "RAA requires one argument"))
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
