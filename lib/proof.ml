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

let get_hypos proof = raise NotImplemented

let rec cancel_hypo canc = function
  | Hypo { prop; cancel } -> Hypo { prop; cancel = cancel || prop = canc }
  | Proof { prop; rule; proofs; assums } ->
      let proofs' = List.map (cancel_hypo canc) proofs in
      let assums' = List.filter (( <> ) canc) assums in
      Proof { prop; rule; proofs = proofs'; assums = assums' }

let archive_apply_rule (proofs : proof list) (rule : rule) : expr =
  match rule with
  (* Intro rules *)
  | And_I -> (
      match proofs with
      | [ p; p' ] -> And (prop p, prop p')
      | _ -> raise WrongProof)
  | Or_I intro -> (
      match proofs with
      | [ p ] -> Or (prop p, intro)
      | _ -> raise WrongProof)
  | Imp_I canc -> (
      match proofs with
      | [ Proof { prop; assums } ] ->
          if List.mem canc assums then Implies (canc, prop)
          else raise (MissingAssum canc)
      | _ -> raise WrongProof)
  (* Elimination rules *)
  | And_E elim -> (
      match proofs with
      | [ Proof { prop = And (e, e') } ] | [ Hypo { prop = And (e, e') } ] ->
          if e = elim then e' else e
      | _ -> raise WrongProof)
  | Or_E elim -> raise NotImplemented
  | _ -> raise NotImplemented

let union_assums (proofs : proof list) =
  let get_assums = function
    | Hypo { prop } -> [ prop ]
    | Proof { assums } -> assums
  in
  let f (acc : expr list) (cur : proof) =
    acc @ List.filter (fun x -> not (List.mem x acc)) (get_assums cur)
  in
  List.fold_left f [] proofs

let apply_rule (proofs : proof list) (rule : rule) =
  match rule with
  | And_I -> (
      match proofs with
      | [ p; p' ] ->
          Proof
            {
              prop = And (prop p, prop p');
              assums = union_assums proofs;
              rule;
              proofs;
            }
      | _ -> raise WrongProof)
  | And_E elim -> (
      match proofs with
      | [ p ] ->
          let prop =
            match prop p with
            | And (e, e') -> if e = elim then e' else e
            | _ -> raise WrongProof
          in
          Proof { prop; assums = union_assums proofs; rule; proofs = [ p ] }
      | _ -> raise WrongProof)
  | Or_I intro -> (
      match proofs with
      | [ p ] ->
          Proof
            {
              prop = Or (prop p, intro);
              assums = union_assums proofs;
              rule;
              proofs;
            }
      | _ -> raise WrongProof)
  | Imp_I canc -> (
      match proofs with
      | [ (Proof { prop; assums } as p) ] ->
          if List.mem canc assums then
            let new_proof =
              Proof
                { prop = Implies (canc, prop); rule; assums; proofs = [ p ] }
            in
            cancel_hypo canc new_proof
          else raise (MissingAssum canc)
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
