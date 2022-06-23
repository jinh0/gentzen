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
let apply_rule (proofs : proof list) (rule : rule) : expr =
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
  | Proof { prop; proofs; rule } ->
      List.for_all verify proofs && is_equiv prop (apply_rule proofs rule)
