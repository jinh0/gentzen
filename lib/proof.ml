open Typing

let rec eval (e: expr) =
  match e with
  | Var (_, v) -> v
  | Not e' -> not (eval e')
  | And (e', e'') -> eval e' && eval e''
  | Or (e', e'') -> eval e' || eval e''
  | Implies (e', e'') -> not (eval e' && not (eval e''))
  | Contra -> raise Contradiction
  
let get_hypos proof =
  raise NotImplemented

let apply_rule (proofs: proof list) (rule: rule): expr =
  match rule with
  (* Intro rules *)
  | And_I ->
      begin
        match proofs with
        | [Proof p; Proof p'] -> And (p.prop, p'.prop)
        | _ -> raise WrongProof
      end
  | Or_I intro ->
      begin
        match proofs with
        | [Proof p] -> Or (p.prop, intro)
        | _ -> raise WrongProof
      end
  (* Elimination rules *)
  | And_E elim ->
      begin
        match proofs with
        | [Proof { prop = And (e, e') }] -> if e = elim then e' else e
        | _ -> raise WrongProof
      end
  | Or_E elim -> raise NotImplemented
  | _ -> raise NotImplemented

let is_equiv e e' =
  match e, e' with
  | Var (v, b), Var (v', b') -> v = v' && b = b'
  | Not n, Not n' -> n = n'
  | And (a, b), And (a', b') | Or (a, b), Or (a', b') ->
      (a = a' && b = b') || (a = b' && a' = b)
  | Implies (a, b), Implies (a', b') -> a = a' && b = b'
  | _ -> false

let rec verify (proof: proof): bool =
  match proof with
  | Hypo _ -> true
  | Proof { prop; proofs; rule } ->
      List.for_all verify proofs &&
      is_equiv prop (apply_rule proofs rule)
