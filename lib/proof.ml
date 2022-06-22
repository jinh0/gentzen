open Typing

let rec eval (e: expr) =
  match e with
  | Var (_, v) -> v
  | Not e' -> not (eval e')
  | And (e', e'') -> eval e' && eval e''
  | Or (e', e'') -> eval e' || eval e''
  | Implies (e', e'') -> not (eval e' && not (eval e''))
  | Contra -> raise Contradiction
  
let prop proof =
  match proof with
  | Proof (e, _, _) -> e
  | _ -> raise Forbidden

let get_hypos proof =
  raise NotImplemented

let apply_rule (proofs: proof list) (rule: rule): expr =
  match rule with
  | And_I ->
      begin
        match proofs with
        | [Proof (e, _, _); Proof (e', _, _)] -> And (e, e')
        | _ -> raise WrongProof
      end
  | And_E _ -> raise NotImplemented
  | Or_I intro ->
      begin
        match proofs with
        | [p] -> Or (prop p, intro)
        | _ -> raise WrongProof
      end
  | Or_E -> raise NotImplemented
  | _ -> raise NotImplemented

let is_equiv e e' =
  match e, e' with
  | Var (v, b), Var (v', b') -> v = v' && b = b'
  | Not n, Not n' -> n = n'
  | And (a, b), And (a', b') | Or (a, b), Or (a', b')
    -> (a = a' && b = b') || (a = b' && a' = b)
  | Implies (a, b), Implies (a', b') -> a = a' && b = b'
  | _ -> false

let rec verify (proof: proof): bool =
  match proof with
  | Hypo _ -> true
  | Proof (exp, proofs, rule) ->
      List.for_all (fun p -> verify p) proofs &&
      is_equiv exp (apply_rule proofs rule)
