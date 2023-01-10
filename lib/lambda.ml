open Typing

type term =
  | Var of string
  | Pair of term * term
  | Fst of term
  | Snd of term
  | Fun of term * term (* function *)
  | App of term * term (* application *)
  | Inl of term * term
  | Inr of term * term
  | Case of term * term
(* | Unit
   | Abort of term *)
(* Negation? *)

let rec get_type term : expr =
  match term with
  | Var x -> Var (String.uppercase_ascii x)
  | Pair (x, y) -> And (get_type x, get_type y)
  | Fst (Pair (x, _)) -> get_type x
  | Snd (Pair (_, y)) -> get_type y
  | Fun (x, y) -> Implies (get_type x, get_type y)
  | App (Fun (x, y), x') ->
      if get_type x = get_type x' then get_type y
      else failwith "Wrong argument for function."
  | Inl (x, y) | Inr (x, y) -> Or (get_type x, get_type y)
  | Case (Inl (x, y), z) | Case (Inr (x, y), z) -> get_type z
  | Fst _ -> failwith "Fst can only be applied to Pairs."
  | Snd _ -> failwith "Snd can only be applied to Pairs."
  | App _ -> failwith "App can only be used on functions."
  | Case _ -> failwith "Case can only be used on Or."

let check_opt term = try Some (get_type term) with Failure message -> None
