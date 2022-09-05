(** Natural Deduction Calculus *)

open Typing

type proof =
  | Hypothesis of expr
  | And_I of { cons: expr; assums: expr * expr }
