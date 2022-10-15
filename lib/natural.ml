(** Natural Deduction Calculus *)

open Typing

type rule = And_I | And_E | Or_I | Or_E | Imp_I | Imp_E | Neg_I | Neg_E

type proof =
  | Assum of expr
  | Proof of { cons: expr; assums: proof list; rule: rule }


