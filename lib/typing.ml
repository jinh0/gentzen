type expr =
  | Var of char * bool
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Implies of expr * expr
  | Contra

type proof =
  | Hypo of expr
  | Proof of expr * proof list * rule
and rule =
  | And_I
  | And_E of expr
  | Or_I of expr
  | Or_E
  | Imp_I
  | Imp_E
  | Neg_I of expr
  | Neg_E
  | Raa of expr

exception EmptyProof
exception NotImplemented
exception WrongProof
exception Contradiction
exception Forbidden
