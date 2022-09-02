type expr =
  | Var of char * bool
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Implies of expr * expr
  | Contra

type proof =
  | Hypo of { prop : expr; cancel : bool }
  | Proof of { prop : expr; proofs : proof list; rule : rule }

and rule =
  | And_I
  | And_E of expr
  | Or_I of expr
  | Or_E of expr
  | Imp_I of expr
  | Imp_E
  | Neg_I of expr
  | Neg_E
  | Raa of expr

exception EmptyProof
exception NotImplemented
exception WrongProof of string
exception Contradiction
exception Forbidden
exception MissingAssum of expr * string
