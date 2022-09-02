type expr =
  | Var of string
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Implies of expr * expr

