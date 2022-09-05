open Typing

type sequent = expr list * expr list
(** Representation of a sequent as a 2-tuple of proposition lists *)

type rule = And_L | And_R | Or_L | Or_R | Imp_L | Imp_R | Neg_L | Neg_R
(** Sequent calculus rules *)

type node
(** Node of a sequent calculus proof *)

val from_theorem : expr list * expr -> sequent
(** [from_theorem thm] returns a `sequent` representation of the theorem *)

val prove : sequent -> (expr * rule) list

val apply : sequent -> rule -> expr -> node

val find_app : sequent -> expr * rule

val equiv_rule : is_conseq:bool -> expr -> rule

val print : sequent -> unit
