open Typing

(** Representation of a sequent as a 2-tuple of proposition lists *)
type sequent

(** Sequent calculus rules *)
type rule

(** Node of a sequent calculus proof *)
type node

(** [from_theorem thm] returns a `sequent` representation of the theorem *)
val from_theorem : expr list * expr -> sequent

val equiv_rule : is_conseq:bool -> expr -> rule

val apply : sequent -> rule -> expr -> node

val find_app : sequent -> expr * rule

val prove : sequent -> (expr * rule) list
