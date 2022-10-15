open Typing

type sequent = expr list * expr list
type rule = And_L | And_R | Or_L | Or_R | Imp_L | Imp_R | Neg_L | Neg_R | Axiom

(** [finished sequent] Check if the sequent is finished decomposing,
    by checking if an assumption is found in the consequences *)
let finished sequent =
  let assums, conseqs = sequent in
  List.exists (fun x -> List.mem x conseqs) assums

  (** [rule is_conseq] Return the rule to apply for the given proposition. *)
let rule is_conseq = function
  | Not _ -> if is_conseq then Neg_R else Neg_L
  | And _ -> if is_conseq then And_R else And_L
  | Or _ -> if is_conseq then Or_R else Or_L
  | Implies _ -> if is_conseq then Imp_R else Imp_L
  | Var _ -> failwith "There is no rule for variables"

(** [find_app sequent] Find applicable expression and rule in sequent *)
let find_app sequent =
  let assums, conseqs = sequent in
  let is_appli = function Var _ -> false | _ -> true in
  match List.find_opt is_appli conseqs with
  | Some chosen -> (chosen, rule true chosen)
  | None ->
      let chosen = List.find is_appli assums in
      (chosen, rule false chosen)

let decompose sequent chosen rule =
  failwith "err"

module Natural = struct
  type rule = And_I | And_E | Or_I | Or_E | Imp_I | Imp_E | Neg_I | Neg_E
  type proof =
    | Assum of expr
    | Proof of { cons: expr; assums: proof list; rule: rule }

  let prop = function Assum e -> e | Proof { cons = e } -> e
  let create cons assums rule = Proof { cons; assums; rule }
end


(** [prove sequent] Construct a proof tree of the sequent *)
let prove sequent =
  let rec prove sequent lefts = 
    if finished sequent then failwith "err" 
    else
      let chosen, rule = find_app sequent in
      match rule, chosen with
      | _ -> failwith "err"
  in
  prove sequent []

