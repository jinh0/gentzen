open Typing

let print_bool b = if b then print_endline "true" else print_endline "false"
let x = Var ('x', false)

let rec to_str (e : expr) : string =
  let concat (e1 : expr) (e2 : expr) (s : string) =
    to_str e1 ^ " " ^ s ^ " " ^ to_str e2
  in
  let paren (s : string) = "(" ^ s ^ ")" in
  match e with
  | Var (x, _) -> String.make 1 x
  | Not e' -> paren ("not " ^ to_str e')
  | And (e1, e2) -> paren (concat e1 e2 "and")
  | Or (e1, e2) -> paren (concat e1 e2 "or")
  | Implies (e1, e2) -> paren (concat e1 e2 "implies")
  | Contra -> "contra"

let prop = function
  | Hypo e -> e.prop
  | Proof p -> p.prop

let is_canceled = function
  | Hypo { cancel } -> cancel
  | Proof p -> false

let proof_to_str (proof : proof) = to_str (prop proof)

let assums_to_str =
  let rec aux = function
    | [] -> ""
    | h :: t -> to_str h ^ " " ^ aux t
  in
  function
  | Hypo h -> to_str h.prop
  | Proof { assums } -> string_of_int (List.length assums)

let rec proofs_to_str =
  let rec aux = function
    | [] -> ""
    | h :: t -> proof_to_str h ^ " " ^ aux t
  in
  function
  | Hypo h -> ""
  | Proof { proofs } -> aux proofs

let str_to_expr (_ : string) = raise NotImplemented
(* let proc_str s = raise NotImplemented *)
(* in *)
(* List.fold_right f (String.split_on_char ' ' s) [] *)
(* in  *)
(* let rec str_to_expr (s: string list) = *)
(* match s with *)
(* | s -> raise NotImplemented *)
(* in *)
(* proc_str s *)
