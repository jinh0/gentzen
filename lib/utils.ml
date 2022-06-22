open Typing

let print_bool b =
  if b then print_endline "true"
  else print_endline "false"
let x = Var ('x', false)

let rec to_str (e: expr): string =
  let concat (e1: expr) (e2: expr) (s: string) =
    to_str e1 ^ " " ^ s ^ " " ^ to_str e2
  in
  let paren (s: string) = "(" ^ s ^ ")" in
  match e with
  | Var (x, _) -> String.make 1 x
  | Not e' -> paren ("not " ^ to_str e')
  | And (e1, e2) -> paren (concat e1 e2 "and")
  | Or (e1, e2) -> paren (concat e1 e2 "or")
  | Implies (e1, e2) -> paren (concat e1 e2 "implies")
  | Contra -> "contra"

let str_to_expr (_: string) = 
  raise NotImplemented 
  (* let proc_str s = raise NotImplemented *)
    (* in *)
    (* List.fold_right f (String.split_on_char ' ' s) [] *)
  (* in  *)
  (* let rec str_to_expr (s: string list) = *)
    (* match s with *)
    (* | s -> raise NotImplemented *)
  (* in *)
  (* proc_str s *)


