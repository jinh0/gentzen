open Typing

type partial = Str of string | Nested of string | List of partial list

let to_list s =
  let rec iter n acc =
    if n = 0 then acc else iter (n - 1) (String.make 1 s.[n - 1] :: acc)
  in
  iter (String.length s) []

let rec str_to_part s =
  let convert x nested =
    if nested then Nested (String.trim x) else Str (String.trim x)
  in
  let rec iter str level acc nested = function
    | [] -> convert str nested :: acc
    | "(" :: t ->
        if level = 0 then iter "" (level + 1) acc true t
        else iter (str ^ "(") (level + 1) acc nested t
    | ")" :: t ->
        if level = 1 then iter str (level - 1) acc nested t
        else iter (str ^ ")") (level - 1) acc nested t
    | " " :: t ->
        if level = 0 then iter "" level (convert str nested :: acc) false t
        else iter (str ^ " ") level acc nested t
    | h :: t -> iter (str ^ h) level acc nested t
  in
  let parsed = s |> to_list |> iter "" 0 [] false |> List.rev in
  List parsed

let rec part_to_expr : partial -> expr = function
  | Str x -> Var x
  | Nested x -> part_to_expr (str_to_part x)
  | List [ Str "~"; t ] -> Not (part_to_expr t)
  | List [ a; Str "/\\"; b ] -> And (part_to_expr a, part_to_expr b)
  | List [ a; Str "\\/"; b ] -> Or (part_to_expr a, part_to_expr b)
  | List [ a; Str "->"; b ] -> Implies (part_to_expr a, part_to_expr b)
  | List [ a ] -> part_to_expr a
  | _ -> failwith "Wrong expression"


let rec join = function [] -> "" | h :: t -> h ^ ", " ^ join t

let split list_str =
  let rec iter cur acc = function
  | [] -> String.trim cur :: acc
  | h :: t ->
      if h = "," then iter "" (String.trim cur :: acc) t
      else iter (cur ^ h) acc t
  in
  iter "" [] list_str |> List.rev

let split_sides theorem =
  let rec get_assums cur assums = function
    | [] -> failwith "Something went wrong."
    | "|" :: _ -> String.trim cur :: assums
    | h :: t ->
        if h = "," then get_assums "" (String.trim cur :: assums) t
        else get_assums (cur ^ h) assums t
  and get_conseqs = function
    | [] -> []
    | "|" :: "-" :: t -> t |> split
    | _ :: t -> get_conseqs t
  in
  (List.rev (get_assums "" [] theorem), get_conseqs theorem)

let str_to_expr s = s |> str_to_part |> part_to_expr

let rec expr_to_str = function
  | Var v -> v
  | Not e -> "~" ^ expr_to_str e
  | And (e, e') -> "(" ^ expr_to_str e ^ " /\\ " ^ expr_to_str e' ^ ")"
  | Or (e, e') -> "(" ^ expr_to_str e ^ " \\/ " ^ expr_to_str e' ^ ")"
  | Implies (e, e') -> "(" ^ expr_to_str e ^ " -> " ^ expr_to_str e' ^ ")"

let convert theorem =
  let assums, conseqs = theorem |> to_list |> split_sides in
  (List.map str_to_expr assums, str_to_expr (List.nth conseqs 0))
