open Atp
open Typing
open Proof

let e1 = Var ('a', true)
let e2 = Var ('b', false)
let e3 = Var ('c', true)
let and_1 = And (e1, e2)
let and_2 = And (e1, e3)
let and_3 = And (and_2, and_2)
let and_4 = And (and_1, and_2)

let test_eval (res: bool) (e: expr) =
  Alcotest.(check bool) "" res (Proof.eval e)

let test_var_true () = test_eval true e1
let test_var_false () = test_eval false e2
let test_conj () = test_eval false (And (e1, e2))
let test_conj_asso () = test_eval false (And (e2, e1))

let h1 = Hypo e1
let h2 = Hypo e2

let test_and_i_left () =
  Alcotest.(check bool) "Are equal" true ((And (e1, e2)) = (Proof.apply_rule [h1; h2] And_I))

let () =
  let open Alcotest in
  run "Evaluate props" [
    "variables", [
      test_case "True variable" `Quick test_var_true;
      test_case "False variable" `Quick test_var_false;
    ];
    "conjunction", [
      test_case "One conjunction" `Quick test_conj;
      test_case "Associative check" `Quick test_conj_asso;
    ];
    (* "complex", [ *)
      (* test_case "One conjunction" `Quick test_conj; *)
      (* test_case "Associative check" `Quick test_conj_asso; *)
    (* ]; *)
    "and", [
      test_case "And_I left" `Quick test_and_i_left;
    ];
  ]
