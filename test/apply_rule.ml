open Atp
open Typing
open Proof

let e1 = Var ('a', true)
let e2 = Var ('b', false)
let h1 = Hypo { prop = e1; cancel = false }
let h2 = Hypo { prop = e2; cancel = false }
let e3 = Hypo { prop = And (e1, e2); cancel = false }

let test_and_i () =
  Alcotest.(check bool)
    "Are equal" true
    (And (e1, e2) = Proof.apply_rule [ h1; h2 ] And_I)

let test_and_e_left () =
  Alcotest.(check bool)
    "Are equal" true
    (e2 = Proof.apply_rule [ e3 ] (And_E e1))

let test_and_e_right () =
  Alcotest.(check bool)
    "Are equal" true
    (e1 = Proof.apply_rule [ e3 ] (And_E e2))

let test_or_i () =
  let applied = Proof.apply_rule [ h1 ] (Or_I e2) in
  Alcotest.(check bool) "Are equal" true (Or (e1, e2) = applied)

let () =
  let open Alcotest in
  run "Apply rules"
    [
      ( "Apply And rules",
        [
          test_case "And_I" `Quick test_and_i;
          test_case "And_E left" `Quick test_and_e_left;
          test_case "And_E right" `Quick test_and_e_right;
        ] );
      ("Apply Or rules", [ test_case "Or_I" `Quick test_or_i ]);
    ]
