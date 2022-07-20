open Gentzen
open Typing
open Proof

let e1 = Var ('a', true)
let e2 = Var ('b', false)
let e3 = Var ('c', true)
let and_1 = And (e1, e2)
let and_2 = And (e1, e3)
let and_3 = And (and_2, and_2)
let and_4 = And (and_1, and_2)

let test_eval (res : bool) (e : expr) =
  Alcotest.(check bool) "" res (Proof.eval e)

let test_var_true () = test_eval true e1
let test_var_false () = test_eval false e2
let test_conj () = test_eval false (And (e1, e2))
let test_conj_asso () = test_eval false (And (e2, e1))
let h1 = Hypo { prop = e1; cancel = false }
let h2 = Hypo { prop = e2; cancel = false }

let () =
  let open Alcotest in
  run "Evaluate props"
    [
      ( "variables",
        [
          test_case "True variable" `Quick test_var_true;
          test_case "False variable" `Quick test_var_false;
        ] );
      ( "conjunction",
        [
          test_case "One conjunction" `Quick test_conj;
          test_case "Associative check" `Quick test_conj_asso;
        ] )
      (* "complex", [ *)
      (* test_case "One conjunction" `Quick test_conj; *)
      (* test_case "Associative check" `Quick test_conj_asso; *)
      (* ]; *);
    ]
