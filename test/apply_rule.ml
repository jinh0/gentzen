open Atp
open Typing
open Proof

let pretty_print ppf proof =
  Fmt.pf ppf "%s %s" (Utils.proof_to_str proof) (Utils.proofs_to_str proof)

let testable_proof = Alcotest.testable pretty_print ( = )
let e1 = Var ('a', true)
let e2 = Var ('b', true)
let h1 = Hypo { prop = e1; cancel = false }
let h2 = Hypo { prop = e2; cancel = false }
let h1_or_i = Proof { prop = Or (e1, e2); rule = Or_I e2; proofs = [ h1 ] }
let h3 = Hypo { prop = And (e1, e2); cancel = false }

let test_and_i () =
  let new_proof =
    Proof { prop = And (e1, e2); proofs = [ h1; h2 ]; rule = And_I }
  in
  Alcotest.(check testable_proof)
    "Are equal" new_proof
    (Proof.apply_rule [ h1; h2 ] And_I)

let test_and_e_left () =
  let new_proof = Proof { prop = e2; proofs = [ h3 ]; rule = And_E e1 } in
  Alcotest.(check testable_proof)
    "Are equal" new_proof
    (Proof.apply_rule [ h3 ] (And_E e1))

let test_and_e_right () =
  let new_proof = Proof { prop = e1; proofs = [ h3 ]; rule = And_E e2 } in
  Alcotest.(check testable_proof)
    "Are equal" new_proof
    (Proof.apply_rule [ h3 ] (And_E e2))

let test_or_i () =
  let applied = Proof.apply_rule [ h1 ] (Or_I e2) in
  Alcotest.(check testable_proof) "Are equal" h1_or_i applied

let test_canc_hypo () =
  let canceled = Proof.cancel_hypo e1 h1_or_i in
  let h1_or_i_canceled =
    Proof
      {
        prop = Or (e1, e2);
        rule = Or_I e2;
        proofs = [ Hypo { prop = e1; cancel = true } ];
      }
  in
  Alcotest.(check testable_proof) "Are equal" canceled h1_or_i_canceled

let test_imp_i () =
  let prop = Implies (Var ('b', true), Or (Var ('a', true), Var ('b', true))) in
  let or_proof =
    Proof
      {
        prop = Or (Var ('a', true), Var ('b', true));
        rule = Or_I (Var ('a', true));
        proofs = [ Hypo { prop = Var ('b', true); cancel = false } ];
      }
  in
  let or_proof_canc =
    Proof
      {
        prop = Or (Var ('a', true), Var ('b', true));
        rule = Or_I (Var ('a', true));
        proofs = [ Hypo { prop = Var ('b', true); cancel = true } ];
      }
  in
  let proof =
    Proof { prop; rule = Imp_I (Var ('b', true)); proofs = [ or_proof_canc ] }
  in
  Alcotest.(check testable_proof)
    "Are equal" proof
    (Proof.apply_rule [ or_proof ] (Imp_I e2))

let test_imp_i_no_assums () =
  let or_proof_canc =
    Proof
      {
        prop = Or (Var ('a', true), Var ('b', true));
        rule = Or_I (Var ('a', true));
        proofs = [ Hypo { prop = Var ('b', true); cancel = true } ];
      }
  in
  let applied =
    try Some (Proof.apply_rule [ or_proof_canc ] (Imp_I (Var ('b', true))))
    with MissingAssum _ -> None
  in
  let res =
    match applied with
    | Some x -> false
    | None -> true
  in
  Alcotest.(check bool) "Are equal" res true

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
      ( "Cancel hypotheses",
        [ test_case "Cancel hypothesis" `Quick test_canc_hypo ] );
      ( "Apply Implies rules",
        [
          test_case "Imp_I" `Quick test_imp_i;
          test_case "Imp_I with no assumption causes error" `Quick test_imp_i_no_assums;
        ]
      );
    ]
