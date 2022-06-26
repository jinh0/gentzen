open Atp
open Typing
open Proof
open Utils

let pretty_print ppf proof = Fmt.pf ppf "%s" (Utils.proof_to_str proof)
let testable_proof = Alcotest.testable pretty_print ( = )

let e1 = Var ('a', true)
and e2 = Var ('b', false)

let h1 = Hypo { prop = e1; cancel = false }
let h1_canceled = Hypo { prop = e1; cancel = true }
let h2 = Hypo { prop = e2; cancel = false }
let p = Proof { prop = And (e1, e2); proofs = [ h1; h2 ]; rule = And_I }

let p_canceled =
  Proof { prop = And (e1, e2); proofs = [ h1_canceled; h2 ]; rule = And_I }

let test_cancel_hypo () =
  Alcotest.(check testable_proof) "same proof" (cancel_hypo e1 p) p_canceled

(* let () = *)
(* let open Alcotest in *)
(* run "Utils" *)
(* [ ("Cancel hypotheses", [ test_case "simple" `Quick test_cancel_hypo ]) ] *)
