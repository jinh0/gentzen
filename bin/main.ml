open Gentzen
open Gentzen.Typing

let th1 = "|- ((P -> R) /\\ (Q -> R)) -> ((P \\/ Q) -> R)"
let x = Sequent.parse_prove
