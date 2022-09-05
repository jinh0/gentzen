open Gentzen
open Gentzen.Typing

let th1 = "|- ((P -> R) /\\ (Q -> R)) -> ((P \\/ Q) -> R)"
let x = Parser.convert th1 |> Sequent.from_theorem |> Sequent.prove
