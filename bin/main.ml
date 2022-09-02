open Gentzen

let th1 = "A -> B"
let assums, conseq = th1 |> Parser.convert
