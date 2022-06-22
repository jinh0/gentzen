open Atp
open Typing

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/"
      (fun _ ->
        Dream.html "Good morning, world!");

    Dream.get "/api/test/:name"
      (fun request ->
        Dream.html (Dream.param request "name"));
  ]
