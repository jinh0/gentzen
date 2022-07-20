open Gentzen
open Typing
open Yojson

let () =
  Dream.run @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" (fun _ -> Dream.html "Good morning, world!");
         Dream.post "/"
          (fun request ->
            let%lwt body = Dream.body request in
            let message_object = body |> Yojson.Safe.from_string in
            let _ = Format.printf "Parsed to %a" Yojson.Safe.pp message_object in
            Dream.html "Something!"
          );
         Dream.get "/api/test/:name" (fun request ->
             Dream.html (Dream.param request "name"));
       ]
