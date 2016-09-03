(* TODO implement parsing in allocine.ml *)
open Allocine

let _ =
    let json = Lwt_main.run (allocine "movie"
        [("format", "json"); ("code", Sys.argv.(1)); ("profile", "large")]) in
    print_endline json
