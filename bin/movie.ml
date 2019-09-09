open Allocine
open Lwt.Infix

let () = Lwt_main.run (
  Api.movie (int_of_string Sys.argv.(1)) >>= function
  | None -> Lwt_io.printl "error"
  | Some m ->
    let open Types_t in
    Lwt_io.printf "%s\n" m.mov_original_title
)
