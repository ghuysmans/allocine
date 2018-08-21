open Allocine
open Lwt.Infix

let () = Lwt_main.run (
  movie (int_of_string Sys.argv.(1)) >>= function
  | None -> Lwt_io.printl "error"
  | Some m ->
    let open Allocine_t in
    Lwt_io.printf "%s\n" m.mov_original_title
)
