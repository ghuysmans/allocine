open Allocine
open Lwt.Infix

let () = Lwt_main.run (
  Api.person (int_of_string Sys.argv.(1)) >>= function
  | None -> Lwt_io.printl "error"
  | Some p ->
    let open Types_t in
    Lwt_io.printf "%s %s\n" p.per_name.given p.per_name.family
)
