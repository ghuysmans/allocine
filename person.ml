open Allocine
open Lwt.Infix

let () = Lwt_main.run (
  person (int_of_string Sys.argv.(1)) >>= function
  | None -> Lwt_io.printl "error"
  | Some p ->
    let open Allocine_t in
    Lwt_io.printf "%s\n" p.per_name
)
