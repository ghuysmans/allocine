open Allocine
open Lwt.Infix

module A = Api.Make (Net_config) (Cohttp_lwt_unix.Client)

let () = Lwt_main.run (
  A.person (int_of_string Sys.argv.(1)) >>= function
  | Error e -> Lwt_io.eprintl e
  | Ok None -> Lwt_io.eprintl "not found"
  | Ok (Some p) ->
    let open Types_t in
    Lwt_io.printf "%s %s\n" p.per_name.given p.per_name.family
)
