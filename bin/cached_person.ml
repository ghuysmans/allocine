let () =
  let open Allocine_t in
  match Atdgen_runtime.Util.Json.from_file Allocine_j.read_get_person Sys.argv.(1) with
  | {person=None; _} -> print_endline "error"
  | {person=Some p; _} ->
    Atdgen_runtime.Util.Json.to_channel Allocine_j.write_person stdout p
