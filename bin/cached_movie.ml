let () =
  let open Allocine_t in
  match Atdgen_runtime.Util.Json.from_file Allocine_j.read_get_movie Sys.argv.(1) with
  | {movie=None; _} -> print_endline "error"
  | {movie=Some m; _} ->
    Printf.printf "%s\n" m.mov_original_title
