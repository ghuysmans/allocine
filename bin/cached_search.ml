let () =
  let open Allocine_t in
  let {feed} =
    Atdgen_runtime.Util.Json.from_file Allocine_j.read_search Sys.argv.(1)
  in
  Printf.printf "%d\n" feed.total_results
