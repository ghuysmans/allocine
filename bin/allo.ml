open Allocine
module M = Api.Make (Allocine_cohttp_proxy.Make (Net_config) (Cohttp_lwt_unix.Client))
open Lwt.Infix

let print_casting t = function
  | None -> Printf.printf "%s\n\n" t;
  | Some {Types_t.directors; actors} ->
    Printf.printf "%s " t;
    if directors <> [] then (
      Printf.printf "de "; French.print_list directors;
    );
    Printf.printf "\n";
    List.iter (Printf.printf "* %s\n") actors;
    Printf.printf "\n"

let print_movie (t : Types_t.search_movie) =
  Printf.printf
    "http://www.allocine.fr/film/fichefilm_gen_cfilm=%d.html\n" t.smo_code;
  (match t.smo_title with
  | None -> Printf.printf "%s" t.smo_original_title
  | Some fr -> Printf.printf "%s (%s)" fr t.smo_original_title);
  (match t.smo_release with
  | None -> Printf.printf "\n"
  | Some {date = {year; _}; _} -> Printf.printf ", %d\n" year);
  print_casting "Film" t.smo_casting_short

let print_series (t : Types_t.search_series) =
  Printf.printf
    "http://www.allocine.fr/series/ficheserie_gen_cserie=%d.html\n" t.sse_code;
  Printf.printf "%s (%d-" t.sse_original_title t.sse_year_start;
  (match t.sse_year_end with
  | None -> Printf.printf ")\n"
  | Some e -> Printf.printf "%d)\n" e);
  print_casting "Série" t.sse_casting_short

let print_pretty fn =
  let ch = open_out fn in
  let t = Yojson.Safe.from_string (!M.last_response) in
  Yojson.Safe.pretty_to_channel ch t;
  close_out ch

let smoke_test t code =
  (* FIXME return ok *)
  try%lwt
    let%lwt () = Lwt_unix.sleep (Random.float 3.) in
    match t with
    | `Movie -> M.movie code >|= ignore
  with Atdgen_runtime.Oj_run.Error e ->
    let fn =
      match t with
      | `Movie -> "m_" ^ string_of_int code ^ ".json"
    in
    Lwt_io.eprintf "%s: %s\n" fn e >>= fun () ->
    print_pretty fn;
    Lwt.return ()


let () = exit @@ Lwt_main.run (
  match Sys.argv with
  | [| _; "test" |] ->
    Lwt.return 0 (* TODO extend smoke_test and get a bunch of examples *)
  | [| _; s |] ->
    (try%lwt
      match%lwt M.search s 10 with
      | Error e ->
        Lwt_io.eprintl e >>= fun () ->
        Lwt.return 1
      | Ok {movies; series; _} ->
        List.iter print_movie movies;
        List.iter print_series series;
        Printf.printf "%!";
        let%lwt () =
          List.map (fun {Types_t.smo_code; _} -> smo_code) movies |>
          Lwt_list.iter_s (smoke_test `Movie) (* FIXME combine *)
        in
        Lwt.return 0 (* FIXME *)
    with Atdgen_runtime.Oj_run.Error e ->
      Lwt_io.eprintl e >>= fun () ->
      print_pretty ("s_" ^ s ^ ".json");
      Lwt.return 1)
  | [| _; typ; fn |] ->
    let ff = Atdgen_runtime.Util.Json.from_file in
    (try
      match typ with
      | "movie" -> ignore (ff Types_j.read_get_movie fn); Lwt.return 0
      | "search" -> ignore (ff Types_j.read_search fn); Lwt.return 0
      | _ ->
        Lwt_io.eprintf "unknown type %s\n" typ >>= fun () ->
        Lwt.return 1
    with Atdgen_runtime.Oj_run.Error e ->
      Lwt_io.eprintf "%s\n" e >>= fun () ->
      Lwt.return 1)
  | _ ->
    Printf.eprintf "usage: %s QUERY | (movie|search) INPUT\n" Sys.argv.(0);
    Lwt.return 1
)
