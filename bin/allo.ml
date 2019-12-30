open Allocine
module M = Api.Make (Net_config) (Cohttp_lwt_unix.Client)
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

let smoke_test t code =
  try%lwt
    let%lwt () = Lwt_unix.sleep (Random.float 3.) in
    match t with
    | `Movie -> M.movie code >|= ignore
  with Atdgen_runtime.Oj_run.Error e ->
    let s =
      match t with
      | `Movie -> "movie"
    in
    Lwt_io.eprintf "M.%s %d: %s\n" s code e


let () = Lwt_main.run (
  try%lwt
    match%lwt M.search Sys.argv.(1) 10 with
    | Error e -> Lwt_io.eprintl e
    | Ok {movies; series; _} ->
      List.iter print_movie movies;
      List.iter print_series series;
      Printf.printf "%!";
      let%lwt () =
        List.map (fun {Types_t.smo_code; _} -> smo_code) movies |>
        Lwt_list.iter_s (smoke_test `Movie)
      in
      Lwt.return ()
  with Atdgen_runtime.Oj_run.Error e ->
    Lwt_io.eprintf "M.search %S: %s\n" Sys.argv.(1) e
)
