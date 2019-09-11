open Allocine
open Lwt.Infix

module A = Api.Make (Net_config) (Cohttp_lwt_unix.Client)

let () = Lwt_main.run (
  A.search Sys.argv.(1) 10 >>= function
  | Error e -> Lwt_io.eprintl e
  | Ok d ->
    let open Types_j in
    Lwt_io.printf "movies\t%d\nseries\t%d\nnews\t%d\npeople\t%d\nmedia\t%d\n"
      (List.length d.movies)
      (List.length d.series)
      (List.length d.news)
      (List.length d.people)
      (List.length d.media) >>= fun () ->
    Lwt_io.printl "movies:\nID\tprod\ttitle" >>= fun () ->
    d.movies |> Lwt_list.iter_s @@ fun m ->
    Lwt_io.printf "%d\t%d\t%s\n"
      m.mov_code
      m.mov_production_year
      m.mov_original_title
)
