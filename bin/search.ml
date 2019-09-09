open Allocine
open Lwt.Infix

let () = Lwt_main.run (
  Api.search Sys.argv.(1) 10 >>= fun d ->
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
