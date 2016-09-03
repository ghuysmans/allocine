open Allocine
open Movies
open Printf

let _ =
    let body = Lwt_main.run (search Sys.argv.(1) 10) in
    printf "items: %d\n" (List.length body);
    body |> List.iter (function
    | Person p -> print_person p
    | Movie m -> print_movie m
    | Series s -> print_series s)
