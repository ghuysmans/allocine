open Lib

type activity =
| Actor
| Director
| UnknownActivity

let string_of_activity = function
| Actor -> "actor"
| Director -> "director"
| UnknownActivity -> "?"

type person = {
    name: string;
    activities: activity list;
    p_code: int;
    male: bool; (* 1 *)
    nationality: string option; (* None=7240 *)
    picture: string option;
}

type movie_or_series = {
    originalTitle: string;
    title: string option;
    ms_code: int;
    productionYear: int option;
    directors: string list;
    mainActors: string list;
    poster: string option;
    userRating: float option;
}

type search_result =
| Person of person
| Movie of movie_or_series
| Series of movie_or_series

let print_movie_or_series x =
    (* only the original title doesn't get printed *)
    print_endline ("\tfr:\t" ^ (string_of_option x.title));
    print_endline ("\tcode:\t" ^ (string_of_int x.ms_code));
    print_endline ("\tyear:\t" ^ (string_of_int_o x.productionYear));
    print_endline "\tdirectors:";
    x.directors |> List.iter (fun x -> print_endline ("\t\t"^x));
    print_endline "\tmain actors:";
    x.mainActors |> List.iter (fun x -> print_endline ("\t\t"^x));
    print_endline ("\tposter:\t" ^ (string_of_option x.poster));
    print_endline ("\tscore:\t" ^ (string_of_float_o x.userRating))

let print_person p =
    let who = if p.male then "Man\t" else "Woman\t" in
    print_string (who ^ p.name ^ ": ");
    p.activities |> List.iter (fun x ->
        print_string ((string_of_activity x)^" "));
    print_string "\n"

let print_movie m =
    print_endline ("Movie\t" ^ m.originalTitle);
    print_movie_or_series m

let print_series s =
    print_endline ("Series\t" ^ s.originalTitle);
    print_movie_or_series s
