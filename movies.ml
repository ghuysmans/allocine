open Lib

type activity =
| Actor
| Director
| UnknownActivity

let string_of_activity = function
| Actor -> "actor"
| Director -> "director"
| UnknownActivity -> "?"

type gender =
| Male
| Female

let string_of_gender = function
| Female -> "woman"
| Male -> "man"

let string_of_gender_o = function
| Some g -> string_of_gender g
| None -> "<unknown>"

type person = {
    name: string;
    activities: activity list;
    p_code: int;
    gender: gender option;
    nationality: string option;
    picture: string option;
}

type date = {
    day: int;
    month: int;
    year: int;
}

let to_date s = try Some {
    day = String.sub s 8 2 |> int_of_string;
    month = String.sub s 5 2 |> int_of_string;
    year = String.sub s 0 4 |> int_of_string;
}
with _ -> None

let string_of_date d =
    Printf.sprintf "%d-%02d-%02d" d.year d.month d.day

let string_of_date_o = function
| None -> "<none>"
| Some x -> string_of_date x

type movie_or_series = {
    originalTitle: string;
    title: string option;
    ms_code: int;
    productionYear: int option;
    releaseDate: date option;
    directors: string list;
    mainActors: string list;
    poster: string option;
    userRating: float option;
}

let date_of_movie_or_series x =
    match x.productionYear with
    | None -> x.releaseDate
    | Some p ->
        match x.releaseDate with
        | None -> Some {day=0; month=0; year=p}
        | Some r ->
            if r.year <= p+2 then
                (* seems to be valid *)
                x.releaseDate
            else
                (* better approximation *)
                Some {day=0; month=0; year=p}

type search_result =
| Person of person
| Movie of movie_or_series
| Series of movie_or_series

let print_movie_or_series x =
    (* only the original title doesn't get printed *)
    print_endline ("\tfr:\t" ^ (string_of_option x.title));
    print_endline ("\tcode:\t" ^ (string_of_int x.ms_code));
    print_endline ("\tprod:\t" ^ (string_of_int_o x.productionYear));
    print_endline ("\trel:\t" ^ (string_of_date_o x.releaseDate));
    let d = date_of_movie_or_series x in
    print_endline ("\tdate:\t" ^ (string_of_date_o d));
    print_endline "\tdirectors:";
    x.directors |> List.iter (fun x -> print_endline ("\t\t"^x));
    print_endline "\tmain actors:";
    x.mainActors |> List.iter (fun x -> print_endline ("\t\t"^x));
    print_endline ("\tposter:\t" ^ (string_of_option x.poster));
    print_endline ("\tscore:\t" ^ (string_of_float_o x.userRating))

let print_person p =
    print_string (string_of_gender_o p.gender ^ "\t" ^ p.name ^ ": ");
    p.activities |> List.iter (fun x ->
        print_string ((string_of_activity x)^" "));
    print_string "\n"

let print_movie m =
    print_endline ("Movie\t" ^ m.originalTitle);
    print_movie_or_series m

let print_series s =
    print_endline ("Series\t" ^ s.originalTitle);
    print_movie_or_series s
