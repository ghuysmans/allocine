(* TODO use the same interface for different providers *)
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic
open Sexplib


let split = function
    | Some s -> Str.split (Str.regexp_string ", ") s
    | None -> []

let string_of_option = function
    | Some t -> t
    | None -> "<none>"

let string_of_float_o = function
    | Some f -> string_of_float f
    | None -> "<none>"

let string_of_int_o = function
    | Some i -> string_of_int i
    | None -> "<none>"


exception HttpError of int

(* Low-level API call *)
let allocine service_method parameters =
    let open Unix in
    let open Net_config in
    let sed = (
        let t = localtime (time ()) in
        Printf.sprintf "%d%02d%02d" (t.tm_year+1900) (t.tm_mon+1) t.tm_mday) in
    let signed = (
        let converted = List.map (fun (a,b) -> (a, [b])) parameters in
        ("sed", [sed]) :: ("partner", [partner_key]) :: converted) in
    let signature = (
        let t = Sha1.string (private_key ^ Uri.encoded_of_query signed) in
        B64.encode (Sha1.to_bin t)) in
    let params = List.append signed [("sig", [signature])] in
    (* TODO optimize parameter passing (useless conversions...) *)
    let uri = api_url ^ service_method ^ "?" ^ (Uri.encoded_of_query params) in
    let header = Header.init_with "User-Agent" user_agent in
    Client.call ~headers:header `GET (Uri.of_string uri) >>= fun (resp, body) ->
    let status = Response.status resp in
    match status with
        | `OK -> body |> Cohttp_lwt_body.to_string
        | _ -> raise (HttpError (Code.code_of_status status))


type activity =
    | Actor
    | Director
    | UnknownActivity

let string_of_activity = function
    | Actor -> "actor"
    | Director -> "director"
    | UnknownActivity -> "?"

let activity_of_code = function
    | 8001 -> Actor
    | 8002 -> Director
    | _ -> UnknownActivity

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

let person_from_search v = {
    name = v |> Util.member "name" |> Util.to_string;
    activities =
        v |> Util.member "activity" |> Util.to_list |>
        Util.filter_member "code" |> Util.filter_int |>
        List.map activity_of_code;
    p_code = v |> Util.member "code" |> Util.to_int;
    male = v |> Util.member "gender" |> Util.to_int = 1;
    nationality = (
        v |> Util.member "nationality" |> Util.to_list |>
        List.filter (fun a -> (a |> Util.member "code" |> Util.to_int)<>7240) |>
        function
            | h::_ -> Some (h |> Util.member "$" |> Util.to_string)
            | [] -> None);
    picture =
        v |> Util.member "picture" |>
        function
            | `Null -> None
            | _ as x -> x |> Util.member "path" |> Util.to_string_option;
}

let movie_or_series_from_search v =
    let cs = Util.member "castingShort" v in {
    originalTitle = v |> Util.member "originalTitle" |> Util.to_string;
    title = v |> Util.member "title" |> Util.to_string_option;
    ms_code = v |> Util.member "code" |> Util.to_int;
    productionYear = v |> Util.member "productionYear" |> Util.to_int_option;
    directors = cs |> Util.member "directors" |> Util.to_string_option |> split;
    mainActors = cs |> Util.member "actors" |> Util.to_string_option |> split;
    poster = (
        v |> Util.member "poster" |>
        function
            | `Null -> None
            | _ as x -> Some (x |> Util.member "path" |> Util.to_string));
    userRating =
        v |> Util.member "statistics" |>
        function
            | `Null -> None
            | _ as x -> x |> Util.member "userRating" |> Util.to_float_option;
}

let search query max_results =
    let params = [("q", query); ("count", (string_of_int max_results))] in
    (if query = "orion" then
        (* TODO do some real caching! *)
        Lwt.return (
            let in_ch = (open_in "orion.list.json") in
            let l = input_line in_ch in
            close_in in_ch;
            l
        )
    else
        allocine "search" (("format", "json")::params)
    ) >|= fun s ->
    from_string s |> Util.member "feed" |>
    Util.to_assoc |> Util.filter_map (fun (k, v) -> match k with
        | "person" ->
            let l = Util.to_list v in
            Some (List.map (fun x -> Person (person_from_search x)) l)
        | "movie" ->
            let l = Util.to_list v in
            Some (List.map (fun x -> Movie (movie_or_series_from_search x)) l)
        | "tvseries" ->
            let l = Util.to_list v in
            Some (List.map (fun x -> Series (movie_or_series_from_search x)) l)
        | _ -> None) |> List.flatten;;


let _ =
    if (Array.length Sys.argv) = 2 then
        (*
        let json = Lwt_main.run (allocine "search"
            [("format", "json"); ("q", Sys.argv.(1)); ("count", "10")]) in
*)
        let json = Lwt_main.run (allocine "movie"
            [("format", "json"); ("code", Sys.argv.(1)); ("profile", "large")]) in
        print_endline json
    else
        let print_movie_or_series x =
            print_endline ("\tfr:\t" ^ (string_of_option x.title));
            print_endline ("\tcode:\t" ^ (string_of_int x.ms_code));
            print_endline ("\tyear:\t" ^ (string_of_int_o x.productionYear));
            print_endline "\tdirectors:";
            x.directors |> List.iter (fun x -> print_endline ("\t\t"^x));
            print_endline "\tmain actors:";
            x.mainActors |> List.iter (fun x -> print_endline ("\t\t"^x));
            print_endline ("\tposter:\t" ^ (string_of_option x.poster));
            print_endline ("\tscore:\t" ^ (string_of_float_o x.userRating))
            in
        let body = Lwt_main.run (search "orion" 10) in
        "items: " ^ (body |> List.length |> string_of_int) |> print_endline;
        body |> List.iter (function
            | Person p ->
                let who = if p.male then "Man\t" else "Woman\t" in
                print_string (who ^ p.name ^ ": ");
                p.activities |> List.iter (fun x ->
                    print_string ((string_of_activity x)^" "));
                print_string "\n";
            | Movie m ->
                print_endline ("Movie\t" ^ m.originalTitle);
                print_movie_or_series m
            | Series s ->
                print_endline ("Series\t" ^ s.originalTitle);
                print_movie_or_series s)
