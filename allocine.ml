(* TODO use the same interface for different providers *)
open Lib
open Movies
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic
open Sexplib (* TODO caching using a hash *)

(* Low-level API call *)
let allocine service_method parameters =
    let open Unix in
    let open Net_config in
    let sed = (
        let t = localtime (time ()) in
        Printf.sprintf "%d%02d%02d" (t.tm_year+1900) (t.tm_mon+1) t.tm_mday) in
    let signed =
        let converted = List.map (fun (a,b) -> (a, [b])) parameters in
        let enc = Uri.encoded_of_query in
        enc (("sed", [sed]) :: ("partner", [partner_key]) :: converted) in
    let signature =
        (private_key ^ signed) |> Sha1.string |> Sha1.to_bin |> B64.encode in
    let uri =
        api_url ^ service_method ^ "?" ^ signed ^ "&sig=" ^ signature in
    let header = Header.init_with "User-Agent" user_agent in
    Client.call ~headers:header `GET (Uri.of_string uri) >>= fun (resp, body) ->
    let status = Response.status resp in
    match status with
    | `OK -> body |> Cohttp_lwt_body.to_string
    | _ -> raise (HttpError (Code.code_of_status status))

let allocine = cache allocine


let activity_of_code = function
| 8001 -> Actor
| 8002 -> Director
| _ -> UnknownActivity

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
    let params = ["q", query; "count", string_of_int max_results] in
    allocine "search" (("format", "json")::params) >|= fun s ->
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
