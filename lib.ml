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

let download url destination =
    let open Lwt in
    let open Cohttp in
    let open Cohttp_lwt_unix in
    let open Net_config in
    let headers = Header.init_with "User-Agent" user_agent in
    Client.call ~headers `GET (Uri.of_string url) >>= fun (resp, body) ->
    let status = Response.status resp in
    match status with
    | `OK -> Cohttp_lwt_body.to_string body >>= fun s ->
        let c = open_out destination in
        output c s 0 (String.length s);
        close_out c;
        return ()
    | _ -> raise (HttpError (Code.code_of_status status))
