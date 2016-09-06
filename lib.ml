open Lwt
open Cohttp
open Cohttp_lwt_unix

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

let cache f m p =
    let s = m ^ Uri.encoded_of_query (List.map (fun (a,b) -> (a, [b])) p) in
    let fn = Net_config.cache_dir ^ (s |> Sha1.string |> Sha1.to_hex) in
    let cached =
        try
            let open Unix in
            let exp = (stat fn).st_mtime +. Net_config.cache_duration in
            if exp >= time () then
                (* read it! *)
                let ch = open_in fn in
                let l = in_channel_length ch in
                let buf = Bytes.create l in
                let read = input ch buf 0 l in
                close_in ch;
                if read = l then
                    Some (return buf)
                else
                    (* TODO retry with the rest *)
                    None
            else
                (* expired *)
                None
        with _ ->
            (* not cached (or not accessible...) *)
            None in
    match cached with
    | None -> f m p >>= fun res ->
        let ch = open_out fn in
        output ch res 0 (String.length res);
        close_out ch;
        return res
    | Some x -> x

let process_lines in_ch process =
    let rec read_all () =
        try
            let line = input_line in_ch in
            process line;
            read_all ()
        with End_of_file -> () in
    read_all ();
    close_in in_ch

let rec process_one f = function
| [] -> None
| h :: t -> match f h with
    | Some x -> Some x
    | None -> process_one f t

let exists target =
    try
        ignore (Unix.stat target);
        true;
    with _ ->
        false
