open Lwt.Infix

(* Low-level API call *)
let allocine service_method parameters =
  let signed =
    let sed =
      let open Unix in
      let t = localtime (time ()) in
      Printf.sprintf "%d%02d%02d" (t.tm_year+1900) (t.tm_mon+1) t.tm_mday
    in
    let converted = List.map (fun (a,b) -> (a, [b])) parameters in
    Uri.encoded_of_query @@
      ("sed", [sed]) :: ("partner", [Net_config.partner_key]) :: converted
  in
  let signature =
    (Net_config.private_key ^ signed) |> Sha1.string |>
    Sha1.to_bin |> B64.encode
  in
  let uri =
    Uri.of_string @@
    Net_config.api_url ^ service_method ^ "?" ^ signed ^ "&sig=" ^ signature
  in
  let header = Cohttp.Header.init_with "User-Agent" Net_config.user_agent in
  Cohttp_lwt_unix.Client.call ~headers:header `GET uri >>= fun (resp, body) ->
  let status = Cohttp.Response.status resp in
  match status with
  | `OK -> body |> Cohttp_lwt.Body.to_string
  | _ -> raise (Lib.HttpError (Cohttp.Code.code_of_status status))

let allocine = Lib.cache allocine

let search query max_results =
  let p = ["q", query; "count", string_of_int max_results] in
  allocine "search" (("format", "json") :: p) >|=
  Bytes.unsafe_to_string >|=
  Atdgen_runtime.Util.Json.from_string Allocine_j.read_search >|= fun s ->
  s.Allocine_t.feed

let movie id =
  let p = [("code", string_of_int id); ("profile", "large")] in
  allocine "movie" (("format", "json") :: p) >|=
  Bytes.unsafe_to_string >|=
  Atdgen_runtime.Util.Json.from_string Allocine_j.read_get_movie >|= fun m ->
  m.Allocine_t.movie

let person id =
  let p = [("code", string_of_int id); ("profile", "large")] in
  allocine "person" (("format", "json") :: p) >|=
  Bytes.unsafe_to_string >|=
  Atdgen_runtime.Util.Json.from_string Allocine_j.read_get_person >|= fun m ->
  m.Allocine_t.person

let clean_director s =
  let re = Re.(compile (seq [
    group (rep any);
    blank; char '('; rep (alt [char 'V'; char 'I']); char ')'; eos
  ])) in
  match Re.exec_opt re s with
  | None -> s
  | Some g -> Re.get g 1
