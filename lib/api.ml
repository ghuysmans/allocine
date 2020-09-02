(** API configuration: keys, etc. *)
module type CONFIG = sig
  val api_url : string
  val partner_key : string
  val private_key : string
  val user_agent : string
end

exception HttpError of int

module Make (C : CONFIG) (H : Cohttp_lwt.S.Client) = struct
  (** [last_response] contains the last successful HTTP response. *)
  let last_response = ref ""

  (** [call service params] calls a [service] with the given [params],
      caching the answer. *)
  let call ?bypass service_method parameters =
    let lifetime = Maki.(Lifetime.KeepFor (Time.days 5)) in
    let args = Maki.(Arg.[
      Hash.string @:: service_method;
      Hash.(list @@ pair string string) @:: parameters;
    ]) in
    let returning = Maki.Codec.string in
    Maki.call_pure ?bypass ~lifetime ~name:"allocine" ~args ~returning @@
    fun () ->
      let open Lwt.Infix in
      let signed =
        Uri.encoded_of_query @@
          ("sed", [
            let open Unix in
            let t = localtime (time ()) in
            Printf.sprintf "%d%02d%02d"
              (t.tm_year+1900) (t.tm_mon+1) t.tm_mday
          ]) ::
          ("partner", [C.partner_key]) ::
          List.map (fun (a,b) -> (a, [b])) parameters
      in
      let signature =
        service_method ^ signed ^ C.private_key |>
        Digestif.SHA1.digest_string |>
        Digestif.SHA1.to_raw_string |>
        Base64.encode_string
      in
      let headers = Cohttp.Header.init_with "User-Agent" C.user_agent in
      C.api_url ^ service_method ^ "?" ^ signed ^ "&sig=" ^ signature |>
      Uri.of_string |>
      H.get ~headers >>= fun (resp, body) ->
      match Cohttp.Response.status resp with
      | `OK ->
        Cohttp_lwt.Body.to_string body >>= fun b ->
        last_response := b;
        Lwt.return b
      | status ->
        Lwt.fail (HttpError (Cohttp.Code.code_of_status status))

  open Maki.E.Infix

  (** [search q max] looks for [q], returning at most [max] results. *)
  let search query max_results =
    let p = ["q", query; "count", string_of_int max_results] in
    call "search" (("format", "json") :: p) >|=
    Atdgen_runtime.Util.Json.from_string Types_j.read_search >|= fun s ->
    s.Types_t.feed

  (** [movie id] returns the movie with the given [id]. *)
  let movie id =
    let p = [("code", string_of_int id); ("profile", "large")] in
    call "movie" (("format", "json") :: p) >|=
    Atdgen_runtime.Util.Json.from_string Types_j.read_get_movie >|= fun m ->
    m.Types_t.movie

  (** [person id] returns the person with the given [id]. *)
  let person id =
    let p = [("code", string_of_int id); ("profile", "large")] in
    call "person" (("format", "json") :: p) >|=
    Atdgen_runtime.Util.Json.from_string Types_j.read_get_person >|= fun m ->
    m.Types_t.person
end

(** [clean name] removes small Roman numbers at the end of [name] (FIXME?). *)
let clean name =
  let re = Re.(compile (seq [
    group (rep any);
    blank; char '('; rep (alt [char 'V'; char 'I']); char ')'; eos
  ])) in
  match Re.exec_opt re name with
  | None -> name
  | Some g -> Re.Group.get g 1
