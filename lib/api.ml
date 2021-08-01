exception HttpError of int

module Make (H : sig
  include Cohttp_lwt.S.Client
  type allocine_auth [@@warning "-34"]
end) = struct
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
      let uri =
        let open Uri in
        parameters |>
        (* FIXME use the real base URI? *)
        with_query' (of_string ("http://allo/" ^ service_method))
      in
      let open Lwt.Infix in
      H.get uri >>= fun (resp, body) ->
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
