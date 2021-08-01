module type CONFIG = sig
  val api_url : string
  val partner_key : string
  val private_key : string
  val user_agent : string
end

module Make (C : CONFIG) (H : Cohttp_lwt.S.Client) = struct
  type ctx = H.ctx
  let default_ctx = H.default_ctx
  let sexp_of_ctx = H.sexp_of_ctx

  let get ?ctx ?(headers=Cohttp.Header.init ()) uri =
    (* FIXME not always *)
    let signed =
      Uri.encoded_of_query @@
        ("sed", [
          let open Unix in
          let t = localtime (time ()) in
          Printf.sprintf "%d%02d%02d"
            (t.tm_year+1900) (t.tm_mon+1) t.tm_mday
        ]) ::
        ("partner", [C.partner_key]) ::
        Uri.query uri
    in
    let service_method =
      let path = Uri.path uri in
      let open String in
      let after = rindex path '/' + 1 in
      sub path after (length path - after)
    in
    let signature =
      service_method ^ signed ^ C.private_key |>
      Digestif.SHA1.digest_string |>
      Digestif.SHA1.to_raw_string |>
      Base64.encode_string
    in
    let headers = Cohttp.Header.add headers "User-Agent" C.user_agent in
    C.api_url ^ service_method ^ "?" ^ signed ^ "&sig=" ^ signature |>
    Uri.of_string |> (* FIXME? *)
    H.get ?ctx ~headers

  let post_form = H.post_form
  let patch = H.patch
  let put = H.put
  let post = H.post
  let delete = H.delete
  let head = H.head

  let call ?ctx ?headers ?body ?chunked meth uri =
    match meth with
    | `GET -> get ?ctx ?headers uri
    | _ -> H.call ?ctx ?headers ?body ?chunked meth uri

  let callv ?ctx uri stream =
    (* FIXME *)
    H.callv ?ctx uri stream

  type allocine_auth
end
