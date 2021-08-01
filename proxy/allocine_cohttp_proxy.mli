(** API configuration: keys, etc. *)
module type CONFIG = sig
  val api_url : string
  val partner_key : string
  val private_key : string
  val user_agent : string
end

module type S = sig
  include Cohttp_lwt.S.Client
  type allocine_auth
end

module Make : functor (C : CONFIG) (H : Cohttp_lwt.S.Client) -> S
