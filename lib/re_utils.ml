(** Utility functions for [Re] *)

(** [repe t n] matches [t] repeated exactly [n] times. *)
let repe t n = Re.repn t n (Some n)

(** [fp_int n] matches an [n]-digit positive integer. *)
let fp_int n = Tyre.(
  conv int_of_string (Printf.sprintf "%0*d" n) (regex (repe Re.digit n))
)
