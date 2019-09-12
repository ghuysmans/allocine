type t = {
  year: int;
  month: int;
  day: int option;
}

(*
TODO use this instead?
type t = {
  year: int;
  m: m option;
}
and m = {
  month: int;
  day: int option;
}
*)

(** [repe t n] matches [t] repeated exactly [n] times. *)
let repe t n = Re.repn t n (Some n)

(** [fp_int n] matches an [n]-digit positive integer. *)
let fp_int n = Tyre.(
  conv int_of_string (Printf.sprintf "%0*d" n) (regex (repe Re.digit n))
)

let fmt = Tyre.(
  start *> fp_int 4 <&> char '-' *> fp_int 2 <&>
  (opt (char '-' *> fp_int 2)) <* stop
)

let wrap s =
  match Tyre.(exec (compile fmt) s) with
  | Ok ((year, month), day) ->
    {year; month; day}
  | Error _ -> failwith "Idate.wrap"

let unwrap {year; month; day} =
  Tyre.eval fmt ((year, month), day)
