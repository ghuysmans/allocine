(** Wrapper for incomplete dates *)

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

open Re_utils

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
