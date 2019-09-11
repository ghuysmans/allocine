type t = CalendarLib.Calendar.t

let fmt = Tyre.(
  start *>
  int <&> char '-' *> int <&> char '-' *> int <&> char 'T' *>
  int <&> char ':' *> int <&> char ':' *> int <* opt (char 'Z')
  <* stop
)

let wrap s =
  match Tyre.(exec (compile fmt) s) with
  | Ok (((((year, month), day), hour), minute), second) ->
    let open CalendarLib in
    Calendar.lmake ~year ~month ~day ~hour ~minute ~second ()
  | _ -> failwith ("Allocine_datetime.wrap " ^ s)

let unwrap t =
  Tyre.eval fmt CalendarLib.Calendar.(((((
    year t, Date.int_of_month (month t)), day_of_month t),
    hour t), minute t), second t)
