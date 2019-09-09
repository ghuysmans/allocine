type t = CalendarLib.Calendar.t

let fmt = Tyre.(start *> int <&> char '-' *> int <&> char '-' *> int <* stop)

let wrap s =
  match Tyre.(exec (compile fmt) s) with
  | Ok ((year, month), day) ->
    CalendarLib.Calendar.lmake ~year ~month ~day ()
  | Error _ -> failwith "Date.wrap"

let unwrap t =
  Tyre.eval fmt CalendarLib.Calendar.(((
    year t, CalendarLib.Date.int_of_month (month t)), day_of_month t))
