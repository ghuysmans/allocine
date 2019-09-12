open CalendarLib
type t = Date.t

let fmt = "%Y-%m-%d"
let wrap = Printer.Date.from_fstring fmt
let unwrap = Printer.Date.sprint fmt
