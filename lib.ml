let split = function
| Some s -> Str.split (Str.regexp_string ", ") s
| None -> []

let string_of_option = function
| Some t -> t
| None -> "<none>"

let string_of_float_o = function
| Some f -> string_of_float f
| None -> "<none>"

let string_of_int_o = function
| Some i -> string_of_int i
| None -> "<none>"

exception HttpError of int
