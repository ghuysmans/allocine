type t =
  | Male
  | Female

let wrap = function
  | 1 -> Male
  | 2 -> Female
  | _ -> failwith "Allocine_gender.wrap"

let unwrap = function
  | Male -> 1
  | Female -> 2
