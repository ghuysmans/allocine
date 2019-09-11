type t = bool

let wrap = function
  | 0 -> false
  | 1 -> true
  | n -> failwith (Printf.sprintf "Binary.wrap: %d" n)

let unwrap b =
  if b then 1 else 0
