type t = string list

let wrap x =
  Re.(split (compile (str ", ")) x)

let unwrap = String.concat ", "
