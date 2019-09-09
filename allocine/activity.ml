open Atom_t

type t =
  | Actor
  | Director
  | Other of int * string

let wrap = function
  | {code=8001; _} -> Actor
  | {code=8002; _} -> Director
  | {code; s} -> Other (code, s)

let unwrap = function
  | Actor -> {code=8001; s="Acteur"}
  | Director -> {code=8002; s="Réalisateur"}
  | Other (code, s) -> {code; s}
