open Atom_t

type t = string list option

let wrap = function
  | [{code=7240; _}] -> None
  | l -> Some (List.map (fun {s; _} -> s) l)

let unwrap = function
  | None -> [{code=7240; s="Indéfini"}]
  | Some l -> List.map (fun s -> {code=42; s}) l
