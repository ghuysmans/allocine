type 'a t = 'a option

let wrap = function
  | [] -> None
  | [x] -> Some x
  | _ -> failwith "first"

let unwrap = function
  | None -> []
  | Some x -> [x]
