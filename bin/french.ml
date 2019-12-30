let rec print_list = function
  | [] -> ()
  | [x] -> Printf.printf "%s" x
  | [x; y] -> Printf.printf "%s et %s" x y
  | x :: l -> Printf.printf "%s, " x; print_list l

let rec separate = function
  | [] -> []
  | [x] -> [x]
  | [x; y] -> [x; " et "; y]
  | x :: l -> x :: ", " :: separate l
