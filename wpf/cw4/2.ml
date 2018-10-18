(*dublowanie elementów*)
let rec double l =
  match l with
    | [] -> []
    | h::t -> h::h::double t
;;
