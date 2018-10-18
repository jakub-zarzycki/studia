(*wszystkie ogony*)
let tails l =
  let rec pom l ret =
    match l with
    | [] -> []::ret
    | h::t -> pom t (l::ret)
  in pom l []
;;

let rec pom head tail ret =
    match tail with
    | [] -> failwith "error"
    | [_] -> []
    | h::t -> h::pom t
;;

let heads l =s 
    funciton 
    | [] -> [[]]
    | l -> l::heads (pom l)
;;
