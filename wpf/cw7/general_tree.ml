type 'a tree = Node of 'a * 'a tree list

(*height of a tree*)
let height t =
    let rec helper1 t = 
        match t with Node(_, l) -> helper2 l 0 + 1
    and helper2 l h =
        match l with
        | [] -> m
        | head::tail -> helper2 tail (max (helper1 head) h)
;;
