(*heap binary tree*)

(*TODO: finish it later*)
(*bool = go left / go right*)
type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree * bool

let insert x t =
    let rec helper x (dir, t) = 
        match t with
        | Leaf -> (Node (Leaf, x, Leaf, true), true)
        | Node (l, v, r, b) -> 
            if b then
                let (nl, dir) = insert x l 
                in Node (nl, v, r, dir) 
            else Node (l, v, insert x r, b)
;;

