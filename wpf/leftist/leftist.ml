(*implementation of priority queue using leftist tree*)

(*queue is a binary tree or Null*)
(*I can't do Objective Caml yet, so here we go:
left child * value in node * right child * height*)
type 'a tree = Node of 'a tree * 'a * 'a tree * int | Leaf

type 'a queue = None | 'a tree

(*exception raised by deleting from empty queue*)
exception Empty

(*exception for surpassing match warnings*)
(*it shouldn't ever appear, but it's nice thing to have*)
exception NoEmptyYetNoFull

(*empty queue*)
let empty = Leaf;;

(*height of a tree*)
let height (t : 'a queue) =
    function 
    | None -> 0
    | Leaf -> 0
    | Node(_, _, _, h) -> h
;;

(*join queues = merge trees*)
(*TODO: check if one of the queues is empty and handle it*)
let rec join (x : 'a queue) (y : queue) =
    match x, y with
    | None, _ -> y
    | _, None -> x
    | Node(x_left, x_value, x_right, x_height), 
      Node(y_left, y_value, y_right, y_height) ->
        if x_value < y_value then
            let left = x_left
            and right = join x_right y in
                if height left < height right then 
                    Node(right, x_value, left, height right + 1)
                else
                    Node(left, x_value, right, height left + 1)
        else
            let left = y_left
            and right = join y_right x in
                if height left < height right then
                    Node(right, y_value, left, height right + 1)
                else
                    Node(left, y_value, right, height left + 1)
;;

(*insert element a to queue q*)
let add a q =
    join q Node(Leaf, a, Leaf, 1)
;;

(*true if queue is empty false otherwise*)
let is_empty q = 
    q = Leaf
;;

(*delete element in root and return pait (element, queue without root)
**raise Empty exception if queue is empty*)
let delete_min (q : 'a queue) =
    if is_empty q then raise Empty

    match q with 
    | Node(q_left, q_value, q_right, _) ->
        let q_new = join q_left q_right in 
            q, q_new
    | _ -> raise NoEmptyYetNoFull      
;;

