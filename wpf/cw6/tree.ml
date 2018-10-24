type 'a tree = Node of 'a tree * 'a * 'a tree | Leaf

let rec size t =
    match t with
    | Leaf -> 0
    | Node(left, _, right) -> size left + size right + 1
;;

let rec height t =
    match t with
    | Leaf -> 0
    | Node(left, _, right) -> (max (height left) (height right)) + 1
;;

(*size and height simultaniusly*)
let rec size_and_height t =
    match t with 
    | Leaf -> (0, 0)
    | Node(left, _, right) -> 
        let (left_size, left_height) = size_and_height left in
        let (right_size, right_height) = size_and_height right in 
            (left_size + right_size + 1, max left_height right_height + 1)
;;

(*check if tree is ultraleft*)
let ultraleft t =
    (*checks node by node*)
    let rec helper t = function
    | Leaf -> (0, 0, true)
    | Node(left,_ , right) -> 
        let (left_left, left_right, left_ok) = helper left in
        let (right_left, right_right, right_ok) = helper right in
            (left_left + 1, right_right + 1, left_right >= right_left && (left && right_left))
    in
        match helper t with _, _, ret -> ret
;;


(*a little bit better*)
exception error;;
let better_ultraleft t =
    (*checks node by node, raises exception if tree is not ultraleft*)
    let rec helper t = function
    | Leaf -> (0, 0)
    | Node(left,_ , right) -> 
        let (left_left, left_right, left_ok) = ultraleft left in
        let (right_left, right_right, right_ok) = ultraleft right in
            if left_right >= right_left then (left_left + 1, right_right + 1)
            else raise error
    in
        try let _ helper t in
            true
        with 
            error -> false
;;

(*prefix form of tree*)
let prefix t = 
    let rec pom t pref = 
        match t with
        | Leaf -> pref
        | Node(left, a, right) -> 
            let pref1 = pom right pref in 
            let pref2 = pom left pref1 in 
                a::pref2
    in 
        pom t []
;;


(*mirror image of a tree*)
let rec mirror = function
    | Leaf -> Leaf
    | Node(left, a, right) -> 
        Node(mirror right, a, mirror left)
;;


