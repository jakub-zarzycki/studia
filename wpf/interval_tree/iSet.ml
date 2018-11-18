(*
 * Written by: Jakub Zarzycki 371722
 * Review    : Alicja Ziarko
 *)


(* 
 * we will use BST with order given by:
 * (a, b) < (x, y) if b < x
 * if they have non-empty interior we replace the one in the tree
 * with their union
 *
 * initially i wanted to just create pSet with 'k = 'a * 'a but it wouldn't work
 * T_T
 *)
type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree * int

(* excteption to handle non empty intersections of intervals in cmp, 
 * but one is not contained in the other*)
exception Nonemptyintersection;;

(* exception for handling one interval contained in the other *)
exception Contained;;

(* 
 * cmp introduces partial order on intervals.
 * disjoint intervals are well-ordered and intersecting intervals are handled
 * via exceptions
 *)
(* we assume that 2nd pair (x, y) came from our set *)
let cmp (a, b) (x, y) =
  if (b + 1) < x then -1
  else if y < (a - 1) then 1
  else if a = x && b = y then 0 
  else if a >= x && b <= y then raise Contained
  else raise Nonemptyintersection
;;

let in_interval x (a, b) =
  a <= x && x <= b
;;

type t = (int * int) tree;;

let height = function
  | Node (_, _, _, h) -> h
  | Empty -> 0
;;

let make l k r = 
  Node (l, k, r, max (height l) (height r) + 1)
;;

let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else Node (l, k, r, max hl hr + 1)
;;

let rec min_elt = function
  | Node (Empty, k, _, _) -> k
  | Node (l, _, _, _) -> min_elt l
  | Empty -> raise Not_found
;;

let rec remove_min_elt = function
  | Node (Empty, _, r, _) -> r
  | Node (l, k, r, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "PSet.remove_min_elt"
;;

let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k = min_elt t2 in
      bal t1 k (remove_min_elt t2)
;;

let create = 
  Empty
;;

let empty = 
  Empty
;;

let is_empty x = 
  x = Empty
;;

let rec add x = function
  | Node (l, k, r, h) ->
      (try
        let c = cmp x k in
        if c = 0 then Node (l, x, r, h)
        else if c < 0 then
          let nl = add x l in
          bal nl k r
        else
          let nr = add x r in
          bal l k nr
      with
        | Contained ->
            Node (l, x, r, h)
        | Nonemptyintersection ->
            let lower_k, upper_k = k 
            and lower_x, upper_x = x in
            let lower = min lower_k lower_x
            and upper = max upper_k upper_x in
            Node (l, (lower, upper), r, h))
  | Empty -> Node (Empty, x, Empty, 1)
;;

(*i give it 50-50, we'll see in tests*)
let rec join l v r =
  match (l, r) with
  | (Empty, _) -> add v r
  | (_, Empty) -> add v l
  | (Node(ll, lv, lr, lh), Node(rl, rv, rr, rh)) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
      make l v r
;;

let split x set =
  let rec loop x = function
    | Empty ->
        (Empty, false, Empty)
    | Node (l, (lower, upper), r, _) ->
      (try
        let c = cmp (x, x) (lower, upper) in
        if c = 0 then  
          (join l (lower, x - 1) Empty), 
          true, 
          (join Empty (x + 1, upper) r)
        else if c < 0 then
          let (ll, pres, rl) = loop x l in 
            (ll, pres, join rl (lower, upper) r)
        else
          let (lr, pres, rr) = loop x r in 
            (join l (lower, upper) lr, pres, rr)
      with
        | Contained ->
            (join l (lower, x - 1) Empty), 
            true, 
            (join Empty (x + 1, upper) r)
        | Nonemptyintersection -> 
            if in_interval x (lower, upper) 
            then
              (join l (lower, x) Empty), 
              true,
              (join Empty (x, upper) r)
            else
              if x < lower then
                (l, false, join Empty (lower, upper) r)
              else 
                (join l (lower, upper) Empty, false, r))
  in
  let setl, pres, setr = loop x set in
    setl, pres, setr
;;

let remove (x, y) set =
  let rec loop = function
    | Node (l, (lower, upper), r, _) ->
        (try 
          let c = cmp (x, y) (lower,upper) in
          if c = 0 then merge l r else
          if c < 0 then bal (loop l) (lower, upper) r 
          else bal l (lower, upper) (loop r)
        with 
          | Nonemptyintersection ->
              let k = if lower < x then (lower, x) else (y, upper) in
              join l k r
          | Contained ->
              merge
                (merge l (Node (Empty, (lower, x), Empty, 1)))
                (merge r (Node (Empty, (y, upper), Empty, 1))))
    | Empty -> Empty in
        loop set
;;

let mem x set =
  let rec loop = function
    | Node (l, (a, b), r, _) ->
        in_interval x (a, b) || if x < a then loop l else loop r
    | Empty -> false in
  loop set
;;

let exists = mem;;

let iter f set =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _) -> loop l; f k; loop r in
  loop set
;;

let fold f set acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _) ->
          loop (f k (loop acc l)) r in
  loop acc set
;;

let elements set = 
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _) -> loop (k :: loop acc r) l in
  loop [] set
;;

let below x s =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, (a, b), r, _) -> acc + (b - a + 1) + loop acc l + loop acc r
  in
    let (leq, _, _) = split x s in
    1 + loop 0 leq
;;
