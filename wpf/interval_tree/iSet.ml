(*
 * Written by: Jakub Zarzycki 371722
 * Review    : Natalia Kucharczuk
 *)

(* Node of left * interval * right * #elements in this tree * height *)
type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree * int * int

(* excteption to handle non empty intersections of intervals in cmp, 
 * but one is not contained in the other*)
exception Nonemptyintersection;;

let in_interval x (a, b) =
  a <= x && x <= b
;;

(* 
 * cmp introduces partial order on intervals.
 * disjoint intervals are well-ordered and intersecting intervals are handled
 * via exceptions
 *)
(* we assume that 2nd pair (x, y) came from our set *)
let cmp (a, b) (x, y) =
  if a = min_int then
    (if b = max_int then raise Nonemptyintersection
     else (if b + 1 < x then -1 else raise Nonemptyintersection))
  (* we already know that a is not minimal, so we can subtract *)
  else if b = max_int then
    (if a - 1 > y then 1 else raise Nonemptyintersection)
  (* now we know that we are not in an edge case *)
  (* checking +-1 because we may want to connect intervals *)
  else if a - 1 > y then 1
  else if b + 1 < x then -1
  else if a >= x && b <= y then 0
  else raise Nonemptyintersection
;;

(*returns sum of intervals if intersecting or first interval*)
let sum_intervals (a, b) (c, d) =
  try 
    let compare = cmp (a, b) (c, d) in
      if compare = 0 then (min a c), (max b d) else (a, b)
  with
    | Nonemptyintersection -> 
        (min a c), (max b d);;

type t = (int * int) tree;;

let height = function
  | Node (_, _, _, _, h) -> h
  | Empty -> 0
;;

(* # elements in this tree *)
let size = function
  | Node (_, _, _, b, _) -> b
  | Empty -> 0
;;

let make l (x, y) r =
  if size l = max_int 
  then Node (l, (x, y), r, max_int,  max (height l) (height r) + 1)
  else if size r = max_int
  then Node (l, (x, y), r, max_int,  max (height l) (height r) + 1)
  else 
  let s = size l + size r + (y - x + 1) in
    if s < 0 then Node (l, (x, y), r, max_int, max (height l) (height r) + 1)
    else Node (l, (x, y), r, s,  max (height l) (height r) + 1)
;;

let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else make l k r
;;

let rec min_elt = function
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found
;;

let rec remove_min_elt = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
  | Empty -> Empty
;;

let rec max_elt = function
  | Node (l, k, Empty, _, _) -> k
  | Node (_, _, r, _, _) -> max_elt r
  | Empty -> failwith "empty set has no max_elt"
;;

let rec remove_max_elt = function
  | Node (l, k, Empty, _, _) -> l
  | Node (l, k, r, _, _) -> bal l k (remove_max_elt r)
  | Empty -> Empty
;;

let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k = min_elt t2 in
      bal t1 k (remove_min_elt t2)
;;

let empty = 
  Empty
;;

let is_empty x = 
  x = Empty
;;

let mem x set =
  let rec loop = function
    | Node (l, (a, b), r, _, _) ->
        in_interval x (a, b) || if x < a then loop l else loop r
    | Empty -> false in
  loop set
;;

let value = function
  | Node (_, v, _, _, _) -> v
  | Empty -> failwith "empty set has no value"
;;

let rec join l v r =
  match (l, r) with
  | (Empty, _) -> bal Empty v r
  | (_, Empty) -> bal l v Empty
  | (Node(ll, lv, lr, _, lh), Node(rl, rv, rr, _, rh)) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
      make l v r
;;

let split x set =
  let rec loop x = function
    | Empty ->
        (Empty, false, Empty)
    | Node (l, (lower, upper), r, _, _) ->
      (try
        let c = cmp (x, x) (lower, upper) in
        if c = 0 then
          (if x - 1 >= lower then join l (lower, x - 1) Empty else l), 
          true,
          (if x + 1 <= upper then join Empty (x + 1, upper) r else r)
        else if c < 0 then
          let (ll, pres, rl) = loop x l in 
            (ll, pres, join rl (lower, upper) r)
        else
          let (lr, pres, rr) = loop x r in 
            (join l (lower, upper) lr, pres, rr)
      with
        Nonemptyintersection -> 
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

let add_helper (lower, upper) l r h =
    let (nl, _, _) = split lower l
    and (_, _, nr) = split upper r
    in
      let v, nl = 
        if mem (lower - 1) nl
        then (sum_intervals (lower, upper) (max_elt nl)), (remove_max_elt nl)
        else (lower, upper), nl
      in let nv, nr =
        if mem (upper + 1) nr
        then (sum_intervals v (min_elt nr)), (remove_min_elt nr)
        else v, nr
      in
        make nl nv nr
;;

let rec add x = function
  | Node (l, k, r, _, h) ->
      (try
        let c = cmp x k in
        if c = 0 then add_helper (sum_intervals x k) l r h
        else if c < 0 then
          let nl = add x l in
          bal nl k r
        else
          let nr = add x r in
          bal l k nr
      with
        Nonemptyintersection ->
          add_helper (sum_intervals k x) l r h)
  | Empty -> make Empty x Empty
;;

let remove (x, y) set =
  let (l, _, _) = split x set
  and (_, _, r) = split y set 
  in 
    merge l r
;;

let iter f set =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r in
  loop set
;;

let fold f set acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) ->
          loop (f k (loop acc l)) r in
  loop acc set
;;

let elements set = 
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) -> loop (k :: loop acc r) l in
  loop [] set
;;

let below x s =
  let rec loop acc = function
    | Empty -> if acc < 0 then max_int else acc
    | Node (l, (a, b), r, s, _) -> 
        if b < x then loop (size l + b - a + 1 + acc) r
        else if a > x then loop acc l
        else 
          let c = x - a in 
            if c < 0 then max_int
            else
                let res = size l + c + acc + 1 in 
                    if res < 0 then max_int else res
  in 
    loop 0 s
;;

