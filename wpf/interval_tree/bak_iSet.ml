(*
 * implementation of iSet
 * Written by: Jakub Zarzycki 371722
 * Review    : Natalia Kucharczuk
 *)

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree * int

type t = (int * int) tree;;

let in_interval x (a, b) =
  a <= x && x <= b
;;

(* 
 * cmp introduces partial order on intervals.
 * disjoint intervals are well-ordered and intersecting intervals are handled
 * via exceptions
 *)
(* we assume that 2nd pair (x, y) came from existing set *)
let cmp (a, b) (x, y) =
  if a = min_int then
    (if b = max_int then 2
     else (if b + 1 < x then -1 else 2))
  (* we already know that a is not minimal, so we can subtract *)
  else if b = max_int then
    (if a - 1 > y then 1 else 2)
  (* now we know that we are not in an edge case *)
  (* checking +-1 because we may want to connect intervals *)
  else if a - 1 > y then 1
  else if b + 1 < x then -1
  else if a >= x && b <= y then 0
  else 2
;;

(*returns sum of intervals if intersecting or first interval*)
let sum_intervals (a, b) (c, d) =
  match cmp (a, b) (c, d) with
  | 0 | 2 -> (min a c), (max b d) 
  | _ -> (a, b)
;;

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
        | Empty -> failwith "cannot balance")
    | Empty -> failwith "cannot balance"
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> failwith "cannot balance")
    | Empty -> failwith "cannot balance"
  else Node (l, k, r, max hl hr + 1)
;;

let rec min_elt = function
  | Node (Empty, k, _, _) -> k
  | Node (l, _, _, _) -> min_elt l
  | Empty -> failwith "empty set has no min_elt"
;;

let rec remove_min_elt = function
  | Node (Empty, _, r, _) -> r
  | Node (l, k, r, _) -> bal (remove_min_elt l) k r
  | Empty -> Empty
;;

let rec max_elt = function
  | Node (l, k, Empty, _) -> k
  | Node (_, _, r, _) -> max_elt r
  | Empty -> failwith "empty set has no max_elt"
;;

let rec remove_max_elt = function
  | Node (l, k, Empty, _) -> l
  | Node (l, k, r, _) -> bal l k (remove_max_elt r)
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
    | Node (l, (a, b), r, _) ->
        in_interval x (a, b) || if x < a then loop l else loop r
    | Empty -> false 
  in
    loop set
;;

let value = function
  | Node (_, v, _, _) -> v
  | Empty -> failwith "empty set has no value"
;;

let rec join l v r =
  match (l, r) with
  | (Empty, _) -> bal Empty v r
  | (_, Empty) -> bal l v Empty
  | (Node(ll, lv, lr, lh), Node(rl, rv, rr, rh)) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
      make l v r
;;

let split x set =
  let rec loop x = function
    | Node (l, (lower, upper), r, _) ->
        (match cmp (x, x) (lower, upper) with
        | 0 ->
          (if x - 1 >= lower then join l (lower, x - 1) Empty else l), 
          true,
          (if x + 1 <= upper then join Empty (x + 1, upper) r else r)
        | -1 ->
          let (ll, pres, rl) = loop x l in 
            (ll, pres, join rl (lower, upper) r)
        | 1 ->
          let (lr, pres, rr) = loop x r in 
            (join l (lower, upper) lr, pres, rr)
        | _ ->
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
    | Empty ->
        (Empty, false, Empty)
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
        then
          (sum_intervals (lower, upper) (max_elt nl)), (remove_max_elt nl)
        else
          (lower, upper), nl
      in let nv, nr =
        if mem (upper + 1) nr
        then (sum_intervals v (min_elt nr)), (remove_min_elt nr)
        else v, nr
      in
        Node (nl, nv, nr, h)
;;

let rec add x = function
  | Node (l, k, r, h) ->
      (match cmp x k with
      | 0 -> add_helper (sum_intervals x k) l r h
      | -1 ->
        let nl = add x l in
        bal nl k r
      | 1 ->
        let nr = add x r in
        bal l k nr
      | _ ->
        add_helper (sum_intervals k x) l r h)
  | Empty -> Node (Empty, x, Empty, 1)
;;

let remove (x, y) set =
  let (l, _, _) = split x set
  and (_, _, r) = split y set 
  in 
    merge l r
;;

let iter f set =
  let rec loop = function
    | Node (l, k, r, _) -> loop l; f k; loop r
    | Empty -> ()
  in
    loop set
;;

let fold f set acc =
  let rec loop acc = function
    | Node (l, k, r, _) ->
          loop (f k (loop acc l)) r 
    | Empty -> acc
  in
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
    | Node (l, (a, b), r, _) -> 
        if b - a < 0 then max_int 
        else acc + (b - a + 1) + loop acc l + loop acc r
    | Empty -> if acc < 0 then max_int else acc
  in
    let (leq, _, _) = split (if x = max_int then x else x + 1) s in
     match leq with
     | Empty -> 0
     | _ ->
        let res = loop 0 leq
        in 
          if res = max_int || res < 0 then max_int else res
;;

