(* 
 * origami
 * pisał:  Jakub Zarzycki
 * review: Mateusz Kałużny
 *)
 
type point = float * float

(* 
 * O(2^n) od ilości złożeń
 *)
type kartka = point -> int

(*
 * różnica wektorów
 *)
let v_diff (x1, y1) (x2, y2) = (x1 -. x2, y1 -. y2);;

(*
 * iloczyn skalar wektor
 *)
let s_v_mul k (x, y) = (k *. x, k *. y);;

(* 
 * iloczyn skalarny wektorów
 *)
let scalar_p (x1, y1) (x2, y2) =
    (x1 *. x2) +. (y1 *. y2)
;;

(*
 * -1 jeśli (p1, p2) leży po dobrej stronie prostej (x1, y1) (x2, y2)
 *  0 jeśli leży na prostej
 *  1 jeśli leży po złej stronie
 *)
let sprawdz_strone (x1, y1) (x2, y2) (p1, p2) = 
    let d = (p1 -. x1) *. (y2 -. y1) -. (p2 -. y1) *. (x2 -. x1) in
        compare d 0.
;;

let norma_kwadrat (x, y) = 
    x *. x +. y *. y
;;

(* 
 * rzut wktora x na wektor p2
 *)
let rzut p2 x =
    let n = 1. /. norma_kwadrat p2
    in
        s_v_mul (n *. scalar_p p2 x) p2
;;

let move p1 p2 x =
    v_diff x p1, v_diff p2 p1
;;

(*
 * UWAGA! czasami daje błędne wyniki przez błędy zaokrągleń
 * przesuń płaszczyznę afiniczną o wektor -p1
 * odbij wektor x - p1 względem p2 - p1
 * przesuń o werktor p1
 *)
let odbicie p1 p2 x =
    if fst p1 = fst p2 && snd p1 = snd p2 then failwith "illegal input";
    let x, p2 = move p1 p2 x in
    let p = rzut p2 x 
    in
        v_diff (v_diff (s_v_mul 2. p) x) (s_v_mul (-1.) p1)
;;

let prostokat (x1, y1) (x2, y2) =
    if x1 >= x2 && y1 >= y2 then failwith "illegal input";
    
    function (x, y) -> if x <= x2 && x >= x1 && 
                          y >= y1 && y <= y2
                       then 1 else 0
;;

let kolko (x1, y1) r (x, y) =
    if r < 0. then failwith "illegal input";

    if ((x -. x1) *. (x -. x1)) +. ((y -. y1) *. (y -. y1)) <= r *. r
    then 1 else 0
;;

(* 
 * samo składanie ma koszt stały
 *)
let zloz x y f = function p ->  
    let d = sprawdz_strone x y p in
        if d = -1 then f p + f (odbicie x y p)
        else if d = 0 then f p
        else 0
;;

(*
 * O(n) od długości listy
 *)
let skladaj p_list f = 
    List.fold_left (fun f (x, y) -> zloz x y f) f p_list
;;

