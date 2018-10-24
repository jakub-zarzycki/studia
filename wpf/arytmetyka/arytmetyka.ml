(*Jakub Zarzycki*)
(*review: *)
(*zadanie o arytmetyce niedokładnych wartości*)
(*1/0 ma dawać zbiór pusty*)
(*nieskończoności nie należą do przedzałów*)
(*uwaga na NaN*)
(*[-1, 1] / [-1, 1] daje co trzeba i ognoruje NaN w 0/0*)
(*zakładamy,że to jest lista co najwyżej 2 elementowa*)
(*co z [(1, 2), (5, 6)] + [(9, 10), (25, 26)]?*)
(*nie spotkamy tego, nie ma takiego konstruktora*)
type wartosc = (float * float) list

(*pomocnicy*)

(*sprawdź czy jest nan*)
let is_nan (f : float) =
    f = f
;;

(*fold_left*)
let rec fold_left f l a =
    match l with
    | [] -> a
    | h::t -> fold_left f t (f h a)
;;

(*sprawdź czy liczba jest w przedziale*)
let wartosc_w_przedziale (f : float) (w : wartosc) =
    match w with
    | [] -> false
    | (x, y)::[] -> f <= y && f >= x
;;

(*suma mnogościowa przedziałów*)
let suma_przedzialow (x : wartosc) (y : wartosc) = 
    match x, y with
    | [], _ | _, [] -> []
    | (ax, bx)::[], (ay, by)::[] -> 
        if wartosc_w_przedziale ay x then [(ax, (max bx by))] else
        if wartosc_w_przedziale by x then [((min ax ay), by)]
        else (ax, bx)::y
;;

(*suma mnogościowa przedziału i wartosci*)
let suma_przedzial_wartosc (i : wartosc) (w : wartosc) =
   match w, i with
   | [], _ -> i
   | x::[], _ -> suma_przedzialow [x] i
   | x::y, [(a, b)] -> 
        if wartosc_w_przedziale a [x] then 
            (suma_przedzialow [x] i) @ y
            else x::(suma_przedzialow y i)
;;

(*suma mnogościowa dwóch wartosci*)
let suma_wartosci (x : wartosc) (y : wartosc) =
    match x with 
    | [] -> y
    | x1::[] -> suma_przedzial_wartosc x y
    | x1::x2 -> 
        suma_przedzial_wartosc x2 (suma_przedzial_wartosc [x1] y)
;;

(*podział przedziału na część dodatnią i ujemną*)
let split (w : wartosc) =
    match w with
    | (0., 0.)::[] -> []
    | (a, b)::[] ->
        if b <= 0. || a >= 0. then w
        else [(a, 0.)] @ [(0., b)]
;;

(*konstruktory*)

(*x : liczba p : %*)
let wartosc_dokladnosc (x : float) (p : float) = 
    let error = x *. p /. 100. in
        [(x -. error, x +. error)]
;;

let wartosc_od_do (x : float) (y : float) =
    [(x, y)]
;;

let wartosc_dokladna (f : float) =
    [(f, f)]
;;

(*selektory*)

let in_wartosc (w : wartosc) (f : float) = 
    match w with
    | [] -> false
    | x::[] -> wartosc_w_przedziale f [x]
    | x1::x2 -> wartosc_w_przedziale f [x1] || wartosc_w_przedziale f x2
;;

let min_wartosc w =
    match w with
    | [] -> nan
    | (a, b)::[] -> a
    | (a1, b1)::[(a2, b2)] -> min a1 a2
;;

let max_wartosc w = 
    match w with
    | [] -> nan
    | (a, b)::[] -> b
    | (a1, b1)::[(a2, b2)] -> max b1 b2
;;

let sr_wartosc w = 
    let lower = min_wartosc w and upper = max_wartosc w in 
        if lower = lower && upper = upper then
            ((lower +. upper) /. 2.) else 
            nan
;;

(*modyfikatory*)

(*suma algebraiczna przedzałów*)
let plus_przedzialow (x : wartosc) (y : wartosc) =
    match x, y with
    | [], _ | _, [] -> []
    | (ax, bx)::[], (ay, by)::[] -> 
        let az = ax +. ay and
        bz = bx +. by in
            if is_nan az || is_nan bz then [] else [(az, bz)]
;;

(*suma algebraiczna wartości (być może 2 przediały) i przedziału*)
let plus_wartosc_przedzial (w : wartosc) (i : wartosc) =
     match w, i with
     | [], _ | _, [] -> []
     | w1::[], i -> plus_przedzialow [w1] i
     | w1::[w2], i -> 
        suma_przedzialow (plus_przedzialow [w1] i) (plus_przedzialow [w2] i)
;;

(*plus*)
let plus (x : wartosc) (y : wartosc) =
    match x with
    | [] -> []
    | x1::[] -> plus_wartosc_przedzial y [x1]
    | x1::[x2] -> 
        suma_wartosci 
            (plus_wartosc_przedzial y [x1]) 
            (plus_wartosc_przedzial y [x2])
;;


assert(
let x = [(-1., 1.)] and y = [(0., 0.)] in 
    plus x y = x;
    plus x y <> y;
);;

(*mnożenie przedziałów*)
(*(-2, 5) * (-10, 1) = (-50, 20)*)
let razy_przeszialow (x : wartosc) (y : wartosc) =
    match x, y with 
    | [], _ | _, [] -> []
    | (ax, bx)::[], (ay, by)::[] ->
        let a = ax *. ay
        and b = bx *. by
        and c = ax *. by 
        and d = bx *. ay
        in
            [(min (min (min a b) c ) d, max (max (max a b) c) d)]
;;

(*mnożenie wartosc * przedział*)
let razy_wartosc_przedzial (w : wartosc) (i : wartosc) = 
    match w with
    | [] -> []
    | w1::[] -> razy_przeszialow [w1] i
    | w1::[w2] ->
        suma_przedzialow (razy_przeszialow [w1] i) (razy_przeszialow [w2] i)
;;

(*razy*)
let razy (x : wartosc) (y : wartosc) = 
    match x with
    | [] -> []
    | x1::[] -> razy_wartosc_przedzial y [x1]
    | x1::[x2] -> 
        suma_wartosci 
            (razy_wartosc_przedzial y [x1]) 
            (razy_wartosc_przedzial y [x2])
;;

(*odwrotność przedziału bez 0*)
(*(2, 3) -> (1/3, 1/2)
* (-3, -2) -> (-1/2, -1/3)*)
let odwrotnosc_bez_zera (i : wartosc) =
    match i with 
    | (0., b)::[] -> [(1. /. b, infinity)]
    | (a, 0.)::[] -> [(neg_infinity, 1. /. a)]
    | (a, b)::[] -> [(1. /. b, 1. /. a)]
;;

(*odwrotność przedziału (1/x)*)
let odwrotnosc (i : wartosc) = 
    if wartosc_w_przedziale 0. i then 
        odwrotnosc_bez_zera i else
        match (split i) with x::y::[] ->
            (odwrotnosc_bez_zera [x]) @ (odwrotnosc_bez_zera [y])
;;

(*podzielic*)
let podzielic x y = 
    razy x (odwrotnosc y)
;;

(*minus*)
let minus x y = 
    let neg_y = razy (wartosc_dokladna (-1.)) y in
        plus x neg_y
;;

