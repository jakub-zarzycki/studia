(* ********************************************
*zadanie o arytmetyce niedokładnych wartości
*pisał:  Jakub Zarzycki 371722
*review: Wojciech Szymański
******************************************** *)


(*możemy reprezentować liczby jako listy przedziałów*)
(*spełniając warunki zadania można dojść do sumy co najwyżej dwóch*) 
type wartosc = (float * float) list



(*pomocnicy ogólni*)

(*sprawdź czy jest nan*)
let is_nan (f : float) =
    not (f = f)
;;

(*sprawdź czy liczba jest w przedziale*)
let wartosc_w_przedziale (w : wartosc) (f : float) =
    match w with
    | (x, y)::[] -> f <= y && f >= x
    | _ -> false
;;


(*konstruktory*)

(*wartość z błędem*)
let wartosc_dokladnosc (x : float) (p : float) = 
    let error = x *. p /. 100. in
        let a, b =  min (x -. error) (x +. error), 
                    max (x -. error) (x +. error) 
        in
            [(a,b)]
;;

(*przedział wartości*)
let wartosc_od_do (x : float) (y : float) =
    [(x, y)]
;;

(*wartość dokładna*)
let wartosc_dokladna (f : float) =
    [(f, f)]
;;



(*selektory*)

(*in wartosc*)
let in_wartosc (w : wartosc) (f : float) = 
    match w with
    | [] -> false
    | x::[] -> wartosc_w_przedziale [x] f
    | x1::x2::[] -> wartosc_w_przedziale [x1] f || wartosc_w_przedziale [x2] f
    | _ -> false
;;

(*min wartosc*)
let min_wartosc (x : wartosc) =
    match x with
    | [] -> nan
    | (a, b)::[] -> min a b
    | (a1, b1)::(a2, b2)::[] -> min a1 a2
    | _ -> nan
;;

(*max wartosc*)
let max_wartosc (x : wartosc) = 
    match x with
    | [] -> nan
    | (a, b)::[] -> max a b
    | (a1, b1)::(a2, b2)::[] -> max b1 b2
    | _ -> nan
;;

(*średnia wartosc*)
let sr_wartosc w = 
    let lower = min_wartosc w and upper = max_wartosc w in 
        if (lower = lower) && (upper = upper) 
        then
            ((lower +. upper) /. 2.) 
        else 
            nan
;;



(*pomocnicy do modyfikatorów*)

(*utrzymaj porządek przedziałów*)
let czysc (x : wartosc) =
    match x with
    | [] -> []
    | x1::[] -> [x1]
    | x1::x2::[] ->
        if (min_wartosc [x1]) < (min_wartosc [x2]) 
        then 
            x
        else 
            x2::x1::[]
    | _ -> []
;;

(*podział przedziału na część dodatnią i ujemną*)
let split (x : wartosc) =
    match x with
    | [] -> []
    | (a, b)::[] ->
        if b <= 0. || a >= 0. 
        then 
            x
        else 
            [(a, 0.)] @ [(0., b)]
    | _ -> []
;;

(*przedział przeciwny*)
let minus_przedzial (x : wartosc) =
    match x with
    | [] -> []
    | (ax, bx)::[] -> (-. bx, -. ax)::[]
    | _ -> []
;;

(*wartość przeciwna*)
let minus_wartosc (x : wartosc) =
    match x with
    | [] -> []
    | x1::[] -> minus_przedzial [x1]
    | x1::x2::[] -> czysc ((minus_przedzial [x1]) @ (minus_przedzial [x2]))
    | _ ->[]

(*suma mnogościowa przedziałów*)
let suma_przedzialow (x : wartosc) (y : wartosc) = 
    match x, y with
    | [], y -> y
    | x, [] -> x
    | x, y -> 
        if wartosc_w_przedziale y (max_wartosc x) || 
           wartosc_w_przedziale y (min_wartosc x) 
        then
                [(min (min_wartosc x) (min_wartosc y),
                 max (max_wartosc x) (max_wartosc y))]
        else
            x @ y
;;

(*suma mnogościowa przedziału i wartosci*)
let suma_przedzial_wartosc (i : wartosc) (w : wartosc) =
    match w, i with
    | [], i -> i
    | w1::[], i -> suma_przedzialow [w1] i
    | w1::w2::[], (a, b)::[] -> 
        if wartosc_w_przedziale [w1] a 
        then 
            (suma_przedzialow [w1] i) @ [w2]
        else
            w1::(suma_przedzialow [w2] i)
    | _ -> []
;;

(*suma mnogościowa dwóch wartosci*)
let suma_wartosci (x : wartosc) (y : wartosc) =
    match x with 
    | [] -> y
    | x1::[] -> suma_przedzial_wartosc x y
    | x1::x2::[] -> 
        suma_przedzial_wartosc [x2] (suma_przedzial_wartosc [x1] y)
    | _ -> []
;;


(*suma algebraiczna przedzałów*)
let plus_przedzialow (x : wartosc) (y : wartosc) =
    match x, y with
    | [], _ | _, [] -> []
    | (ax, bx)::[], (ay, by)::[] -> 
        let az = ax +. ay 
        and bz = bx +. by in
            if is_nan az || is_nan bz then [] else [(az, bz)]
    | _ -> []
;;

(*suma algebraiczna wartości (być może 2 przediały) i przedziału*)
let plus_wartosc_przedzial (w : wartosc) (i : wartosc) =
    match w, i with
    | [], _ | _, [] -> []
    | w1::[], i -> plus_przedzialow [w1] i
    | w1::[w2], i -> 
        suma_przedzialow (plus_przedzialow [w1] i) (plus_przedzialow [w2] i)
    | _ -> []
;;

(*mnożenie przedziałów o elementach >= 0.*)
let razy_nieujemnych_przedzialow (x : wartosc) (y : wartosc) =
    match x, y with
    | [], _ | _, [] -> []
    | (ax, bx)::[], (ay, by)::[] -> 
        (match (is_nan (ax *. ay)), (is_nan (bx *. by)) with
        | false, false -> [(ax *. ay, bx *. by)]
        | false, true -> [(ax *. ay, 0.)]
        | true, false -> [(0., bx *. by)]
        | true, true -> [(0., 0.)])
    | _ -> []
;;

(*mnożenie przedziałów niezawierających 0. wewnątrz, może być na brzegu*)
let razy_bez_zera (x : wartosc) (y : wartosc) = 

    (*x leży poniżej 0.*)
    if (min_wartosc x <= 0. && max_wartosc x <= 0.) then
        (*y leży poniżej 0.*)
        if min_wartosc y <= 0. && max_wartosc y <= 0. then       
            razy_nieujemnych_przedzialow 
                (minus_przedzial x)
                (minus_przedzial y)
        (*x ujemny, y dodatni*)
        else
            minus_przedzial (razy_nieujemnych_przedzialow 
                                            (minus_przedzial x) y)
    (*x dodatni*)
    else
        (*x dodatni, y ujemny*)
        if min_wartosc y <= 0. && max_wartosc y <= 0. then       
            minus_przedzial (razy_nieujemnych_przedzialow 
                                            x (minus_przedzial y))
        (*x dodatni, y dodatni*)
        else
            razy_nieujemnych_przedzialow x y
;;

(*mnożenie przedziałów*)
let razy_przedzialow (x : wartosc) (y : wartosc) =
    match x, y with 
    | [], _ | _, [] -> []
    | x, y ->
        (*podział przedziału na część dodatnią i ujemną*)
        (*i mnożenie przedziałów o stałym znaku*)
        match (split x), (split y) with 
        | _, [] | [],_ -> []
        | x1::[], y1::[] -> razy_bez_zera [x1] [y1]
        | x1::x2::[], y1::[] -> 
            suma_przedzialow
                (razy_bez_zera [x1] [y1])
                (razy_bez_zera [x2] [y1])
        | x1::[], y1::y2::[] ->
            suma_przedzialow
                (razy_bez_zera [x1] [y1])
                (razy_bez_zera [x1] [y2])
        | x1::x2::[], y1::y2::[] ->
            suma_wartosci
                (suma_wartosci
                    (razy_bez_zera [x1] [y1])
                    (razy_bez_zera [x1] [y2]))
                
                (suma_wartosci
                    (razy_bez_zera [x2] [y1])
                    (razy_bez_zera [x2] [y2]))
        | _ -> []
;;

(*mnożenie wartosc *. przedział*)
let razy_wartosc_przedzial (w : wartosc) (i : wartosc) = 
    match w with
    | [] -> []
    | w1::[] -> razy_przedzialow [w1] i
    | w1::[w2] ->
        suma_przedzialow (razy_przedzialow [w1] i) (razy_przedzialow [w2] i)
    | _ -> []
;;

(*odwrotność przedziału bez 0. w środku, może być na brzegu*)
let odwrotnosc_bez_zera (i : wartosc) =
    match i with 
    | [] -> []
    | (0., b)::[] -> if b <> 0. then [(1. /. b, infinity)] else []
    | (a, 0.)::[] -> if a <> 0. then [(neg_infinity, 1. /. a)] else []
    | (a, b)::[] -> [(1. /. b, 1. /. a)]
    | _ -> []
;;

(*odwrotność przedziału (i -> 1/i)*)
let odwrotnosc_przedzialu (i : wartosc) = 
    if not (wartosc_w_przedziale i 0.) then 
        odwrotnosc_bez_zera i 
    else
        match (split i) with 
        | [] -> []
        | x::[] -> odwrotnosc_bez_zera [x]
        | x::y::[] ->
            suma_przedzialow (odwrotnosc_bez_zera [x]) (odwrotnosc_bez_zera [y])
        | _ -> []
;;

(*odwrotnosc wartosci*)
let odwrotnosc (x : wartosc) =
    match x with
    | [] -> []
    | x1::[] -> odwrotnosc_przedzialu [x1]
    | x1::x2::[] ->
        czysc (suma_wartosci 
                (odwrotnosc_przedzialu [x1])
                (odwrotnosc_przedzialu [x2]))
    | _ -> []
;;



(*modyfikatory*)

let plus (x : wartosc) (y : wartosc) =
    match x with
    | [] -> []
    | x1::[] -> czysc (plus_wartosc_przedzial y [x1])
    | x1::x2::[] -> 
        czysc (suma_wartosci 
                    (plus_wartosc_przedzial y [x1]) 
                    (plus_wartosc_przedzial y [x2]))
    | _ -> []
;;

let minus x y = 
    czysc (plus x (minus_wartosc y))
;;

let razy (x : wartosc) (y : wartosc) = 
    match x with
    | [] -> []
    | x1::[] -> czysc (razy_wartosc_przedzial y [x1])
    | x1::[x2] -> 
        czysc (suma_wartosci 
                    (razy_wartosc_przedzial y [x1]) 
                    (razy_wartosc_przedzial y [x2]))
    | _ -> []
;;

let podzielic x y = 
    czysc (razy x (odwrotnosc y))
;;


(*testy*)
let a = wartosc_od_do (-1.) 1.            (* <-1, 1> *)
let b = wartosc_dokladna (-1.)            (* <-1, -1> *)
let c = podzielic b a                     (* (-inf -1> U <1 inf) *)
let d = plus c a                          (* (-inf, inf) *)
let e = wartosc_dokladna 0.               (* <0, 0> *)
let f = razy c e                          (* <0, 0> *)
let g = razy d e                          (* <0, 0> *)
let h = wartosc_dokladnosc (-10.) 50.     (* <-15, -5> *)
let i = podzielic h e                     (* nan, przedzial pusty*)
let j = wartosc_od_do (-6.) 5.            (* <-6, 5> *)
let k = razy j j                          (* <-30, 36> *)
let l = plus a b                          (* <-2, 0> *)
let m = razy b l                          (* <0, 2> *)
let n = podzielic l l                     (* <0, inf) *)
let o = podzielic l m                     (* (-inf, 0) *)
let p = razy o a                          (* (-inf, inf) *)
let q = plus n o                          (* (-inf, inf) *)
let r = minus n n                         (* (-inf, inf) *)
let s = wartosc_dokladnosc (-0.0001) 100. (* <-0.0002, 0> *)
let t = razy n s;;                        (* (-inf, 0) *)

assert ((min_wartosc c, max_wartosc c) = (neg_infinity, infinity));
assert (is_nan (sr_wartosc c) );
assert (not (in_wartosc c 0.));
assert ((in_wartosc c (-1.)) && (in_wartosc c (-100000.)) && (in_wartosc c 1.) && (in_wartosc c 100000.));
assert ((in_wartosc d 0.) && (in_wartosc d (-1.)) && (in_wartosc d (-100000.)) && (in_wartosc d 1.) && (in_wartosc d 100000.));
assert ((min_wartosc f, max_wartosc f, sr_wartosc f) = (0., 0., 0.));
assert ((min_wartosc g, max_wartosc g, sr_wartosc g) = (0., 0., 0.));
assert ((min_wartosc h, max_wartosc h, sr_wartosc h) = (-15., -5., -10.));
assert (is_nan (min_wartosc i) && is_nan (sr_wartosc i) && is_nan (max_wartosc i));
assert ((min_wartosc k, max_wartosc k, sr_wartosc k) = (-30., 36., 3.));
assert ((min_wartosc n, max_wartosc n, sr_wartosc n) = (0., infinity, infinity));
assert ((min_wartosc o, max_wartosc o, sr_wartosc o) = (neg_infinity, 0., neg_infinity));
assert ((min_wartosc p, max_wartosc p, is_nan (sr_wartosc p)) = (neg_infinity, infinity, true));
assert ((min_wartosc q, max_wartosc q, is_nan (sr_wartosc q)) = (neg_infinity, infinity, true));
assert ((min_wartosc r, max_wartosc r, is_nan (sr_wartosc r)) = (neg_infinity, infinity, true));
assert ((min_wartosc t, max_wartosc t, sr_wartosc t) = (neg_infinity, 0., neg_infinity));;

let a = wartosc_od_do neg_infinity infinity
let c = plus a a
let d = razy a a
let e = podzielic a a
let f = minus a a;;
assert ((min_wartosc c, max_wartosc c, is_nan (sr_wartosc c)) = (neg_infinity, infinity, true));
assert ((min_wartosc d, max_wartosc d, is_nan (sr_wartosc d)) = (neg_infinity, infinity, true));
assert ((min_wartosc e, max_wartosc e, is_nan (sr_wartosc e)) = (neg_infinity, infinity, true));
assert ((min_wartosc d, max_wartosc d, is_nan (sr_wartosc d)) = (neg_infinity, infinity, true));;

let a = wartosc_od_do 0. infinity
let b = wartosc_dokladna 0.
let c = podzielic a b
let d = podzielic b b;;
assert ((is_nan(min_wartosc c), is_nan(max_wartosc c), is_nan (sr_wartosc c)) = (true, true, true));
assert ((is_nan(min_wartosc d), is_nan(max_wartosc d), is_nan (sr_wartosc d)) = (true, true, true));;

let a = wartosc_od_do (-10.) 10.
let b = wartosc_od_do (-1.) 1000.
let c = podzielic a b;;
assert ((min_wartosc c, max_wartosc c, is_nan (sr_wartosc c)) = (neg_infinity, infinity, true));;

let a = wartosc_od_do (-1.0) 1.0
let b = wartosc_dokladna 1.0
let c = podzielic b a
let d = wartosc_dokladna 3.0
let e = plus c d      (* (-inf, 2> U <4 inf) *)
let f = podzielic b e (* (-inf, 1/4> U <1/2, inf) *)
let g = podzielic d a (* (-inf, -3> U <3, inf) *)
let h = podzielic g f (* (-inf, inf *)
let i = plus f g;;    (* (-inf, inf) *)

assert ((in_wartosc f 0.25, in_wartosc f 0.26, in_wartosc f 0.49, in_wartosc f 0.50)=(true, false, false, true));
assert ((min_wartosc h, max_wartosc h, is_nan (sr_wartosc h), in_wartosc h 0.) = (neg_infinity, infinity, true, true));
assert ((min_wartosc h, max_wartosc h, is_nan (sr_wartosc h), in_wartosc h 0.3) = (neg_infinity, infinity, true, true));;

let jed = wartosc_dokladna 1.
let zero = wartosc_dokladna 0.;;
assert ((sr_wartosc zero, max_wartosc zero, min_wartosc zero) = (0.,0.,0.));;

let a = wartosc_od_do 0. 1. (* <0,1> *)
let b = podzielic a a       (* <0, inf)*)
let c = razy b zero;;       (* <0,0> *)
assert ((sr_wartosc c, max_wartosc c, min_wartosc c) = (0.,0.,0.));;

let a = podzielic jed zero;; (* nan *)
assert (is_nan (min_wartosc a));
assert (is_nan (max_wartosc a));
assert (is_nan (sr_wartosc a));;

let a = wartosc_dokladnosc 1. 110.;; (* <-0.1, 2.1> *)
assert (in_wartosc a (-.0.1));
assert (in_wartosc a (2.1));;

let a = wartosc_od_do (-.3.) 0.  (* <-3.0, 0.0> *)
let b = wartosc_od_do 0. 1.      (* <-0.0, 1.0> *)
let c = podzielic a b;;          (* (-inf, 0> *)
assert (max_wartosc c = 0.);
assert (min_wartosc c = neg_infinity);
assert (sr_wartosc c = neg_infinity);;

let a = wartosc_od_do 1. 4.     (* <1.0, 4.0> *)
let b = wartosc_od_do (-.2.) 3. (* <-2.0, 3.0> *)
let c = podzielic a b           (* (-inf, -1/2> U <1/3, inf) *)
let d = podzielic c b           (* (-inf, -1/6> U <1/9, inf) *)
let e = plus d jed              (* (-inf, 5/6> U <10/9, inf) *)
let f = sr_wartosc (podzielic jed (wartosc_dokladna 9.));; (* 1/9 *)
assert (is_nan (sr_wartosc d));
assert (in_wartosc d 0.12);
assert (not (in_wartosc d 0.));
assert (not (in_wartosc d (-0.125)));
assert (in_wartosc d f);
assert (not (in_wartosc e 1.));;

