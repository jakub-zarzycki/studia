(* ********************************************
*zadanie o arytmetyce niedokładnych wartości
*pisał: Jakub Zarzycki
*review: 
******************************************** *)
type wartosc = (float * float) list

(*pomocnicy*)

(*sprawdź czy jest nan*)
let is_nan (f : float) =
    not (f = f)
;;

(*sprawdź czy liczba jest w przedziale*)
let wartosc_w_przedziale (w : wartosc) (f : float) =
    match w with
    | [] -> false
    | (x, y)::[] -> f <= y && f >= x
;;

(*podział przedziału na część dodatnią i ujemną*)
let split (w : wartosc) =
    match w with
    | (0., 0.)::[] -> []
    | (a, b)::[] ->
        if b <= 0. || a >= 0. then w
        else [(a, 0.)] @ [(0., b)]
;;

(*przedział przeciwny*)
let minus_przedzial (x : wartosc) =
    match x with
    | [] -> []
    | (ax, bx)::[] -> (-. bx, -. ax)::[]
;;

(*konstruktory*)

(*wartość z błędem*)
let wartosc_dokladnosc (x : float) (p : float) = 
    let error = x *. p /. 100. in
        [(x -. error, x +. error)]
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

(*inwartosc*)
let in_wartosc (w : wartosc) (f : float) = 
    match w with
    | [] -> false
    | x::[] -> wartosc_w_przedziale [x] f
    | x1::x2::[] -> wartosc_w_przedziale [x1] f || wartosc_w_przedziale [x2] f
;;

(*min_wartosc*)
let min_wartosc w =
    match w with
    | [] -> nan
    | (a, b)::[] -> a
    | (a1, b1)::(a2, b2)::[] -> min a1 a2
;;

(*max_wartosc*)
let max_wartosc w = 
    match w with
    | [] -> nan
    | (a, b)::[] -> b
    | (a1, b1)::(a2, b2)::[] -> max b1 b2
;;

(*sr_wartosc*)
let sr_wartosc w = 
    let lower = min_wartosc w and upper = max_wartosc w in 
        if lower = lower && upper = upper then
            ((lower +. upper) /. 2.) else 
            nan
;;

(*modyfikatory*)

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
   match w with
   | [] -> i
   | w1::[] -> suma_przedzialow [w1] i
   | w1::w2::[] ->
        match i with (a, b)::[] -> 
            if wartosc_w_przedziale [w1] a then 
                (suma_przedzialow [w1] i) @ [w2]
                else w1::(suma_przedzialow [w2] i)
;;

(*suma mnogościowa dwóch wartosci*)
let suma_wartosci (x : wartosc) (y : wartosc) =
    match x with 
    | [] -> y
    | x1::[] -> suma_przedzial_wartosc x y
    | x1::x2 -> 
        suma_przedzial_wartosc x2 (suma_przedzial_wartosc [x1] y)
;;


(*suma algebraiczna przedzałów*)
let plus_przedzialow (x : wartosc) (y : wartosc) =
    match x, y with
    | [], _ | _, [] -> []
    | (ax, bx)::[], (ay, by)::[] -> 
        let az = ax +. ay 
        and bz = bx +. by in
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
    | x1::x2::[] -> 
        suma_wartosci 
            (plus_wartosc_przedzial y [x1]) 
            (plus_wartosc_przedzial y [x2])
;;

(*mnożenie przedziałów o elementach >= 0.*)
let razy_nieujemnych_przedzialow (x : wartosc) (y : wartosc) =
    match x, y with
    | [], _ | _, [] -> []
    | (ax, bx)::[], (ay, by)::[] -> [(ax *. ay, bx *. by)]
;;

(*mnożenie przedziałów niezawierających 0.*)
let mnozenie_bez_zera (x : wartosc) (y : wartosc) = 

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
        match (split x), (split y) with 
        | _, [] | [],_ -> []
        | x1::[], y1::[] -> mnozenie_bez_zera [x1] [y1]
        | x1::x2::[], y1::[] -> 
            suma_przedzialow
                (mnozenie_bez_zera [x1] [y1])
                (mnozenie_bez_zera [x1] [y1])
        | x1::[], y1::y2::[] ->
            suma_przedzialow
                (mnozenie_bez_zera [x1] [y1])
                (mnozenie_bez_zera [x1] [y2])
        | x1::x2::[], y1::y2::[] ->
            suma_wartosci
                (suma_wartosci
                    (mnozenie_bez_zera [x1] [y1])
                    (mnozenie_bez_zera [x1] [y2]))
                
                (suma_wartosci
                    (mnozenie_bez_zera [x2] [y1])
                    (mnozenie_bez_zera [x2] [y2]))
;;

(*mnożenie wartosc * przedział*)
let razy_wartosc_przedzial (w : wartosc) (i : wartosc) = 
    match w with
    | [] -> []
    | w1::[] -> razy_przedzialow [w1] i
    | w1::[w2] ->
        suma_przedzialow (razy_przedzialow [w1] i) (razy_przedzialow [w2] i)
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

(*odwrotność przedziału bez 0.*)
let odwrotnosc_bez_zera (i : wartosc) =
    match i with 
    | [] -> []
    | (0., b)::[] -> [(1. /. b, infinity)]
    | (a, 0.)::[] -> [(neg_infinity, 1. /. a)]
    | (a, b)::[] -> [(1. /. b, 1. /. a)]
;;

(*odwrotność przedziału (i -> 1/i)*)
let odwrotnosc_przedzialu (i : wartosc) = 
    if not (wartosc_w_przedziale i 0.) then 
        odwrotnosc_bez_zera i else
        match (split i) with x::y::[] ->
            suma_przedzialow (odwrotnosc_bez_zera [x]) (odwrotnosc_bez_zera [y])
;;

(*podzielic*)
let podzielic x y = 
    razy x (odwrotnosc_przedzialu y)
;;

(*minus*)
let minus x y = 
    plus x (minus_przedzial y)
;;

