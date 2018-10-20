(*Jakub Zarzycki*)
(*review: *)
(*zadanie o arytmetyce niedokładnych wartości*)
(*1/0 ma dawać zbiór pusty*)
(*nieskończoności nie należą do przedzałów*)
(*uwaga na NaN*)
(*[-1, 1] / [-1, 1] daje co trzeba i ognoruje NaN w 0/0*)
(*zakładamy,że to jest lista co najwyżej 2 elementowa*)
(*co z [(1, 2), (5, 6)] + [(9, 10), (25, 26)]?*)
type wartosc = float * float list

(*posortuj przedziałyrosnąco, zlikwiduj zachodzące na siebie*)
let czysc_wartosc w =
    w
;;


(*konstruktory*)

(*x : wartosc p : %*)
let wartosc_dokladnosc x p = 
    let error = x *. p /. 100. in
        [(x -. error, x +. error)]
;;

let wartosc_od_do x y =
    [(x, y)]
;;

let wartosc_dokladna x =
    [(x, x)]
;;

(*selektory*)

let in_wartosc w x = 
    let rec helper w x =
        match w with
        | [] -> false
        | (lower, upper)::t -> if x >= lower && x <= upper then true else helper t x
    in
        helper w x
;;

let min_wartosc w =
    match w with
    | [] -> nan
    | (lower, upper)::t -> lower;;
;;

let max_wartosc w = 
    match w with
    | [] -> nan
    | (lower, upper)::t -> 
        match t with
        | [] -> upper
        | (_, upper)::t -> upper
;;

let sr_wartosc w = 
    let lower = min_wartosc w and upper = max_wartosc w in 
        if lower = lower && upper = upper then ((lower +. upper) /. 2.) else nan
;;

(*modyfikatory*)

(*x + y*)
(*TODO: trzeba przejśćwszystkie pary par, nie tylko przekątną*)
let plus x y =
    let rec helper x y suma =
        match x, y with
        | _, [] | [], _ -> suma
        | (ax, bx)::tx, (ay, by)::ty ->
            helper tx ty ((ax +. ay, bx +. by)::suma)
    in
        czysc_wartosc (helper x y [])
;;

(*x - y*)
(*trzeba podmienić kolejność w parze*)
let minus x y = 
    let rec helper x y roznica = 
        match x, y with
        |  _, [] | [], _ -> roznica
        |  
;;

