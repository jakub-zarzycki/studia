(*Jakub Zarzycki*)
(*zadanie o arytmetyce niedokładnych wartości*)
(*1/0 ma dawać zbiór pusty*)
(*nieskończoności nie należą do przedzałów*)
(*uwaga na NaN*)
(*[-1, 1] / [-1, 1] daje co trzeba i ognoruje NaN w 0/0*)
type wartosc = float * float list

(*posortuj przedziałyrosnąco, zlikwiduj zachodzące na siebie*)
let czysc_wartosc w =
    w
;;

(*x : wartosc p : %*)
let wartosc_dokladnosc x p = 
    let error = x *. p /. 100 in
        [(x - error, x + error)]
;;

let wartosc_od_do x y =
    [(x, y)]
;;

let wartosc_dokladna x =
    [(x, x)]
;;

let in_wartosc w x = 
    let rec helper w x b =
        match w with
        | [] -> b
        | h::t ->
            match h with 
            | lower, upper -> if x >= lower && x <= upper then true else helper t x false
    in
        helper w x false
;;

let min_wartosc w =
    match w with
    | [] -> nan
    | h::t -> 
        let rec helper w lower, upper -> lower
;;


