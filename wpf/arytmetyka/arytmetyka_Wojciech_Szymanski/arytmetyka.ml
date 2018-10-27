(*Autor: Wojciech Szymański*)
(*Recenzent: Anon Danon*)
open List;;

type przedzial = {
	lewy : float ;
	prawy : float
} ;;

type wartosc = {
	lista : przedzial list
} ;;

let nowy_przedzial x y =
	{ lewy = x; prawy = y } ;;

let nowa_wartosc x =
	{ lista = x } ;;

let nowa_wartosc_przedzial x y =
	nowa_wartosc [ nowy_przedzial x y ] ;;

let max2 a b =
	match (a > 0. || a <= 0.), (b > 0. || b <= 0.) with 
		| false, false -> nan
		| false, _ -> b
		| _, false -> a
		| _, _ -> max a b ;;

let min2 a b =
	match (a > 0. || a <= 0.), (b > 0. || b <= 0.) with 
		| false, false -> nan
		| false, _ -> b
		| _, false -> a
		| _, _ -> min a b ;;

let wartosc_dokladnosc x p =
	let w1 = x -. p /. 100. *. x in
	let w2 = x +. p /. 100. *. x in
	nowa_wartosc_przedzial (min2 w1 w2) (max2 w1 w2) ;;

let wartosc_dokladna x =
	nowa_wartosc_przedzial x x ;; 

let wartosc_od_do x y =
	nowa_wartosc_przedzial x y ;;

let czy_wewnatrz l x y =
	x || (y.prawy >= l && y.lewy <= l) ;;

let in_wartosc x y =
	fold_left (czy_wewnatrz y) false x.lista ;;

let min_wartosc x =
	let rec min_wartosc_h x =
		match x with
			| (h :: []) -> h.lewy
			| (h :: t) -> min2 h.lewy (min_wartosc_h t)
			|  _ -> nan
	in min_wartosc_h x.lista ;;

let max_wartosc x =
	let rec max_wartosc_h x =
		match x with
			| (h :: []) -> h.prawy
			| (h :: t) -> max2 h.prawy (max_wartosc_h t)
			|  _ -> nan
	in max_wartosc_h x.lista ;;

let sr_wartosc x = 
	(min_wartosc x +. max_wartosc x) /. 2. ;;

let przeciecie x1 x2 y1 y2 =
	(y1 <= x2 && y1 >= x1) || (x1 <= y2 && x1 >= y1) ;;

let rec usun_z y x =
	let usun_z_h a z =
		if przeciecie x.lewy x.prawy z.lewy z.prawy then
			a
		else
			(z :: a)
	in fold_left usun_z_h [] y ;;

let rec suma_przedzialu_z_lista x y =
	let suma_przedzialu_z_lista_h a xx =
		if przeciecie xx.lewy xx.prawy a.lewy a.prawy then
			nowy_przedzial (min2 xx.lewy a.lewy) (max2 xx.prawy a.prawy)
		else
			a
	in fold_left suma_przedzialu_z_lista_h x y ;;

let rec napraw_liste x =
	let napraw_liste_h a x =
		suma_przedzialu_z_lista x a :: usun_z a x
	in
		fold_left napraw_liste_h [] x ;;

let napraw_liczbe x =
	nowa_wartosc (napraw_liste x.lista) ;;

let dodaj_przedzial_razy l p a y =
	let max_h = max2 (l *. y.prawy) (p *. y.lewy) in
	let min_h = min2 (l *. y.prawy) (p *. y.lewy) in
	let max_h2 = max2 (p *. y.prawy) (l *. y.lewy) in
	let min_h2 = min2 (p *. y.prawy) (l *. y.lewy) in
	let lewy_h = min2 min_h min_h2 in
	let prawy_h = max2 max_h max_h2 in
		if (y.lewy = 0. && y.prawy = 0.) || (l = 0. && p = 0.) then
			nowy_przedzial 0. 0. :: a
		else
				nowy_przedzial lewy_h prawy_h :: a ;;

let dodaj_przedzial_minus l p a y =
	nowy_przedzial (l -. y.prawy) (p -. y.lewy) :: a ;;

let dodaj_przedzial_plus l p a y =
	nowy_przedzial (l +. y.lewy) (p +. y.prawy) :: a ;;

let odwroc_przedzial a x =
	let p_odw = 1. /. x.prawy in
	let l_odw = 1. /. x.lewy in
	let p_l = nowy_przedzial p_odw l_odw in
	let p_inf = nowy_przedzial p_odw infinity in
	let neginf_l = nowy_przedzial neg_infinity l_odw in
		if x.lewy *. x.prawy > 0. then
			nowa_wartosc (p_l :: a.lista)
		else
			let odwroc_przedzial_h l p =
				match l, p with
					| 0., 0. -> a.lista
					| 0., _ -> p_inf :: a.lista
					| _, 0. -> neginf_l :: a.lista
					| _, _ -> neginf_l :: p_inf :: a.lista
			in 
				nowa_wartosc (odwroc_przedzial_h x.lewy x.prawy) ;;

let odwroc_liczbe x =
	fold_left odwroc_przedzial (nowa_wartosc []) x.lista ;;

let plus_listy a b =
	let plus_listy_h ak x =
		fold_left (dodaj_przedzial_plus x.lewy x.prawy) [] b @ ak
	in
		fold_left plus_listy_h [] a ;;

let minus_listy a b =
	let minus_listy_h ak x =
		fold_left (dodaj_przedzial_minus x.lewy x.prawy) [] b @ ak
	in
		fold_left minus_listy_h [] a ;;

let razy_listy a b =
	let razy_listy_h ak x =
		fold_left (dodaj_przedzial_razy x.lewy x.prawy) [] b @ ak
	in
		fold_left razy_listy_h [] a ;;

let plus_h a b = 
	nowa_wartosc (plus_listy a.lista b.lista) ;;

let minus_h a b =
	nowa_wartosc (minus_listy a.lista b.lista) ;;

let razy_h a b = 
	nowa_wartosc (razy_listy a.lista b.lista) ;;

let razy a b = napraw_liczbe (razy_h a b) ;;

let plus a b = napraw_liczbe (plus_h a b) ;;

let minus a b = napraw_liczbe (minus_h a b) ;;

let podzielic a b =
	if b = nowa_wartosc_przedzial 0. 0. then
		nowa_wartosc []
	else
		razy a (odwroc_liczbe b) ;;

