exception Cykliczne

open PMap
(***funkcja, ktora tworzy przedstawienie zadanego grafu jako mapa*)

let rec rob_graf lista graf =
  let f a (b, c) =
    if mem b a
    then let listpom = fst (find b a) in add b ((c@listpom), ref 0) a
    else add b (c, ref 0) a
  in
  List.fold_left f graf lista;;

(*dfs*)
let rec dfs (a, (b, c)) graf stos =
  if ((!c) = 1) then raise Cykliczne else
  if ((!c) = 0) then
  begin
    c := 1;
    let f  h =
      if mem h (!graf) then
      dfs (h, find h (!graf)) graf stos
      else
      begin
        graf := add h ([], ref 2) (!graf);
    (***jezeli czegos nie ma w liscie,
    ale jest sasiadem czegos z listy,
    to trzeba to dodac do grafu*)
        dfs (h, ([], ref 0)) graf stos;
      end
    in
    List.iter f b; (*wywolanie dfsa na b*)
    c := 2;
    Stack.push a stos;
  end;;

(***funkcja wywolujaca dfsa na wierzcholkach
nieprzetworzonych*)

let dfspom a (b, c) (graf, stos) =
  if !c = 0 then dfs (a, (b, c)) graf stos;
  (graf, stos);;

let topol lista =
  let graf = ref (rob_graf lista empty) in
  let lis = ref [] in
  let stos = Stack.create() in
  let _ = foldi dfspom (!graf) (graf, stos) in
  while not (Stack.is_empty stos)
  do
    lis := (Stack.pop stos)::(!lis);
  done;
  List.rev (!lis);;

