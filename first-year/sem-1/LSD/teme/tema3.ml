(* Valean Vlad 1D.03.2(6.2) tema lab 3 *)


(* EX1 *)
(* a *)

let filterImp n =
  let rec auxImp n t = 
    let h = n mod 10 in 
    match n with
    | 0 -> t
    | _ -> if h mod 2 = 1 
           then auxImp (n/10) (h :: t) 
           else auxImp (n/10) t
in auxImp n []

(* afisare inversa *)

let filterImpInv =
  let rec auxImpInv n = 
    let h = n mod 10 in 
    match n with
    | 0 -> []
    | _ -> if h mod 2 = 1 
           then h :: auxImpInv (n/10) 
           else auxImpInv (n/10) 
in auxImpInv
;;

(* b *)

let myCond1 x =
  if x < 7 then true
    else 
      false

let myCond2 x =
  if x > 3 then true
    else 
      false

let myCond3 x =
  if x mod 2 = 0 then true
    else 
      false

let filterGen n cond=
  let rec auxGen n cond t =
    let h = n mod 10 in 
    match n with
    | 0 -> t
    | _ -> if cond h 
           then auxGen (n / 10) cond (h :: t) 
           else auxGen (n / 10) cond t
in auxGen n cond [] 
;;

(* c *)

let listToNumber li cond =
  let rec auxLTN li cond nr = match li with
  | [] -> nr
  | h :: t -> if cond h
        then auxLTN t cond (nr * 10 + h)
        else auxLTN t cond nr
in auxLTN li cond 0
;;

(* EX2 *)

let intervalDiv =
  let rec auxDiv x y d  =
    if x <= y  then
      ( 
        if x mod d = 0 then x :: auxDiv (x+1) y d
        else auxDiv (x+1) y d
      )
    else [] 
  in auxDiv     
;;

(* invers 4 fun *)
let intervalDivInv =
  let rec auxDivInv li x y d  =
    if x <= y  then
      (
        if x mod d = 0 then  auxDivInv (x :: li) (x+1) y d
        else auxDivInv li (x+1) y d
      )
    else li
  in auxDivInv []
;;

(* EX3 *)
(* a   conventie aleasa: primul element din lista este pe pozitia 1 *)

let rec nth li n = 
match li with
| [] -> failwith "numar prea mare"
| h :: t -> match n with
            | 0 -> invalid_arg "conventia aleasa este n >= 1"
            | 1 -> h
            | _ -> if n < 0 then invalid_arg "numar negativ"
                   else nth t (n-1)
;;

(* b   conventie aleasa: primul element din lista este pe pozitia 1 *)

let rec firstn li n =
  match li with
| [] -> failwith "numar prea mare"
| h :: t -> match n with
            | 0 -> []
            | _ -> if n < 0 then invalid_arg "numar negativ"
                   else h :: firstn t (n-1)
;;

(* EX5 *)
(* a *)

let filter func li = 
  let parametruFunctie el rez = 
    if func el then el :: rez 
      else rez in
  List.fold_right parametruFunctie li []
;;

(* EX6 *)
(* a *)

let countIf f li =
  let rec auxCountIf f li counter = 
    let cond h t =
      if f h  then auxCountIf f t (counter + 1)
              else auxCountIf f t counter
  in
  match li with
  | [] -> counter
  | h :: t -> cond h t 
in auxCountIf f li 0
;;

(* b *)

let sumIf f li =
  let rec auxSumIf f li sum = 
    let cond h t = match f h with
    | true -> auxSumIf f t (sum + h)
    | false -> auxSumIf f t sum
  in
  match li with
  | [] -> sum
  | h :: t -> cond h t
  in auxSumIf f li 0
;;