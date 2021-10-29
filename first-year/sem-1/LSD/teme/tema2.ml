
(* TEMA LAB 2 *)
(* VALEAN VLAD 2D.03.2 *)

(* EX3 *)

(* Tail recursive *)

let cmmdc a b =
  let rec auxcmmdc a b p=
    if b = 0 
      then p
      else auxcmmdc b (a mod b) b
      in auxcmmdc a b 1
  ;;



(* EX5 *)

(* Recursive *)


let rec sumCif x =
  if x = 0 then 0
  else
      sumCif (x/10) + x mod 10

let rec nrCif x =
  if x = 0 then 0
  else
      nrCif (x/10) + 1


let rec prodCif x =
  if x = 0 then 0
  else
      prodCif (x/10) * (x mod 10)

      
let rec maxCif x =
  if x = 0 then 0
  else
      if x mod 10 > maxCif (x/10) then x mod 10
        else maxCif(x/10)


let rec minCif x =
  if x = 0 then 9
  else
      if x mod 10 < minCif (x/10) then x mod 10
        else minCif(x/10) 


(* Tail recursive *)


let sumCifTail x =
  let rec auxSum x r =
    if x = 0 
    then  r
    else auxSum (x / 10) ( r + (x mod 10))
    in auxSum x 0
  

let nrCifTail x =
  let rec auxNr x r =
    if x = 0 
    then  r
    else auxNr (x / 10) (r + 1)
    in auxNr x 0


let prodCifTail x =
  let rec auxProd x r =
    if x = 0 
    then  r
    else auxProd (x / 10) (r * (x mod 10))
    in auxProd x 1
  

let maxCifTail x =
  let rec auxMaxCifTail x maxx =
    if x = 0 then maxx
    else
      if maxx > x mod 10 then auxMaxCifTail (x/10) maxx
      else
        auxMaxCifTail (x/10) (x mod 10)
  in auxMaxCifTail x (-1)

let minCifTail x =
  let rec auxMinCifTail x minn = 
    if x = 0 then minn
    else
      if minn < x mod 10 then auxMinCifTail (x/10) minn
      else
        auxMinCifTail (x/10) (x mod 10)
  in auxMinCifTail x 9



(* EX9 *)

(* Recursive *)

let serie x =
  let rec auxserie x n a_n =
    if a_n < Float.epsilon then 0.
    else let urm = auxserie x (n +. 1.) (a_n *. (x /. n)); in urm +. a_n;  
  in auxserie x 1. 1.

(* Tail recursive *)

let serieTail x =
  let rec auxserieTail x n a_n e =
    if a_n < Float.epsilon then e
    else auxserieTail x (n +. 1.) (a_n *. (x /. n)) (e +. a_n);
  in auxserieTail x 1. 1. 0.



(* FIBONACCI *)

(* Recursive *)

(* am luat cifra 0 ca termenul 1 al sirului*)

let fibo n = 
  let rec auxfibo n pre1 pre2=
    if n = 1 then 0
    else (pre2 - pre1) + auxfibo (n - 1)  pre2 (pre1 + pre2) 
in auxfibo n 0 1

(* Tail recursive *)


let fiboTail n =
  let rec auxfiboTail n pre1 pre2 =
    if n = 1 then pre1
    else auxfiboTail (n - 1) pre2 (pre1 + pre2);
  in auxfiboTail n 0 1

;;