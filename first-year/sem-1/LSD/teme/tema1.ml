
open Printf


(* EX2 *)



let grad2 a b c =
   let delta = int_of_float(float_of_int b ** 2.) - 4*a*c in
    if delta == 0 then
    (
        printf "\nradacina este: ";
        print_float (float_of_int (-b) /. (-2. *. float_of_int a));
    )
    else
    (
        printf "\nradacina 1 este: ";
        print_float ((float_of_int (-b) +. sqrt(float_of_int delta)) /. (-2. *. float_of_int a));
        printf "\nradacina 2 este: ";
        print_float ((float_of_int (-b) -. sqrt(float_of_int delta)) /. (-2. *. float_of_int a))
    )
;; 



(* EX3 *)

let bisect an = 
    if an mod 400 == 0 then 
        true 
    else
        if an mod 4 == 0 && an mod 100 !=0 then
            true
        else 
            false

;;

(* EX4 *)

let valDis a b c =
    if a == b && b == c then
    (
        printf "toate argumentele sunt egale";
    )
    else
        if a == b then
        printf "argumentele 1 si 2"
        else
            if b == c then
            printf "argumentele 2 si 3"
             else
                if a == c then
                printf "argumentele 1 si 3"
                    else 
                    printf "toate argumentele sunt distincte"

        (* Nu este o rezolvare eficienta *)
;;

(* EX5 *)

(* 
stim ca a <= b asadar trebuie verificata doar relatia dintre a si c
daca a este mai mare decat c atunci a este mediana
daca c este mai mare decat a atunci verificam daca relatia dintre b si c
*)


let medianaAux a b c = 
    if c <= a then print_int a 
    else 
        if c >= b then print_int b
        else
            print_int c 

let medianaPrinc a b c =
    printf "\nmediana este: ";
    if a <= b then 
        medianaAux a b c 
    else 
        medianaAux b a c 

let testare = medianaPrinc 1 2 3
;;

(* EX6 *)


let funcc a = a + 5
let func2 b = b + 10
let func3 c = c - 7

let operatiiAdunare g a h b  = g a + h b

let operatiiInmultire g a h b = g a * h b

let operatiiGenerale f g a h x = f (g a) (h x)   


;;

