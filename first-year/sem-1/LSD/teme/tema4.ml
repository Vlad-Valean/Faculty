(* Valean Vlad 1D.03.2(6.2) tema lab 4 *)

(* EX 7 *)


let split lst =
  let rec fold_right func lst = match lst with
    | [] -> ([], [])
    | h :: t -> func h (fold_right func t)
  in fold_right (fun (x, y) (lst1, lst2) -> (x :: lst1, y :: lst2)) lst 

  
let splitList lst = ([], [])
|> List.fold_right (fun (x, y) (lst1, lst2) -> (x :: lst1, y :: lst2)) lst 


let rs = split [(1,2);(3,4);(5,6)] 
let rsList = splitList [(1,2);(3,4);(5,6)] 
;;


let combine listTuple = 
  let rec fold_right func listTuple = match listTuple with
  | ([], []) -> [] 
  | (_::_, []) -> invalid_arg "Prima lista este mai mare decat prima!!"
  | ([], _::_) -> invalid_arg "A doua lista este mai mare decat a doua!!"
  | (h1 :: t1, h2 :: t2) -> func (h1,h2) (fold_right func (t1,t2))
  in fold_right (fun tup lst -> tup :: lst) listTuple


  let rc = combine ([5; 3; 2; 1; 0], [4; 7; 6; 0; 9])  
;;

(* EX 8 *)


let partition cond lst = 
  let rec fold_right func lst = match lst with
    | [] -> ([], [])
    | h :: t -> func h (fold_right func t)
  in fold_right (fun h (t, f) -> if cond h then (h :: t, f) else (t, h :: f)) lst


let partitionList cond lst = 
  List.fold_right (fun h (t, f) -> if cond h then (h :: t, f) else (t, h :: f)) lst ([], [])

  
let rp = partition (fun x -> x mod 2 == 0) [1;2;3;4;5;6]
let rpList = partitionList (fun x -> x < 5 && x > 2) [1;2;3;4;5;6]
;;


(* EX 10 *)
 

let rec no_double lst = match lst with
| [] -> []
| head :: [] -> head :: no_double [] 
| h1 :: h2 :: [] -> if h1 = h2 
                    then h1 :: (no_double []) 
                    else h1 :: h2 :: (no_double [])
| hA :: hB :: hC :: t ->(*if hA = hB && hB = hC
                            then hA :: (no_double t)
                            else *) if hA = hB || hB = hC 
                                    then hA :: hC :: (no_double t)
                                    else hA :: hB :: hC :: (no_double t)
                            

let rnd = no_double [2;1;1;1]
;;


(* EX 12 *)

let rec merge (lst1,lst2) = 
  match (lst1,lst2) with
  | ([], []) -> [] 
  | (h1 :: t1, []) -> h1 :: merge (t1, []) 
  | ([], h2 :: t2) -> h2 :: merge ([], t2)
  | (h1 :: t1, h2 :: t2) -> if h1 < h2 
                            then h1 :: merge (t1, lst2)
                            else h2 :: merge (lst1, t2)  
  

let rm = merge ([1;3;6;9],[2;4;5;7;8])
;;


(* EX 13 *)


let spliter =
  let rec auxspliter lst =
    let firstCase h (lst1,lst2) = (h :: lst1, lst2) in
    let secondCase h1 h2 (lst1,lst2) = (h1 :: lst1, h2 :: lst2) in
    match lst with
    | [] -> ([],[])
    | h :: [] -> firstCase h (auxspliter [])
    | h1 :: h2 :: t -> secondCase h1 h2 (auxspliter t)
  in auxspliter 
  

let rsp = spliter [1;2;3;4;5;6;7;8;9]
;;
    

(* EX 14 *)


let rec mergeSort lst = 
  let split = spliter lst in
      match split with 
      | ([x], [y])   -> merge ([x], [y])
      | ([x], lst2)  -> merge ([x], mergeSort lst2)
      | (lst1, [y])  -> merge (mergeSort lst1, [y])
      | (lst1, lst2) -> merge (mergeSort lst1, mergeSort lst2)
  

let rmS = mergeSort [5;3;4;2;1;9]
;;


(* Min / Max *)


let max_list lst = 
  let head = List.hd lst in 
  List.fold_left (fun h rez -> max h rez) head lst;;


let min_list lst = 
  let head = List.hd lst in 
  List.fold_left (fun h rez -> min h rez) head lst;;


let min_max_list lst = 
  let head = List.hd lst in 
  List.fold_right
  (
    fun h (minn, maxx) ->
    let min x y = if x < y then x else y in
    let max x y = if x > y then x else y in
    let min_max x (y, z) = (min x y, max x z) in
    min_max h (minn, maxx)
  ) lst (head, head)
;; 


let rmin = min_list [2;3;1;5;2;3;8]
let rmax = max_list [2;3;1;5;2;3;8]
let rmin_max = min_max_list [2;3;1;5;2;3;8]
;;


(* Perimetru *)


let perimetru lst =
  let first = List.hd lst in
  let distance (x1,y1) (x2,y2) = 
    sqrt ((x2 -. x1) ** 2.  +. (y1 -. y2) ** 2.) in
  let rec auxPer lst (prev,restPer) = 
    match lst with 
    | [] -> restPer +. distance prev first 
    | h :: t -> auxPer t (h, restPer +. distance prev h) 
  in auxPer lst (first , 0. ) 


let perimetruList lst =
  let auxPerimetruList lst = 
    let first = List.hd lst in 
    let distance (x1,y1) (x2,y2) = 
      sqrt ((x2 -. x1) ** 2.  +. (y1 -. y2) ** 2.) in
  List.fold_right 
  (fun h (prev, restPer) ->(h, restPer +. distance prev h)) lst (first, 0.)in 
match auxPerimetruList lst with
|(_, rez) -> rez
  

let points = 
[
  (3., 3.);
  (9., 3.);
  (9., 9.);
  (3., 9.)
] (* 24 *)


let rper = perimetru points   
let rperList = perimetruList points
;;
