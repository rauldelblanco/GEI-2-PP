let rec divide l = match l with
    h1::h2::t -> let t1, t2 = divide t in (h1::t1, h2::t2)
  | _ -> l, [];;
  
(*let rec merge = function
    [], l | l, [] -> l
  | h1::t1, h2::t2 -> if h1 <= h2 
                      then h1 :: merge (t1, h2::t2)
                      else h2 :: merge (h1::t1, t2);;*)
                      
(*let rec msort1 l = match l with
    [] | _::[] -> l
  | _ -> let l1, l2 = divide l in
                      merge (msort1 l1, msort1 l2);;*)
                      

let rec merge ord = function
  [], l | l, [] -> l
  | h1::t1, h2::t2 -> if (ord h1) h2 
                      then h1 :: merge ord (t1, h2::t2)
                      else h2 :: merge ord (h1::t1, t2);;

let rec msort1 ord l = match l with
  [] | _::[] -> l
  | _ -> let l1, l2 = divide l 
         in merge ord (msort1 ord l1, msort1 ord l2);;
        
(*Al no ser terminales, se puede producir un Stack Overflow con facilidad, como por ejemplo: *)

(*divide (List.init 1_000_000 (function x -> x));;*)

let l2 = List.init 1_000_000 (function x -> Random.int 1000);;

let divide' l = 
    let rec aux l1 l2 l3 = match l1 with
        [] -> List.rev(l2),List.rev(l3)
      | h1::h2::t -> aux t (h1::l2) (h2::l3)
      | h::[] -> List.rev(h::l2),List.rev(l3)
    in aux l [] [];;
    
let merge' ord (l1, l2) =
  let rec aux (a1, a2) mer = match a1, a2 with
      [], l | l, [] -> List.rev_append mer l
    | h1::t1, h2::t2 -> if ord h1 h2 
                        then aux (t1, h2::t2) (h1::mer)
                        else aux (h1::t1, t2) (h2::mer)
  in aux (l1, l2) [];;
  
let rec msort2 ord l = match l with
    [] | _::[] -> l
  | _ -> let l1, l2 = divide' l
         in merge' ord (msort2 ord l1, msort2 ord l2);;
         
(*La función msort2 es más lenta que msort1 pero, mucho más rápida que qsort2. 
  A la hora de realizar las pruebas de rendimiento, vamos a utilizar una lista de 10_000 elementos.
  Durante las pruebas, he comprobado que la función msort1 ordena la lista en 0.007742, la función
  msort2 la ordena en 0.008421 y qsort2 la ordena en 2.232748*)

