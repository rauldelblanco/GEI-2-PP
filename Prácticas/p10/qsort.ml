let rec qsort1 ord = function
    [] -> []
  | h::t -> let after, before = List.partition (ord h) t in
            qsort1 ord before @ h :: qsort1 ord after;;
            
(*En casos en los que la lista no esté balanceada esta implementación no será buena*)

let rec qsort2 ord =
    let append' l1 l2 = List.rev_append (List.rev l1) l2 in
    function
       [] -> []
     | h::t -> let after, before = List.partition (ord h) t in
               append' (qsort2 ord before) (h :: qsort2 ord after);;
               
(*La función qsort2 tiene la ventaja de que se utilizan funciones recursivas terminales 
  (List.rev_append y List.rev). Además, qsort2 es más rápido que qsort1 cuando la lista se encuentra 
  inicialmente ordenada y evita el stack overflow en listas más grandes*)
  
let l1 = List.init 500 (function _ -> Random.int 1000);;

(*qsort2 es más lento que qsort1 cuando la lista está desordenada aleatoriamente y cuando lo está 
  inversamente*)
(*En los casos de prueba, la función qsort2 ha sido al rededor de un 115% más lento que qsort1*)  

