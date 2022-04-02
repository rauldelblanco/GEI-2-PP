open G_tree;;

let rec breadth_first = function
    Gt (x, []) -> [x]
  | Gt (x, (Gt (y, t2))::t1) -> x :: breadth_first (Gt (y, t1@t2));;

let breadth_first_t arbol = 
    let rec aux a l = match a with
        Gt (x, []) -> List.rev(x::l)
      | Gt (x,(Gt(y,t2))::t1) -> aux (Gt(y,List.rev_append (List.rev t1) t2)) (x::l)
    in aux arbol [];;
    
(*La primera definición de breadth_first falla con listas de muchos elementos, dando un Stack Overflow*)

let t = let rec aux a n =
            if n <= 0 
            then a
            else aux (Gt(n, [a])) (n - 1)
        in aux (Gt(150_000, [])) 150_000;;
