let to0from n = 
    let rec aux n l = 
        if n < 0
        then l
        else aux (n-1) (n::l)
    in aux n [];;
    
let fromto m n =
  let rec aux n l =
      if n < m
      then l
      else aux (n - 1) (n::l)
  in aux n [];;
  
let from1to n =
   let rec aux n l =
       if n < 1
       then l
       else aux (n - 1) (n::l)
   in aux n [];;
   
let map f l = 
   let rec aux l1 l2 = match l1 with 
       [] -> List.rev(l2)
     | h::t -> aux t (f(h)::l2)
   in aux l [];;
   
let power x y = 
    let rec innerpower x y z= 
        if y = 0 
        then 1
        else if y > 0
             then innerpower x (y-1) x*x
             else invalid_arg "power"
    in innerpower x y 1;;
    
let incseg l = 
    let rec aux l a l2 = match l with
        [] -> []
      | [h] -> List.rev ((h + a)::l2)
      | h::t -> aux t (h + a) ((h + a)::l2)
    in aux l 0 [];;
  
let remove x l = 
    let rec aux x l l2 = match l with
        [] -> l2
      | h::t -> if x = h
                then List.rev_append l2 t
                else aux x t (h::l2)
    in aux x l [];;
    
let divide l = 
    let rec aux l1 l2 l3 = match l1 with
        [] -> List.rev(l2),List.rev(l3)
      | h1::h2::t -> aux t (h1::l2) (h2::l3)
      | h::[] -> List.rev(h::l2),List.rev(l3)
    in aux l [] [];;
    
let compress l = 
    let rec aux l l1 = match l with
        [] -> l1
      | h1::h2::t -> if h1 = h2
                     then aux (h2::t) l1
                     else aux (h2::t) (h1::l1)
      | h::[] -> List.rev(h::l1) 
    in aux l [];;
    
    
    
    
    
    
    
