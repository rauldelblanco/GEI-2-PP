let rec remove v = function
    [] -> []
  | h::t -> if h = v 
            then t
            else h::(remove v t);;

let remove_all v l = 
    let rec aux l l1 = match l with
        [] -> List.rev (l1)
      | h::t -> if h = v
                then aux t l1
                else aux t (h::l1)
    in aux l [];;
    
let rec ldif l1 l2 = match l2 with
    [] -> l1
  | h::t -> ldif (remove_all h l1) t;;
  
let rec lprod l1 l2 = match l1 with
    [] -> []
  | h::t -> let rec aux h1 l2 = match l2 with
                [] -> []
              | h::t -> (h1,h)::(aux h1 t)
            in (aux h l2) @ (lprod t l2);;
            
let rec divide = function 
    h1::h2::t -> let l1,l2 = divide t
                 in h1::l1,h2::l2
 | l -> l, [];;
