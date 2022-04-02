let hd = function
         [] -> raise(Failure "hd")
       | h::_ -> h;;
           
let tl = function
           [] -> raise(Failure "tl")
           | _::t -> t;;
           
let length l = 
    let rec aux l x = match l with 
        [] -> x
      | h::t -> aux t (x + 1)
    in aux l 0;;   

let rec compare_lengths l1 l2 = match l1, l2 with
        [], [] ->  0
      | [], _::_ -> -1
      | _::_, [] -> 1
      | _::t1, _::t2 -> compare_lengths t1 t2;;           
      
let rec nth l n =
  if n < 0
    then raise (Invalid_argument"List.nth")
    else let aux1 = function
      [] -> raise (Failure"nth")
      | h::t -> (function 0 -> h | n -> nth t (n - 1))
    in aux1 l n;;
      
let rec append l1 l2 = match l1 with
    [] -> l2
  | h::t -> h :: append t l2;;
  
let rec find f l = match l with
    [] -> raise (Not_found)
  | h::t -> if f h
            then h
            else find f t;;   
    
(*let rec for_all f l = match l with
    [] -> true
  | h::t -> if f h 
            then for_all f t
            else false;;*)
            
let rec for_all f l = match l with
    [] -> true
  | h::t -> f h && for_all f t;;        
                   
(*let rec exists f l = match l with
    [] -> false
  | h::t -> if f h 
            then true 
            else exists f t;;*)
            
let rec exists f l = match l with
    [] -> false
  | h::t -> f h || exists f t;;
    
let rec mem a l = match l with
    [] -> false
  | h::t -> if a = h
            then true
            else mem a t;;
    
let rec filter f l = match l with
    [] -> []
  | h::t -> if f h 
            then h::filter f t
            else filter f t;;

let rec find_all f l = match l with
    [] -> []
  | h::t -> if f h 
            then h::find_all f t
            else find_all f t;;  

let rec partition f = function
    [] -> [],[]
  | h::t -> let t1,t2 = partition f t 
            in if f h 
               then h::t1,t2
               else t1, h::t2;;
    
    
let rec split l = match l with
    [] -> ([],[])
  | (h1,h2)::t -> let (l1,l2) = split t in (h1::l1, h2::l2);;
  
let rec combine l1 l2 = match l1,l2 with
    [],[] -> []
  | [],_ | _,[] -> raise (Invalid_argument "combine")
  | h1::t1,h2::t2 -> let t = combine t1 t2 in (h1,h2)::t;;
                     
let rec rev l = match l with
    [] -> []
  | h::t -> append (rev t) [h];;

let rec aux len f = if len = 0 
                    then []
                    else (f len)::aux (len - 1) f;;

let init len f = if len < 0 
                     then raise (Invalid_argument "init")
                     else rev(aux len f);;

let rev_append l1 l2 = append (rev l1) l2;;

let rec concat = function
  [] -> []
  | h::t -> append h (concat t);;

let flatten = concat;;

let rec map f l = match l with
  [] -> []
  | h::t -> f(h)::map f t;;

let rev_map f l = map f (rev l);;

let rec map2 f l1 l2 =
  if (length l1 != length l2)
    then raise (Invalid_argument"map2")
    else if (length l1 == 0)
      then []
      else (f(hd l1)(hd l2))::map2 f (tl l1)(tl l2);;

let rec fold_left f a l = match l with
  [] -> a
  | h::t -> fold_left f (f a h) t;;

let rec fold_right f l a = match l with
  [] -> a
  | h::t -> f h (fold_right f t a);;


















