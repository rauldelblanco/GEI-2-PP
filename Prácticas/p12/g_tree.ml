type 'a g_tree = Gt of 'a * 'a g_tree list;;

let rec size = function 
    Gt (_,[]) -> 1
  | Gt (r,h::t) -> size h + size (Gt (r,t));;
  
let rec height = function
    Gt(x, []) -> 1
   | Gt(x, subarbol) -> 1 + (List.fold_left (max) 1 (List.map height subarbol));; 
  
let rec leaves = function
    Gt(x,[]) -> [x]
  | Gt(_, subarbol) -> List.fold_left (@) [] (List.map leaves subarbol);;
        
let rec mirror = function
    Gt (v, []) -> Gt (v, [])
  | Gt (v, l) -> Gt (v , List.rev (List.map (mirror) l) );;
  
let rec preorder =
    let rec bucle l = function
        [] -> l
      | h::t -> bucle ( l @ (preorder h) ) t
    in function
           Gt (v, []) -> [v]
         | Gt (v, l) -> v::bucle [] l;;
         
let rec postorder =
    let rec bucle l = function
        [] -> l
      | h::t -> bucle ( l @ (postorder h) ) t
    in function
           Gt (v, []) -> [v]
         | Gt (v, l) -> (bucle [] l) @ [v];;
