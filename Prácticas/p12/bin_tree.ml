type 'a bin_tree =
    Empty
  | Node of 'a * 'a bin_tree * 'a bin_tree;;

let rec fold_tree f a = function
    Empty -> a
  | Node (x, l, r) -> f x (fold_tree f a l) (fold_tree f a r);;

(* Implemente sum, prod, size, inorder y mirror usando fold_tree *)

let sum arbol = 
    let aux a b c = a + b + c in
        fold_tree aux 0 arbol;;

let prod arbol = 
    let aux a b c = a *. b *. c in
        fold_tree aux 1. arbol;;

let size arbol = 
    let aux a b c = 1 + b + c in
        fold_tree aux 0 arbol;;

let inorder arbol = fold_tree (fun a b c -> b@[a]@c) [] arbol;;

let mirror arbol = fold_tree (fun a izq der -> Node(a,der,izq)) Empty arbol;;

