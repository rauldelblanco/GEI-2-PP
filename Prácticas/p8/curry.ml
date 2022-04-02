let curry = function a -> function b -> function c -> a (b,c);;
(*val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c = <fun>*)

let uncurry a = function (b,c) -> a b c;;
(*val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c = <fun>*)

uncurry (+);;
(*- : int * int -> int = <fun>*)

let sum = (uncurry (+));;
(*val sum : int * int -> int = <fun>*)

(*sum 1;;*)
(*This expression has type int but an expression was expected of type int * int*)

sum (2,1);;
(*- : int = 3*)

let g = curry (function p -> 2 * fst p + 3 * snd p);;
(*val g : int -> int -> int = <fun>*)

(*g (2,5);;*)
(*This expression has type 'a * 'b but an expression was expected of type int*)

let h = g 2;;
(*val h : int -> int = <fun>*)

h 1, h 2, h 3;;
(*- : int * int * int = (7, 10, 13)*)
