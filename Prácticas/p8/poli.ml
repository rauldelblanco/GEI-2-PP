let f = function a -> a;;
(*Tan solo se puede escribir una función de este tipo*)

let h = function (a,_) -> a;;
(*Tan solo se puede escribir una función de este tipo*)

let i = function (_,b) -> b;;
(*Tan solo se puede escribir una función de este tipo*)

let j = function a -> [a];;
(*Se pueden escribir infinitas soluciones para este tipo de dato*)
