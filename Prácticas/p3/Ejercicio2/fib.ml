let rec fib n =
if n <= 1 then n
else fib (n-1) + fib (n-2);;

let rec traza n =
    if n = 0 then "0"
    else traza (n-1) ^ "\n" ^ string_of_int (fib n);;

if (Array.length Sys.argv) = 2  then 
    print_endline (traza (int_of_string (Sys.argv.(1))))
    else print_endline ("fib: numero de argumentos invalido");;
