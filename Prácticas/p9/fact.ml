let rec fact = function
    0 -> 1
  | n -> n * fact (n - 1);;

if (Array.length Sys.argv) = 2
    then
        try print_endline(string_of_int(fact (int_of_string Sys.argv.(1)))) with
            |_-> print_endline("fact: argumento inválido")
else print_endline("fact: número de argumentos inválido");;  
    
    
