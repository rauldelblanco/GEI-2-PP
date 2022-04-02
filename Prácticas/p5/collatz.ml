let f n = if n mod 2 = 0 then n / 2 else 3 * n + 1;;

let rec orbit n = if n > 0
                  then match n with
                       | 1 -> Printf.printf "1\n"
                       | n -> Printf.printf "%d, " n ; if n mod 2 = 0 then orbit (n / 2) else orbit (3 * n + 1)
                  else Printf.printf "Error: Number not valid\n";;

let rec length n = if n > 0
                   then if (f n) = 1
                        then 1
                        else 1 + length (f n)
                   else 0;;
                   
let rec top n = if n = 1
                then 1
                else max n (top (f n));;
                
let rec length'n'top n = if n = 1
                         then (0,1)
                         else let len,tp = length'n'top (f n)
                              in (len+1, max n tp);;

let rec longest_in m n =
    if m=n 
    then m 
    else let aux = longest_in (m+1) n 
    in if (length m) >= (length aux) 
       then m
       else aux;;
       
let rec highest_in m n = 
    if m = n
    then m
    else let aux = highest_in (m+1) n 
         in if (top m) >= (top aux)
            then m
            else aux;;
