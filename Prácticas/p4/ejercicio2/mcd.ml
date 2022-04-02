let rec mcd (x, y) = 
    if (x == 0 || y == 0 || x < 0 || y < 0)
    then 0
    else if (x >= y && x > 0)
         then if ((x mod y) == 0)
              then y
              else mcd ((x mod y), y)
         else if (y >= x && y > 0)
              then if ((y mod x) == 0)
                   then x
                   else mcd (x, (y mod x))
              else 0;;

