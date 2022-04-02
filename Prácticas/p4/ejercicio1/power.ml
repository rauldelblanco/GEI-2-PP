let rec power x y = if (y > 0)
     then x * power x (y-1)
     else 1;;

let rec power' x y = if (y mod 2 == 0)
     then if (y > 0)
          then power' (x * x) (y / 2)
          else 1
     else if (y > 0)
          then x * power' (x * x) (y / 2)
          else 1;;          

(*La función power' debería de ser más eficiente que power porque en la primera se dividen las 
operaciones (haciendo que haya menos llamadas recursivas), mientras que en la segunda se tienen que
realizar todas las operaciones recursivas desde 1 hasta n. Esta mejora no es muy notable, ya que 
los tiempos de ejecución son muy parejos*)

let rec powerf x y = if (y mod 2 == 0)
     then if (y > 0)
          then powerf (x *. x) (y / 2)
          else 1.
     else if (y > 0)
          then x *. powerf (x *. x) (y / 2)
          else 1.;;
