type cadea =
	  Raiz
	| Punto of int * int * cadea;;

let shortest_tour m n saida chegada =
	let rec resolver_cadea l x = (* Convirte a lista de puntos nunha lista de tuplas *)
		match x with
			  Raiz -> l
			| Punto (x, y, Raiz) -> (x,y)::l
			| Punto (x, y, z) -> resolver_cadea ((x,y)::l) z
	in
	let casillas pp saida =
		let rec pasado a = function
			  [] -> false
			| h::t ->
				if h = a then
					true
				else
					pasado a t
		in
		let filtro p =
			match p with
				  Raiz -> None
				| Punto (x,y,z) ->
					if x > 0 && y > 0 && x <= m && y <= n && not (pasado (x,y) pp)
					then
						Some p
					else
						None
		in
			match saida with
			  Raiz -> []
			| Punto (x, y, _) ->
				let l =
					[ Punto (x+2, y+1, saida); Punto (x+2, y-1, saida); Punto (x-2, y+1, saida); Punto (x-2, y-1, saida);
						Punto (x+1, y+2, saida); Punto (x-1, y+2, saida); Punto (x+1, y-2, saida); Punto (x-1, y-2, saida) ]
				in List.filter_map filtro l
	in
	let rec buscar_sol = function (* Comproba se algún punto da lista é solución *)
		  [] -> None
		| h::t ->
			match h with
				  Raiz -> None
				| Punto (x, y, _) ->
					if  (x,y) = chegada
					then
						Some h
					else
						buscar_sol t
	in
	let rec engadir_puntos_pasados pp = function
		  [] -> pp
		| h::t ->
			match h with
				  Raiz -> pp
				| Punto (x,y,_) -> engadir_puntos_pasados ( (x,y)::pp ) t
	in
	let rec lista_casillas pp l = function
		  [] -> l
		| h::t -> lista_casillas pp ( (casillas pp h) @ l ) t
	in
	let rec buscar puntos_pasados posibilidades =
		if posibilidades = [] then
			raise Not_found
		else
			match buscar_sol posibilidades with
				  None -> buscar (engadir_puntos_pasados puntos_pasados posibilidades) (lista_casillas puntos_pasados [] posibilidades)
				| Some x -> resolver_cadea [] x
	in
		let x,y = saida in
		buscar [] (casillas [] (Punto (x,y,Raiz) ) );;
