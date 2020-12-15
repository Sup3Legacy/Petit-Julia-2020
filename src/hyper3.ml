open Astype;;
open AstcompilN


let rec calcArb = function
	|[] -> Failure
	|([],i)::tl -> if List.length tl = 0 then Feuille i else Failure
	|l -> let tM1 = List.fold_left (fun t (l,i) -> match l with 
				|[] -> assert false
				|hd::tl -> if TypeMap.mem hd t then 
						let l1 = TypeMap.find hd t in TypeMap.add hd ((tl,i)::l1) t
					else TypeMap.add hd [tl,i] t) TypeMap.empty l in
		let tM2 = if TypeMap.mem Any tM1 then 
				let l = TypeMap.find Any tM1 in
				TypeMap.map (fun l2 -> l@l2) tM1
			else tM1
		in
		let tM3 = TypeMap.map calcArb tM2 in
		Appels tM3
