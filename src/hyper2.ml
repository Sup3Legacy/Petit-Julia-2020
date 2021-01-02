open Ast

let fusionPos p1 p2 =
  {ldeb = p1.ldeb; cdeb = p1.cdeb; lfin = p2.lfin; cfin = p2.cfin}

let posVide:Ast.position = {ldeb = -1; cdeb = -1; lfin = -1; cfin = -1}

let rec build_array arr =
  let l = List.length arr in
  let head, tail = 
    (match arr with
      | t :: q -> t, q
      | [] -> (posVide, Etrue), [])
  in
  let creation = posVide, ElvalueAffect (posVide, Lident (posVide, "_temp_array"), (posVide, Eapplication(posVide, "newarray", [(posVide, Eentier (Int64.of_int l)); head]))) in (* Code pour créer l'array temporaire *)
  (posVide, Ebloc1 (posVide, creation :: (List.mapi (fun indice x -> posVide, Eapplication (posVide, "_setelement", [(posVide, Elvalue (Lident (posVide, "_temp_array"))); (posVide, Eentier (Int64.of_int (indice))); x])) arr) @ [(posVide, Elvalue (Lident (posVide, "_temp_array")))] ))
;;
  

