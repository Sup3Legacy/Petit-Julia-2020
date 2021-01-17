(*
###########################################################
#                                                         #
#             Fonctions utiles pour le parsing            #
#                                                         #
###########################################################
*)

open Ast

let arr = ref 0;;
let newArr () =
  let temp = !arr in
  arr := !arr + 1;
  temp
;;

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
  let name = ".temp_array" ^ (string_of_int (newArr ())) in
  let creation = posVide, ElvalueAffect (posVide, Lident (posVide, name), (posVide, Eapplication(posVide, "newarray", [(posVide, Eentier (Int64.of_int l)); head]))) in (* Code pour créer l'array temporaire *)
  (posVide, Ebloc1 (posVide, creation :: (
      List.mapi (fun indice x -> posVide, 
        ElvalueAffect (posVide, Larray ((posVide, Elvalue (Lident (posVide, name))), (posVide, Eentier (Int64.of_int (indice)))), x))
        arr
        ) @ [(posVide, Elvalue (Lident (posVide, name)))] 
      )
    )
;;

let separate_string str file_name =
  let termList = List.filter (fun x -> x <> "") (String.split_on_char ':' str) in (* identifiant coupé aux :: *)
  match termList with
  | [_] -> (str, None) (* Il n'y a pas de "::", tout est bon *)
  | t :: [q]->  
    if t = file_name then (t ^ ("::" ^ q), None)
    else
      (t, Some q)
  | t :: q :: n ->
    if t = file_name then (t ^ ("::" ^ q), Some (String.concat "::" n))
    else
      (t, Some (String.concat "::" (q :: n)))
  | _ -> assert false

