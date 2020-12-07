open Ast

let fusionPos p1 p2 =
  {ldeb = p1.ldeb; cdeb = p1.cdeb; lfin = p2.lfin; cfin = p2.cfin}

let posVide:Ast.position = {ldeb = -1; cdeb = -1; lfin = -1; cfin = -1}

let rec build_liste liste =
  match liste with
  | [] -> (posVide, Eapplication(posVide, "List", [(posVide, Elvalue (Lident (posVide, "nothing"))); (posVide, Eapplication(posVide, "emptyList", []))]))
  | t :: q -> posVide, Eapplication(posVide, "List", [t; build_liste q])
