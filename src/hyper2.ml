open Ast

let fusionPos p1 p2 =
  {ldeb = p1.ldeb; cdeb = p1.cdeb; lfin = p2.lfin; cfin = p2.cfin}
