open Ast
open Astype
open Lexer
open Parser

module Tmap = Map.Make(String) (* Map contenant les environnements typage *)
type env = Astype.pjtype Tmap.t

let rec type_fonctions programme = ()
  (* retourne l'environnement contenant les déclaratiosn de type des fonctions, sans les explorer *)
;;
