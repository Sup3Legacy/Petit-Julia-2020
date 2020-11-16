open Ast
open Astype
open Lexer
open Parser

module Tmap = Map.Make(String) (* Map contenant les environnements typage *)
type varEnv = Astype.pjtype list Tmap.t
type funcEnv = (Astype.pjtype list) TMmap.t

let rec type_fonctions prog (env : funcEnv) =
  (* retourne l'environnement contenant les déclaratiosn de type des fonctions, sans les explorer *)
  match prog with
  | DeclarationList [] -> env
  | DeclarationList (t :: q) -> type_fonctions (DeclarationList q) (add_function t env)
;;

let add_function func (env : funcEnv) =
  match func with
  | Function (i, p, t, b) ->
    if Tmap.mem i env then
      Tmap.add i (t :: (Tmap.find i env)) env
    else
      Tmap.add i [t] env
;;
