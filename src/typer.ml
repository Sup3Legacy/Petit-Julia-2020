open Ast
open Astype
open Lexer
open Parser

module Tmap = Map.Make(String) (* Map contenant les environnements typage *)
type env = typ Tmap.t
