
open Yojson
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util
open Printf

let repo_url = "https://raw.githubusercontent.com/Sup3Legacy/pjulia-packages/main/" (* URL du repo *)
let index_name = "index.json"
let index_url = repo_url ^ index_name

module DepSet = Set.Make(String) (* pour les dépendances *)
module PackMap = Map.Make(String) (* pour les packages *)

type dependencySet = DepSet.t
type package = (string * string * string * string * dependencySet) PackMap.t

let packagesMap = (ref PackMap.empty : package ref);;

let decompose_json pack = (* Décompose un champ de package en ses champs *)
  let name = pack |> member "name" |> to_string in
  let description = pack |> member "description" |> to_string in
  let version = pack |> member "version" |> to_string in
  let url = pack |> member "url" |> to_string in
  let dep = pack |> member "dependencies" |> to_list in
  let dependencies = DepSet.of_list (List.map (fun json -> json |> member "name" |> to_string) dep) in
  (* dependencies est une liste de noms de packagtes *)
  (name, version, description, url, dependencies)
;;


let get_packages_list json_file =
  (* Read the JSON file *)
  let pack = json_file |> member "packages" |> to_list in
  let packages = List.map (fun json -> decompose_json json) pack in
  let names = List.map (fun json -> json |> member "name" |> to_string) pack in
  let rec construit map lpacks lnames =
    match lpacks, lnames with
    | [], [] -> map
    | p :: qp, n :: qn -> construit (PackMap.add n p map) qp qn
    | _ -> failwith "WTF man"
  in
  packagesMap := construit PackMap.empty packages names;
;;

let download name =
  let download_procedure  =
    Client.get (Uri.of_string name) >>= fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    body
  in
  Lwt_main.run download_procedure
;;

let download_package file_name =
  let index =
    try
      Yojson.Basic.from_file file_name
    with _ ->
      begin
        print_endline "Index isn't here. Downloading...";
          let file = download index_url in
          let oc = open_out "index.json" in
          Printf.fprintf oc "%s" file;
          close_out oc;
          print_endline "Downloaded, Imma parse it";
          Yojson.Basic.from_file "index.json"
      end
  in
  let map = get_packages_list index in
  ()
;;
