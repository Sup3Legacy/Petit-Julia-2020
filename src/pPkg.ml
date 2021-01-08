
open Yojson
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util
open Printf

let repo_url = "https://raw.githubusercontent.com/Sup3Legacy/pjulia-packages/main/" (* URL du repo *)
let index_name = "index.json"
let index_url = repo_url ^ index_name

let package_folder = "packages"

let package_path = 
  (let paths = List.rev (String.split_on_char '/' Sys.executable_name) in
  match paths with
  | t :: q -> (String.concat "/" (List.rev q)) ^ "/" ^ package_folder ^ "/"
  | [] -> failwith "empty path");;

print_endline package_path;;

module DepSet = Set.Make(String) (* pour les dépendances *)
module PackMap = Map.Make(String) (* pour les packages *)

type dependencySet = DepSet.t
type package = (string * string * string * string * dependencySet) PackMap.t

exception Error404

let packagesMap = (ref PackMap.empty : package ref);;

let get_package_path filename =
  package_path ^ filename
;;

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
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    body
  in
  Lwt_main.run download_procedure
;;

let update () =
  print_endline "Downloading index...";
  let file =
    try
      download index_url
    with _ -> failwith "Unable to download index..." in (* À améliorer *)
  (try
    Sys.remove (get_package_path "index.json");
  with _ -> ());
  let oc = open_out (get_package_path "index.json") in
  Printf.fprintf oc "%s" file;
  close_out oc;
  print_endline "Downloaded, Imma parse it";
;;

let download_package request_name =
  let index =
  Yojson.Basic.from_file (get_package_path "index.json")
  in
  get_packages_list index;
  let (name, version, description, url, dependencies) =
  try
    PackMap.find request_name !packagesMap
  with _ -> failwith ("Unable to find requested package " ^ request_name) (* À améliorer *)
  in
  try
    begin
      let file = download (url) in
      if (String.sub file 0 3) = "404" then raise Error404;
      (try
       Sys.remove (get_package_path (name ^ ".jl"));
      with _ -> ());
      let oc = open_out (get_package_path (name ^ ".jl")) in
      Printf.fprintf oc "%s" file;
      close_out oc;
      print_endline ("Succesfully downloaded package " ^ name)
    end
  with _ -> failwith "Failed downloading requested package"
;;

let remove_package name =
  let index =
  Yojson.Basic.from_file (get_package_path "index.json")
  in
  get_packages_list index;
  let (name, version, description, url, dependencies) =
   try
    PackMap.find name !packagesMap
  with _ -> failwith ("Requested package not found : " ^ name ^ ". Are you trying to try and abuse pPkg by removing non-package files?!") (* À améliorer *)
in
try
  begin
    Sys.remove (get_package_path (name ^ ".jl"));
    print_endline ("Succesfully removed package " ^ name)
  end
with _ -> failwith "Failed removing requested package"
