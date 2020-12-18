open Core_kernel
open Yojson
open Lwt
open Cohttp
open Cohttp_lwt_unix

let repo_url = "https://raw.githubusercontent.com/Sup3Legacy/pjulia-packages/main/" (* URL du repo *)
let index_url = repo_url ^ "index.json"

let download_procedure name =
  Client.get (Uri.of_string (repo_url ^ name)) >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  body
;;

let download name =
  Lwt_main.run body name
;;

let decompose_json pack = (* Décompose un champ de package en ses champs *)
  let name = pack |> member "name" |> to_string in
  let description = pack |> member "description" |> to_string in
  let version = pack |> member "version" |> to_string in
  let url = pack |> member "url" |> to_string in
  let dep = pack |> member "dependencies" |> to_list in
  let dependencies = List.map dep ~f:(fun json -> member "name" json |> to_string) in
  (* dependencies est une liste de noms de packagtes *)
  ()
;;


let get_packages_list file_name =
  (* Read the JSON file *)
  let json = Yojson.Basic.from_file file_name in
  let pack = json |> member "packages" |> to_list in
  let packages = List.map pack ~f:(fun json -> decompose_json json) in
  packages
;;
