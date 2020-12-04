open Format

type structure =
  string * bool * (string list) * ((string, value) Hashtbl.t) (* Le premier string est le nom de la structure *)
and value =
  | Vint of Int64.t
  | Vbool of bool
  | Vstring of string
  | Vfloat of float (* Future *)
  | Vstruct of structure
  | Vnothing
;;
