open Format

type structure =
  (string * bool * (string * value) list) ref (* Le premier string est le nom de la structure *)
and value =
  | Vint of int
  | Vbool of bool
  | Vstring of string
  | Vfloat of float (* Future *)
  | Vstruct of structure
  | Vnothing
;;
