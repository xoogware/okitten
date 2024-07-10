module Gateway = Gateway
module Presence = Presence

type date = float

let yojson_of_date d = `String (ISO8601.Permissive.string_of_datetime d)

let dbg_yojson_type_of = function
  | `String _ -> "string"
  | `Int _ -> "int"
  | `Bool _ -> "bool"
  | `Null -> "null"
  | `Assoc _ -> "assoc"
  | `List _ -> "list"
  | `Float _ -> "float"
  | `Intlit _ -> "intlit"
  | `Tuple _ -> "tuple"
  | `Variant _ -> "variant"
;;

let date_of_yojson = function
  | `String d -> ISO8601.Permissive.datetime d
  | c ->
    Logs.err (fun m -> m "%s" @@ dbg_yojson_type_of c);
    failwith "Bad date type"
;;
