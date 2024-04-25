(** Returns the given option's value if it is Some, or [default] if it is None. *)
let unwrap_or ~default = function
  | Some v -> v
  | None -> default
;;

(** Returns the given option's value if it is Some, or the return value of [f] if it is None. *)
let unwrap_or_else ~f = function
  | Some v -> v
  | None -> f ()
;;

(** Returns the given option's value if it is Some, or fails with [msg] (or a default message). *)
let unwrap ?msg = function
  | None -> failwith @@ unwrap_or ~default:"Found none while unwrapping option" msg
  | Some v -> v
;;
