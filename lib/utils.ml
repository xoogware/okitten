(** Returns the given option's value if it is Some, or [default] if it is None. *)
let unwrap_or ~default = function
  | Some v -> v
  | None -> default
;;

(** infix option coalescing :flushed: *)
let ( /// ) lhs rhs = unwrap_or ~default:rhs lhs

(** Returns the given option's value if it is Some, or the return value of [f] if it is None. *)
let unwrap_or_else ~f = function
  | Some v -> v
  | None -> f ()
;;

(** Returns the given option's value if it is Some, or fails with [msg] (or a default message). *)
let unwrap ?msg = function
  | None -> failwith @@ (msg /// "Found none while unwrapping option")
  | Some v -> v
;;
