(** Gets the current built version of OKitten from the Dune version tag. *)
let get () =
  match Build_info.V1.version () with
  | None -> "unknown"
  | Some v -> Build_info.V1.Version.to_string v
;;
