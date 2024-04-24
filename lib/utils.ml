let unwrap_or ~default = function
  | Some v -> v
  | None -> default
;;

let unwrap ?msg = function
  | None -> failwith @@ unwrap_or ~default:"Found none while unwrapping option" msg
  | Some v -> v
;;
