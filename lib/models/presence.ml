module Activity = struct
  type activity_kind =
    | Game
    | Streaming
    | Listening
    | Watching
    | Custom
    | Competing

  let int_of_activity_kind = function
    | Game -> 0
    | Streaming -> 1
    | Listening -> 2
    | Watching -> 3
    | Custom -> 4
    | Competing -> 5
  ;;

  let activity_kind_of_int = function
    | 0 -> Game
    | 1 -> Streaming
    | 2 -> Listening
    | 3 -> Watching
    | 4 -> Custom
    | 5 -> Competing
    | _ -> failwith "unknown activity kind"
  ;;

  let yojson_of_activity_kind k = `Int (int_of_activity_kind k)

  let activity_kind_of_yojson = function
    | `Int v -> activity_kind_of_int v
    | _ -> failwith "invalid activity kind"
  ;;

  type t =
    { name : string
    ; kind : activity_kind [@key "type"]
    ; url : string option
    ; state : string option
    }
  [@@deriving yojson]

  (** Initializes a new {!t} to build. *)
  let empty = { name = ""; kind = Custom; url = None; state = None }

  let set_name name t = { t with name }
  let set_kind kind t = { t with kind }
  let set_url url t = { t with url }
  let set_state state t = { t with state }
end

type status =
  | Online
  | DoNotDisturb
  | Idle
  | Invisible
  | Offline

let status_of_string = function
  | "online" -> Online
  | "dnd" -> DoNotDisturb
  | "idle" -> Idle
  | "invisible" -> Invisible
  | "offline" -> Offline
  | _ -> failwith "invalid status"
;;

let string_of_status = function
  | Online -> "online"
  | DoNotDisturb -> "dnd"
  | Idle -> "idle"
  | Invisible -> "invisible"
  | Offline -> "offline"
;;

let status_of_yojson = function
  | `String s -> status_of_string s
  | _ -> failwith "invalid status"
;;

let yojson_of_status s = `String (string_of_status s)

type t =
  { since : int option
  ; activities : Activity.t list
  ; status : status
  ; afk : bool
  }
[@@deriving yojson]

let empty = { since = None; activities = []; status = Online; afk = false }
let set_since since t = { t with since }
let since_now t = { t with since = Some (int_of_float (Unix.time () *. 1000.)) }
let with_activity activity t = { t with activities = activity :: t.activities }
let set_status status t = { t with status }
let set_afk afk t = { t with afk }
