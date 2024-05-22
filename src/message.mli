type message =
  | Message of { id : int; time : float; content : string }
  | Acknowledgement of int
[@@deriving yojson]

type t = message
[@@deriving yojson]

val to_string : t -> string
val of_string_exn : string -> t
val create : string -> t

(* Helper functions to get message values without destructuring *)
val get_time_exn : t -> float
val get_content_exn : t -> string
val get_id_exn : t -> int
