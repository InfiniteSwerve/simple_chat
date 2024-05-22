type message =
  | Message of { id : int; time : float; content : string }
  | Acknowledgement of int
[@@deriving yojson]

type t = message [@@deriving yojson]

let to_string message = to_yojson message |> Yojson.Safe.to_string
let of_string_exn message = Yojson.Safe.from_string message |> message_of_yojson |> function
  | Ok msg -> msg
  | Error err -> failwith (Printf.sprintf "Failed to parse message from string: %s\n" err)
let id content = String.hash ((Float.to_string @@ Unix.time ()) ^ content)

let create (content : string) =
  Message { id = id content; time = Unix.time (); content }

let get_time_exn message =
  match message with
  | Message msg -> msg.time
  | Acknowledgement _ -> failwith "Message is an acknowledgement"

let get_content_exn message =
  match message with
  | Message msg -> msg.content
  | Acknowledgement _ -> failwith "Message is an acknowledgement"

let get_id_exn message =
  match message with
  | Message msg -> msg.id
  | Acknowledgement _ -> failwith "Message is an acknowledgement"
