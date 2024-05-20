(* Application should start in two modes: *)

(* as a server, waiting for one client to connect or; *)
(* as a client, taking an IP address (or hostname) of server to connect to. *)
(* After connection is established, user on either side (server and client) can send messages to the other side.
   After connection is terminated by the client, server continues waiting for another client.
   The receiving side should acknowledge every incoming message (automatically send back a "message received" indication),
   sending side should show the roundtrip time for acknowledgment.
   Wire protocol shouldn't make any assumptions on the message contents (e.g. allowed byte values, character encoding, etc). *)

open Lwt

let listen_address = Unix.inet_addr_loopback
let port = 9000
let backlog = 10

(* TODO: Non-local addresses *)
(* TODO: Will overwrite current user inputs *)
let insert_message_above msg =
  Lwt_io.print "\0277" >>= fun () ->
  (* ESC 7 - Save cursor position *)
  Lwt_io.print "\027[G" >>= fun () ->
  (* ESC [G - Move to start of line *)
  Lwt_io.print "\027[A" >>= fun () ->
  (* ESC [A - Move up one line *)
  Lwt_io.print "\027[L\027[B" >>= fun () ->
  (* ESC [L - Insert a new line, ESC [B - Move down one line *)
  Lwt_io.printlf "other: %s" msg >>= fun () ->
  Lwt_io.print "\0278" (* ESC 8 - Restore cursor position *)

module Message = struct
  type message =
    | Message of { id : int; time : float; content : string }
    | Acknowledgement of int
  [@@deriving yojson]

  type t = message [@@deriving yojson]

  let to_string message = to_yojson message |> Yojson.Safe.to_string
  let from_string message = Yojson.Safe.from_string message |> message_of_yojson
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
end

let handle_connection sock =
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input sock in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output sock in
  let queued_messages = Hashtbl.create 16 in
  let rec read_messages () =
    Lwt_io.read_line_opt ic >>= function
    | Some msg -> (
        match String.trim msg |> Message.from_string with
        | Ok msg -> (
            match msg with
            | Acknowledgement id ->
                let time, content = Hashtbl.find queued_messages id in
                Lwt_io.printf "User: %s\nRoundtrip time: %f\n" content
                  (Unix.time () -. time)
                >>= fun () -> read_messages ()
            | Message msg ->
                Lwt_io.printf "Other: %s\n" msg.content >>= fun () ->
                Lwt_io.write_line oc
                  (Message.Acknowledgement msg.id |> Message.to_string) 
                >>= fun () -> Lwt_io.flush oc >>=  fun () -> read_messages ())
        | Error err ->
            Lwt_io.printf "Failed to parse message: %s\n" err >>= fun () ->
            read_messages ())
    | None -> Lwt_io.print "Connection closed\n"
  in
  let rec send_messages () =
    Lwt_io.read_line Lwt_io.stdin >>= fun content ->
    let message = Message.create content in
    (* TODO: A bit verbose here *)
    Hashtbl.add queued_messages
      (Message.get_id_exn message)
      (Message.get_time_exn message, Message.get_content_exn message);
    Message.to_yojson message |> Yojson.Safe.to_string |> Lwt_io.write_line oc
     >>= fun () -> send_messages ()
  in

  let start () =
    Lwt.async read_messages;
    send_messages ()
  in
  start ()

let accept_connection conn =
  let sock, _ = conn in
  Lwt.on_failure (handle_connection sock) (fun e ->
      Logs.err (fun m -> m "%s" (Printexc.to_string e)));
  Lwt_io.print "New connection!\n" >>= return

(* Always uses a predetermined address *)
let create_socket () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  (bind sock @@ ADDR_INET (listen_address, port) |> fun x -> ignore x);
  listen sock backlog;
  sock

let create_server sock =
  let addr = Unix.string_of_inet_addr listen_address in
  let rec serve sock () =
    Lwt_unix.accept sock >>= accept_connection >>= serve sock
  in
  fun () ->
    Lwt_io.print
      (Printf.sprintf "Server created at %s:%d\nWaiting for connection...\n"
         addr port)
    >>= fun () -> serve sock ()

let retrieve_address_and_port () =
  if Array.length Sys.argv < 3 then
    Logs_lwt.info (fun m ->
        m "Specify both address and por\n        t, i.e. 127.0.0.1 9000")
    >>= fun () -> Lwt.fail_with "Insufficient arguments"
  else
    let address = Sys.argv.(2) |> Unix.inet_addr_of_string in
    let port = Sys.argv.(3) |> int_of_string in
    return (address, port)

let create_connection address port =
  let open Lwt_unix in
  let sockaddr = ADDR_INET (address, port) in
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  connect sock sockaddr >|= fun () -> sock

let run_client address port =
  create_connection address port >>= fun sock ->
  Lwt_io.print "Successfully connected\n" >>= fun () -> handle_connection sock

let () =
  if Array.length Sys.argv < 2 then
    Logs_lwt.info (fun m -> m "Please specify either 'client' or 'server' mode")
    |> Lwt_main.run
  else
    match Sys.argv.(1) with
    | "server" ->
        let sock = create_socket () in
        let serve = create_server sock in
        Lwt_main.run @@ serve ()
    | "client" ->
        if Array.length Sys.argv < 4 then
          Logs_lwt.info (fun m ->
              m "Please specify address and port for client mode")
          |> Lwt_main.run
        else
          let address = Sys.argv.(2) |> Unix.inet_addr_of_string in
          let port = Sys.argv.(3) |> int_of_string in
          Lwt_main.run @@ run_client address port
    | _ ->
        Lwt_main.run
          (Logs_lwt.info (fun m ->
               m
                 "Unknown mode, specify either 'server' or 'client' with \
                  address and port"))
