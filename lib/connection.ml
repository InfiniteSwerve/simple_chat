open Lwt

(* NOTE: handle is the main loop for both client and server.
   Either side can send a message which will only appear in the senders chat after receiving an ack from the reciever.
   This allows for handling multiple en-route messages *)
(* NOTE: This code only runs between two peers,
   but could be scaled up by using a CRDT if we either wanted to drop the roundtrip requirement,
   bite the bullet on roundtrip time,
   or implement a more sophisticated gossip protocol *)
(* NOTE: This code only runs locally *)
(* BUG: If the user is typing input when receiving a message, their input will be distorted by the received text.
   I tried fixing this by utilizing both ansi escape characters, and utilizing the terminal raw mode, but I was unable to fix.
   Ideally we would replace the terminal printing with something dedicated like lambda-term.*)

let handle sock =
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input sock in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output sock in
  let queued_messages = Hashtbl.create 16 in
  let rec read_messages () =
    Lwt_io.read_line_opt ic >>= function
    | Some msg -> (
        (* remove the newlines otherwise message_of_json will fail to parse *)
        match String.trim msg |> Message.of_string_exn with
        | Acknowledgement id ->
            let time, content = Hashtbl.find queued_messages id in
            Lwt_io.printf "User: %s\nRoundtrip time: %f ms\n" content
              (Unix.gettimeofday () -. time)
            >>= fun () -> read_messages ()
        | Message msg ->
            Lwt_io.printf "Other: %s\n" msg.content >>= fun () ->
            Lwt_io.write_line oc
              (Message.Acknowledgement msg.id |> Message.to_string)
            >>= fun () ->
            Lwt_io.flush oc >>= fun () -> read_messages ())
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

let accept conn =
  let sock, _ = conn in
  Lwt.on_failure (handle sock) (fun e ->
      Logs.err (fun m -> m "%s" (Printexc.to_string e)));
  Lwt_io.print "New connection!\n" >>= return

let create address port =
  let open Lwt_unix in
  let sockaddr = ADDR_INET (address, port) in
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  connect sock sockaddr >|= fun () -> sock
