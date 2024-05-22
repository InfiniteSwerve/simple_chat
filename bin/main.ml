(* Application should start in two modes: *)

(* as a server, waiting for one client to connect or; *)
(* as a client, taking an IP address (or hostname) of server to connect to. *)
(* After connection is established, user on either side (server and client) can send messages to the other side.
   After connection is terminated by the client, server continues waiting for another client.
   The receiving side should acknowledge every incoming message (automatically send back a "message received" indication),
   sending side should show the roundtrip time for acknowledgment.
   Wire protocol shouldn't make any assumptions on the message contents (e.g. allowed byte values, character encoding, etc). *)

open Simple_chat_lib

let () =
  if Array.length Sys.argv < 2 then
    Lwt_io.print "Please specify either 'client' or 'server' mode"
    |> Lwt_main.run
  else
    match Sys.argv.(1) with
    | "server" ->
        let sock = Server.create_socket () in
        let serve = Server.create sock in
        Lwt_main.run @@ serve ()
    | "client" ->
        let address, port =
          if Array.length Sys.argv < 4 then (
            Printf.printf
              "No address and port specified, defaulting to 127.0.0.1 : 9000\n\
               %!";
            Constants.(server_address, port))
          else
            ( Sys.argv.(2) |> Unix.inet_addr_of_string,
              Sys.argv.(3) |> int_of_string )
        in
        Lwt_main.run @@ Client.run address port
    | _ ->
        Lwt_main.run
        @@ Lwt_io.print
             "Unknown mode, specify either 'server' or 'client' with address \
              and port"
