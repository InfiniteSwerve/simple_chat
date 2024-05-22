open Lwt
open Constants

(* Always uses a predetermined address *)
let create_socket () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  (* TODO: allows us to reconnect to the bound socket in case of a server kill *)
  setsockopt sock SO_REUSEADDR true;
  (bind sock @@ ADDR_INET (listen_address, port) |> fun x -> ignore x);
  listen sock backlog;
  sock

let create sock =
  let addr = Unix.string_of_inet_addr listen_address in
  let rec serve sock () =
    Lwt_unix.accept sock >>= Connection.accept >>= serve sock
  in
  fun () ->
    Lwt_io.print
      (Printf.sprintf "Server created at %s:%d\nWaiting for connection...\n"
         addr port)
    >>= fun () -> serve sock ()
