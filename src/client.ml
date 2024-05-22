open Lwt

let run address port =
  Connection.create address port >>= fun sock ->
  Lwt_io.print "Successfully connected\n" >>= fun () -> Connection.handle sock
