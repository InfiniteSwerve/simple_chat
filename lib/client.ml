open Utils

let run address port =
  let* sock = Connection.create address port in
  let* () = Lwt_io.print "Successfully connected\n" in 
  Connection.handle sock
