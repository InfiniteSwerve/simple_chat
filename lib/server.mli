val create_socket : unit -> Lwt_unix.file_descr
val create : Lwt_unix.file_descr -> unit -> 'a Lwt.t
