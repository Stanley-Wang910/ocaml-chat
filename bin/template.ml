open Lwt
exception NotImplemented
(*prelude*)

(*shared mutable counter *)
let counter = ref 0

(*set up server address*)
let listen_address = Unix.inet_addr_loopback
let port = 9000

(* implementation *)

let handle_message msg = 
  raise NotImplemented
  
let rec handle_connection incoming_conn outgoing_conn () =
  Lwt_io.read_line_opt incoming_conn >>=
    raise NotImplemented

let accept_connection conn =
    raise NotImplemented

let create_socket () =
    raise NotImplemented

let create_server sock =
  let rec serve () =
    raise NotImplemented
  in serve

