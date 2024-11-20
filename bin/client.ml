open Lwt
open Lwt.Syntax

let connect_address = Unix.inet_addr_loopback
let port = 9000

let create_connection () =
  let sock = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
  let* () = Lwt_unix.connect sock @@ ADDR_INET(connect_address, port) in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input sock in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output sock in
  return (ic, oc)

let send_message oc msg =
  let* () = Lwt_io.write_line oc msg in
  Lwt_io.flush oc
    
let rec write_loop oc =
  Lwt.bind (Lwt_io.read_line Lwt_io.stdin) (
    function msg -> match msg with
      | "quit" -> Lwt_io.close oc
      | "" -> write_loop oc
      | x ->
        let* () = send_message oc x in
        write_loop oc
  )
  
let rec receive_loop ic =
  Lwt.bind (Lwt_io.read_line_opt ic) (
    fun response -> 
      match response with
      | Some resp ->
        let* () = Lwt_io.write_line Lwt_io.stdout ("\n" ^ resp) in
        let* () = Lwt_io.write Lwt_io.stdout "> " in
        receive_loop ic
      | None ->
        Lwt_io.write_line Lwt_io.stdout "Server disconnected"
  )

let () =
  Lwt_main.run begin
    let* (ic, oc) = create_connection () in
    let* () = Lwt_io.write_line Lwt_io.stdout "Connected to server. Type 'quit' to exit.\n" in
    Lwt.pick [write_loop oc ; receive_loop ic]
  end
