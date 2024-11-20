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

let receive_message ic =
  Lwt_io.read_line_opt ic

let rec input_loop oc =
  let* () = Lwt_io.write Lwt_io.stdout "> " in
  let* input = Lwt_io.read_line Lwt_io.stdin in
  match input with
  | "quit" -> Lwt_io.close oc
  | msg ->
    let* () = send_message oc msg in
    input_loop oc
    

let rec receive_loop ic = 
    let* response = receive_message ic in
let rec write_loop oc =
  print_string "> ";
  flush stdout;
  Lwt.bind (Lwt_io.read_line Lwt_io.stdin) (
    function msg -> match msg with
      | "quit" -> Lwt.return_unit
      | x ->
        let* () = send_message oc x in
        write_loop oc
  )

let rec receive_loop ic =
  let* response = receive_message ic in
    match response with
    | Some resp ->
        let* () = Lwt_io.write_line Lwt_io.stdout ("\n" ^ resp) in
        let* () = Lwt_io.write Lwt_io.stdout "> " in
        receive_loop ic
      Printf.printf "Server: %s\n" resp;
      receive_loop ic
    | None ->
      Lwt_io.write_line Lwt_io.stdout "Server disconnected"


let () =
  Lwt_main.run begin
    let* (ic, oc) = create_connection () in
    Printf.printf "Connected to server. Type 'quit' to exit.\n";
    Lwt.pick [
      write_loop oc;
      receive_loop ic
    ]
    let* () = Lwt_io.write_line Lwt_io.stdout "Connected to server. Type 'quit' to exit.\n" in
    Lwt.pick [input_loop oc ; receive_loop ic]
  end
