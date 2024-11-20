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


let clear_line = "\027[2K"
let move_cursor_left = "\027[1000D"

let rec input_loop oc input_buffer =
 let* char = Lwt_io.read_char Lwt_io.stdin in
  match char with
  | '\n' ->
    let input = Buffer.contents input_buffer in
    Buffer.clear input_buffer;
    if input = "quit" then
      Lwt_io.close oc
    else
      let* () = send_message oc input in
      input_loop oc input_buffer
  | c ->
    Buffer.add_char input_buffer c;
    let* () = Lwt_io.write_char Lwt_io.stdout c in
    Lwt_io.flush Lwt_io.stdout >>= fun () ->
    input_loop oc input_buffer
    

let rec receive_loop ic input_buffer =
  let* response = receive_message ic in
  match response with
  | Some resp ->
      let current_input = Buffer.contents input_buffer in
      (* Move cursor to the beginning of the line and clear it *)
      let* () = Lwt_io.write Lwt_io.stdout (move_cursor_left ^ clear_line) in
      (* Print the incoming message *)
      let* () = Lwt_io.write_line Lwt_io.stdout resp in
      (* Re-display the prompt and current input *)
      let* () = Lwt_io.write Lwt_io.stdout ("> " ^ current_input) in
      (* Flush stdout to ensure everything is displayed *)
      let* () = Lwt_io.flush Lwt_io.stdout in
      receive_loop ic input_buffer
  | None ->
    Lwt_io.write_line Lwt_io.stdout "Server disconnected"

let () =
  Lwt_main.run begin
    let* (ic, oc) = create_connection () in
    let* () = Lwt_io.write_line Lwt_io.stdout "Connected to server. Please enter your username. Type 'quit' to exit." in
    let* () = Lwt_io.write Lwt_io.stdout "\nUsername: " in
    let input_buffer = Buffer.create 128 in
    Lwt.pick [
      input_loop oc input_buffer;
      receive_loop ic input_buffer
    ]
  end
