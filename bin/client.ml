open Lwt
open Lwt.Syntax

(* Escape characters for formatting *)
let clear_line = "\027[2K"
let move_cursor_left = "\027[1000D"
let clear = clear_line ^ move_cursor_left
let line_up = "\027[A"
let clear_screen = "\027[2J"
let move_cursor_home = "\027[H"
let fresh_screen = clear_screen ^ move_cursor_home

(* Colors for display *)
let red = "\027[1;31m"
let yellow = "\027[1;33m"
let caml_orange = "\027[38;5;214m"
let bold = "\027[1m"
let reset = "\027[0m"

(* Messages to display *)
let welcome_msg = bold ^ caml_orange ^ "Welcome to Chani, an OCaml-based CLI chat server!" ^ reset
let disconn_msg = yellow ^ "Disconnecting..." ^ reset
let server_disconn_msg = red ^ "\nServer disconnected" ^ reset

(* Client "structure" *)
let default_host = ref "127.0.0.1"
let default_port = ref 9000

(* Getter and setter for host *)
let set_host str = 
  default_host := str

let get_server_ip ()  = !default_host

(* Handle command input to update host and port *)
let parse_command_line () =
  let usage_msg = 
    "Usage: client [-h <host>] [-p <port>]\n\
     Default: host=127.0.0.1, port=9000" 
  in
  let spec_list = [
    ("-h", Arg.String set_host, "Host to connect to (default: 127.0.0.1)");
    ("-p", Arg.Set_int default_port, "Port to connect to (default: 9000)");
  ] in
  Arg.parse spec_list (fun _ -> ()) usage_msg

(* Create the client-server connection *)
let create_connection server_ip =
  let connect_address = Unix.inet_addr_of_string server_ip in
  let sock = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
  let* () = Lwt_unix.connect sock @@ ADDR_INET(connect_address, !default_port) in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input sock in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output sock in
  return (ic, oc)

(* Functions for message sending and receiving *)
let send_message oc msg =
  let* () = Lwt_io.write_line oc msg in
  Lwt_io.flush oc

let receive_message ic =
  Lwt_io.read_line_opt ic

(* Input handling loop *)
let rec input_loop oc =
  let* line = Lwt_io.read_line_opt Lwt_io.stdin in
  match line with
  | Some input ->
      let input = String.trim input in
      if input = "quit" then
        let* () = Lwt_io.write_line Lwt_io.stdout disconn_msg in
        Lwt_io.close oc
      else if input = "" then
        let* () = Lwt_io.write Lwt_io.stdout (line_up ^ clear ^ "> ") in
        input_loop oc
      else
        let* () = send_message oc input in
        input_loop oc
  | None -> Lwt_io.close oc

(* Message receiving loop *)
(* WHAT WAS INPUTTED BEFORE RECEIVING A MESSAGE IS NOT DISPLAYED BUT STILL EXISTS. IT MUST BE DISPLAYED. *)
let rec receive_loop ic =
  let* response = receive_message ic in
  match response with
  | Some resp ->
      let* () = Lwt_io.write_line Lwt_io.stdout resp in
      let* () = Lwt_io.write Lwt_io.stdout "> " in
      receive_loop ic
  | None -> Lwt_io.write_line Lwt_io.stdout server_disconn_msg

(* "Main function" *)
let () =
  parse_command_line ();
  Lwt_main.run begin
    Lwt.catch
      (fun () ->
        let server_ip = get_server_ip () in
        let* (ic, oc) = create_connection server_ip in
	let* () = Lwt_io.write_line Lwt_io.stdout (fresh_screen ^ welcome_msg) in
	let* () = Lwt_io.write Lwt_io.stdout "Username > " in
        Lwt.pick [
          input_loop oc;
          receive_loop ic
        ]
      )
      (fun _ -> Lwt.return_unit)
  end
