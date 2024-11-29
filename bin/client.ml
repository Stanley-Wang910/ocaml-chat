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
let current_input = ref ""

(* Getter and setter for host *)
let set_host str = 
  default_host := str
let get_server_ip () = !default_host

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

(* Set up raw mode for terminal *)
let setup_terminal ?(echo=false) () =
  let term = Unix.tcgetattr Unix.stdin in
  let raw_term = {term with Unix.c_icanon = false; Unix.c_echo = echo} in
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH raw_term;
  Lwt.return term

(* Restore terminal settings *)
let restore_terminal term =
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH term

(* Functions for message sending and receiving *)
let send_message oc msg =
  let* () = Lwt_io.write_line oc msg in
  Lwt_io.flush oc

let receive_message ic =
  Lwt_io.read_line_opt ic

(*Function to redraw the input line *)
let redraw_input () =
  let* () = Lwt_io.write Lwt_io.stdout (clear ^ "> " ^ !current_input) in
  Lwt_io.flush Lwt_io.stdout

(* Input handling loop (character by character) *)
let rec input_loop oc =
  let* char = Lwt_io.read_char_opt Lwt_io.stdin in
  match char with
  | Some '\n' ->
      let input = !current_input in
      current_input := "";
      let* () = Lwt_io.write_line Lwt_io.stdout "" in
      if input = "quit" then
        let* () = Lwt_io.write_line Lwt_io.stdout disconn_msg in
        Lwt_io.close oc
      else if String.trim input = "" then
        let* () = Lwt_io.write Lwt_io.stdout (line_up ^ clear ^ "> ") in 
        input_loop oc
      else
        let* () = send_message oc (String.trim input) in
        input_loop oc
  (* Handle backspace *)
  | Some '\127' ->
      if String.length !current_input > 0 then
        begin
	  current_input := String.sub !current_input 0 (String.length !current_input - 1);
	  let* () = Lwt_io.write Lwt_io.stdout "\b \b" in
          let* () = Lwt_io.flush Lwt_io.stdout in
          input_loop oc
	end
      else
        input_loop oc
  | Some c ->
      (* Check for unprintable characters *)
      if Char.code c < 32 then
        input_loop oc
      else
	begin
          current_input := !current_input ^ String.make 1 c;
          let* () = Lwt_io.write_char Lwt_io.stdout c in
          let* () = Lwt_io.flush Lwt_io.stdout in
      	  input_loop oc
	end
  | None -> Lwt_io.close oc

(* Message receiving loop *)
let rec receive_loop ic =
  let* response = receive_message ic in
  match response with
  | Some resp ->
      let* () = Lwt_io.write Lwt_io.stdout clear in
      let* () = Lwt_io.write_line Lwt_io.stdout resp in
      let* () = redraw_input () in
      receive_loop ic
  | None ->
      let* () = Lwt_io.write_line Lwt_io.stdout server_disconn_msg in
      Lwt.return_unit

(* Username validation loop *)
let rec username_loop ic oc =
  let* username = Lwt_io.read_line_opt Lwt_io.stdin in
  match username with
  | Some name ->
      if String.trim name = "" then
        let* () = Lwt_io.write Lwt_io.stdout (line_up ^ clear ^ "Please enter a username > ") in 
	username_loop ic oc
      else
	begin
          let* () = send_message oc name in
	  let* msg = receive_message ic in
	  match msg with
	  | Some resp ->
	      let* () = Lwt_io.write_line Lwt_io.stdout resp in
	      (* Check if username exists *)
	      if String.ends_with ~suffix:"exists." (String.trim resp) then
	        let* () = Lwt_io.write Lwt_io.stdout "Username > " in
	        username_loop ic oc
	      else Lwt.return_unit
	  | None -> Lwt.return_unit
	end
  | None -> Lwt.return_unit

(* "Main function" *)
let () =
  parse_command_line ();
  let term = Unix.tcgetattr Unix.stdin in
  Lwt_main.run begin
    Lwt.catch
      (fun () ->
        let server_ip = get_server_ip () in
        let* (ic, oc) = create_connection server_ip in
        let* _raw_term = setup_terminal ~echo:true () in  (* Enable echo for username *)
        let* () = Lwt_io.write_line Lwt_io.stdout (fresh_screen ^ welcome_msg) in
        let* () = Lwt_io.write Lwt_io.stdout "Username > " in
        let* () = username_loop ic oc in
	let* _chat_term = setup_terminal ~echo:false () in  (* Disable echo for chat *)
        Lwt.pick [
          input_loop oc;
          receive_loop ic
        ]
      )
      (fun _ -> 
        restore_terminal term;
        Lwt.return_unit
      )
  end;
  restore_terminal term
