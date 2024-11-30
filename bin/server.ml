open Lwt.Syntax

(* Escape characters for formatting *)
let clear_line = "\027[2K"
let move_cursor_left = "\027[1000D"
let clear = clear_line ^ move_cursor_left
let line_up = "\027[A"
let clear_screen = "\027[2J"
let move_cursor_home = "\027[H"
let fresh_screen = clear_screen ^ move_cursor_home

(* Colors for connection/transmission messages *)
let red = "\027[1;31m"
let green = "\027[1;32m"
let yellow = "\027[1;33m"
let white = "\027[1;37m"
let caml_orange = "\027[38;5;214m"
let default = "\027[39m"
let bold = "\027[1m"
let reset = "\027[0m"

(* user colors *)
let red_orange = "\027[38;5;196m"
let orange = "\027[38;5;208m"
let gold_yellow = "\027[38;5;220m"
let yellow_green = "\027[38;5;154m"
let better_green = "\027[38;5;42m"
let cyan = "\027[96m"
let blue = "\027[38;5;75m"
let purple = "\027[38;5;141m"
let magenta = "\027[95m"
let pink = "\027[38;5;219m"

(* Message for starting up the server *)
let startup_msg = bold ^ caml_orange ^ "Server running." ^ reset

(* The client type *)
type client = { 
  ic: Lwt_io.input_channel;
  oc: Lwt_io.output_channel;
  id: int;
  username: string;
  color: string
}

(* Indicate if some client or none (the server) broadcast a message *)
type broadcast_opt = Some of client | None

(* The server "structure" *)
let listen_address = Unix.inet_addr_any  
let port = 9000
let backlog = 5

(* To keep track of all connected clients *)
let client_id_counter = ref 0
let clients = ref []

(* Put all colors in an array to allow for easy access *)
let colors = [red_orange; orange; gold_yellow; yellow_green; better_green; cyan; blue; purple; magenta; pink]

(* Obtain the necessary color from the above list *)
let get_color () =
  List.nth colors ((List.length !clients) mod (List.length colors))

(* Broadcast a message to all connected clients *)
let broadcast msg b_opt =
  match b_opt with 
  | Some sender -> 
      Lwt_list.map_p
        (fun client ->
          if client.id <> sender.id then
            Lwt.catch
              (* Broadcast message to all other clients *)
              (fun () -> Lwt_io.write_line client.oc (clear ^ sender.color ^ sender.username ^ " > " ^ reset ^ msg))
              (fun _ -> Lwt.return_unit)
          else
            Lwt.catch
            (* Notify the client their message was sent *)
              (fun () -> Lwt_io.write_line client.oc (line_up ^ clear ^ white ^ "[SENT] " ^ reset ^ msg))
              (fun _ -> Lwt.return_unit)
        )
        !clients
  | None -> 
      (* Broadcast message to all clients (for server-end communication) *)
      Lwt_list.map_p
        (fun client ->
          Lwt.catch
            (fun () -> Lwt_io.write_line client.oc (Printf.sprintf "%s%s" clear msg))
            (fun _ -> Lwt.return_unit)
        )
        !clients

(* Connection handling loop *)
let rec handle_connection client =
  let* msg = Lwt_io.read_line_opt client.ic in
  match msg with
  | Some msg ->
      let _ = broadcast msg (Some client) in
      handle_connection client
  | None -> 
      clients := List.filter (fun c -> c.id <> client.id) !clients;
      let _ = broadcast (Printf.sprintf "%s%s%s disconnected. Total online: %d%s" client.color client.username yellow (List.length !clients) reset) None in
      Lwt.return_unit

(* Connection accepting loop *)
let rec accept_connection conn =
  let fd, _ = conn in
  (* Create I/O channels for client communication *)
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  let id = !client_id_counter in
  incr client_id_counter;
  Lwt.async (fun () -> 
    let* name = Lwt_io.read_line_opt ic in
    match name with
    | Some username ->
        (* Username validation to ensure client uniqueness *)
        if List.exists (fun name -> name = username) (List.map (fun client -> client.username) !clients) then
          let* () = Lwt_io.write_line oc (Printf.sprintf "%sThe username %s already exists.%s" yellow username reset) in
          accept_connection conn
        else
          (* Client initialization *)
          let color = get_color () in
          let client = { ic; oc; id; username; color } in
          clients := client :: !clients;
          let msg = Printf.sprintf "%s%s%s connected. Total online: %d%s" client.color client.username green (List.length !clients) reset in
          let* _ = broadcast msg None in
          Lwt.catch
            (* Start handling the connection *)
            (fun () -> handle_connection client)
            (* Remove the client once they disconnect *)
            (fun _ -> 
              clients := List.filter (fun c -> c.id <> client.id) !clients;
              Lwt.return_unit
            )
    | None -> Lwt.return_unit
  );
  Lwt.return_unit

(* Server input handling loop *)
let rec server_input_loop () =
  let* input = Input.input_loop () in
  match input with
  | Some msg ->
      if String.lowercase_ascii msg = "quit" then
        Lwt.return_unit
      else if msg = "" then
        let* () = Lwt_io.write Lwt_io.stdout (line_up ^ clear) in
        server_input_loop ()
      else
        let* () = Lwt_io.write_line Lwt_io.stdout ("Broadcasted: " ^ msg) in
        let* _ = broadcast (red ^ "[SERVER] " ^ default ^ msg ^ reset) None in
        server_input_loop ()
  | None -> Lwt.return_unit

(* Create a quickly reusable socket for connections *)
let create_socket () =
  let sock = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
  Lwt_unix.setsockopt sock SO_REUSEADDR true;
  let _ = Lwt_unix.bind sock @@ ADDR_INET(listen_address, port) in
  Lwt_unix.listen sock backlog;
  sock

(* Create the server that supports multiple clients *)
let create_server sock =
  let rec serve () =
    let* conn = Lwt_unix.accept sock in
    let* _ = accept_connection conn in
    serve ()
  in 
  serve

(* "Main function" *)
let () =
  let term = Unix.tcgetattr Unix.stdin in
  let sock = create_socket () in
  let _ = Lwt_io.write_line Lwt_io.stdout (fresh_screen ^ startup_msg) in
  (* Enable character-by-character input *)
  let _raw_term = Input.setup_raw_terminal () in
  Input.prompt := "";
  Lwt_main.run
    begin
      Lwt.catch
        (fun () ->
          Lwt.pick [
            create_server sock ();
            server_input_loop ()
          ]
        )
        (fun _ ->
          Input.restore_terminal term;
          Lwt.return_unit
        )
    end;
  Input.restore_terminal term
