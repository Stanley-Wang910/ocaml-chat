(*open Lwt*)
open Lwt_unix
open Lwt.Syntax
(*open Lwt.Infix*)

(**)
type client = { 
  ic: Lwt_io.input_channel;
  oc: Lwt_io.output_channel;
  id: int;
  username: string;
}

type broadcast_opt = Some of client | None

(*let counter = ref 0*)
let listen_address = Unix.inet_addr_loopback
let port = 9000
let backlog = 5

(* Keep track of all connected clients *)
let client_id_counter = ref 0
let clients = ref []

(* Broadcast a message to all connected clients *)
let broadcast msg b_opt = match b_opt with 
  | Some sender -> 
  (Lwt_list.map_p
    (fun client ->
      if client.id <> sender.id then
      Lwt.catch
        (fun () -> Lwt_io.write_line client.oc (Printf.sprintf "\n%s > %s\n" sender.username msg))
        (fun _ -> Lwt.return_unit)
      else
      (*Lwt.catch*)
      (*  (fun () -> Lwt_io.write_line client.oc (Printf.sprintf "\nyou > %s\n" msg))*)
        Lwt_io.write_line sender.oc ("\nsent\n")
        )
    !clients)
| None -> 
  (Lwt_list.map_p
    (fun client ->
      Lwt.catch
        (fun () -> Lwt_io.write_line client.oc (Printf.sprintf "\n%s\n" msg))
        (fun _ -> Lwt.return_unit))
        !clients)

(*let handle_message msg = *)
(*  match msg with*)
(*  | "read" -> string_of_int !counter*)
(*  | "inc" -> counter := !counter + 1; "Incremented Counter"*)
(*  | _ -> msg*)

let rec handle_connection client =
  let* msg = Lwt_io.read_line_opt client.ic in
  match msg with
  | Some msg ->
    let _ = broadcast msg (Some client) in
    handle_connection client
  | None -> 
    clients := List.filter (fun c -> c.id <> client.id) !clients;
    Printf.printf "Client %d disconnected. Total clients: %d\n" client.id (List.length !clients);
    Lwt.return_unit


let accept_connection conn =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  let id = !client_id_counter in
  incr client_id_counter;
  Lwt.async (fun () -> 
    let* username = Lwt_io.read_line ic in
    let* () = Lwt_io.write_line oc (Printf.sprintf "Welcome to the chat %s!" username) in  (* Changed this line *)
    let client = { ic; oc; id ; username } in 
    clients := client :: !clients;
    let msg = Printf.sprintf "%s connected. Total online: %d" client.username (List.length !clients) in
    let* _ = broadcast msg None in
  
    Lwt.catch
      (fun () -> handle_connection client)
      (fun _ -> 
        clients := List.filter (fun c -> c.id <> client.id) !clients;
        Lwt.return_unit
      )
  );
  Lwt.return_unit
  
(*(* Server input handling loop *)*)
(*let rec server_input_loop () =*)
(*  let* () = Lwt_io.read_line_opt Lwt_io.stdin >>= function*)
(*    | Some "quit" -> Lwt.fail Exit*)
(*    | Some msg ->*)
(*        let* () = Lwt_io.write_line Lwt_io.stdout ("Server: " ^ msg) in*)
(*        Printf.printf "Broadcasted: %s\n" msg;*)
(*        Lwt.return_unit*)
(*    | None -> Lwt.return_unit*)
(*  in*)
(*  server_input_loop ()*)




let create_socket () =
  let sock = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
  let _ = Lwt_unix.bind sock @@ ADDR_INET(listen_address, port) in
  Lwt_unix.listen sock backlog;
  sock

let create_server sock =
  let rec serve () =
    let* conn = Lwt_unix.accept sock in
    let* _ = accept_connection conn in
    serve ()
  in serve

let () =
  let sock = create_socket () in
  Lwt_main.run
    (Lwt.pick [
      create_server sock ();
      (*server_input_loop ()*)
    ])

