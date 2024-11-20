(*open Lwt*)
open Lwt_unix
open Lwt.Syntax
open Lwt.Infix

(**)
(*type client = { *)
(*  ic: Lwt_io.input_channel;*)
(*  oc: Lwt_io.output_channel;*)
(*  addr: Lwt_unix.sockaddr;*)
(*  username: string;*)
(*}*)

let counter = ref 0
let listen_address = Unix.inet_addr_loopback
let port = 9000
let backlog = 10

(* Keep track of all connected clients *)
let clients = ref []

(* Broadcast a message to all connected clients *)
let broadcast msg =
  Lwt_list.iter_p
    (fun oc ->
      Lwt.catch
        (fun () -> Lwt_io.write_line oc msg)
        (fun _ -> Lwt.return_unit))
    !clients

(* Server input handling loop *)
let rec server_input_loop () =
  let* () = Lwt_io.read_line_opt Lwt_io.stdin >>= function
    | Some "quit" -> Lwt.fail Exit
    | Some msg ->
        let* () = broadcast ("Server: " ^ msg) in
        Printf.printf "Broadcasted: %s\n" msg;
        Lwt.return_unit
    | None -> Lwt.return_unit
  in
  server_input_loop ()

let handle_message msg = 
  match msg with
  | "read" -> string_of_int !counter
  | "inc" -> counter := !counter + 1; "Incremented Counter"
  | _ -> Printf.sprintf "Client says: %s" msg

let rec handle_connection ic oc =
  let* msg = Lwt_io.read_line_opt ic in
  match msg with
  | Some msg ->
    let reply = handle_message msg in
    let _ = Lwt_list.map_p (fun c -> Lwt_io.write_line c reply) !clients in
    handle_connection ic oc
  | None -> 
    clients := List.filter (fun c -> c != oc) !clients;
    Printf.printf "Client disconnected. Total clients: %d\n" (List.length !clients);
    Lwt.return_unit

let accept_connection conn =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in

  clients := oc :: !clients;
  Printf.printf "New client connected. Total clients: %d\n" (List.length !clients);
  Lwt.async (fun () -> 
    Lwt.catch
      (fun () -> handle_connection ic oc)
      (fun _ -> 
        clients := List.filter (fun c -> c != oc) !clients;
        Lwt.return_unit
      )
  );
  Lwt.return_unit

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
      server_input_loop ()
    ])


(**)
(*type client = { *)
(*  ic: Lwt_io.input_channel;*)
(*  oc: Lwt_io.output_channel;*)
(*  addr: Lwt_unix.sockaddr;*)
(*  username: string;*)
(*}*)
(**)
(*let clients = ref []*)
(*let clients_mutex = Lwt_mutex.create () (* init unlocked *)*)
(**)
(**)
(*let init_serv ip port = *)
(*  let sock = Lwt_unix.socket PF_INET SOCK_STREAM 0 in*)
(*  Lwt_unix.setsockopt sock SO_REUSEADDR true; (*want to reuse addr? why *)*)
(**)
(*  let addr = Unix.(ADDR_INET(inet_addr_of_string ip, port)) in (*converts string ip to internal rep*)*)
(*  Lwt_unix.bind sock addr >>= fun () ->  (* binds into what function ? *)*)
(*    Lwt_unix.listen sock 100; (* backlog size of pending connects *)*)
(*    Lwt.return sock*)
(**)
(**)
(*let handle_conns ic oc addr = *)
(*  Lwt_io.write_line oc "test server" >>= fun () -> *)
(*    Lwt_io.read_line ic >>= fun msg ->*)
(*      Lwt_io.printlf "received: %s" msg >>= fun () -> *)
(*        Lwt_io.write_line oc "got message" >>= fun () -> *)
(*          Lwt_io.flush oc*)
(**)
(*let rec accept_connection sock =*)
(*  Lwt_unix.accept sock >>= fun (client_sock, client_addr) ->*)
(*    let ic = Lwt_io.of_fd ~mode:Input client_sock in*)
(*    let oc = Lwt_io.of_fd ~mode:Output client_sock in*)
(**)
(*    Lwt_io.printlf "Conn from: %s"*)
(*      (match client_addr with*)
(*      | ADDR_INET(addr, port) -> *)
(*        Printf.sprintf "%s:%d" (Unix.string_of_inet_addr addr) port*)
(*      | _ -> "Unknown") >>= fun () ->*)
(*       Lwt.async (fun () -> *)
(*    Lwt.catch*)
(*      (fun () -> handle_connection ic oc client_addr)*)
(*      (function*)
(*        | End_of_file -> *)
(*            Lwt_io.printl "Client disconnected" >>= fun () ->*)
(*            Lwt_io.close ic >>= fun () ->*)
(*            Lwt_io.close oc*)
(*        | e -> *)
(*            Lwt_io.printlf "Error: %s" (Printexc.to_string e) >>= fun () ->*)
(*            Lwt_io.close ic >>= fun () ->*)
(*            Lwt_io.close oc));*)
(*            accept_connections sock*)









(*let start_serv (ip: string) (port: int) : Lwt_unix.file_descr Lwt.t = *)
(*  init_serv ip port >>= fun server_sock -> *)
(*    Lwt_io.printlf "Server started on %s:%d" ip port >>= fun () ->*)
(*      Lwt.return server_sock*)
(**)
(*let run_server () =*)
(*  start_serv "127.0.0.1" 9000 >>= fun sock -> *)
