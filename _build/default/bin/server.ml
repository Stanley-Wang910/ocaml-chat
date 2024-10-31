open Lwt


(*shared mutable counter *)

let counter = ref 0

let listen_address = Unix.inet_addr_loopback
let port = 9000
let backlog = 10


let handle_message msg = 
  match msg with
  | "read" -> string_of_int !counter
  | "inc" -> counter := !counter + 1; "Incremented Counter"
  | _ -> "Unknown Command"
  



let rec handle_connection incoming_conn outgoing_conn () =
  Lwt_io.read_line_opt incoming_conn >>=
  (fun msg ->
    match msg with
    | Some msg ->
      let reply  = handle_message msg in
      Lwt_io.write_line outgoing_conn reply >>= handle_connection incoming_conn outgoing_conn
    | None -> Logs_lwt.info (fun m -> m "Connection Closed") >>= return)


let accept_connection conn =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  Lwt.on_failure (handle_connection ic oc ()) (fun e -> Logs.err (fun m -> m "%s" (Printexc.to_string e) ));
  Logs_lwt.info (fun m -> m "New Connection") >>= return


let create_socket () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  bind sock @@ ADDR_INET(listen_address, port) |> (fun x -> ignore x);
  listen sock backlog;
  sock


let create_server sock =
  let rec serve () =
    Lwt_unix.accept sock >>= accept_connection >>= serve
  in serve

