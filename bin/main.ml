let () =
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  let socket = Server.create_socket () in
  let serve = Server.create_server socket in
  Lwt_main.run @@ serve ()
