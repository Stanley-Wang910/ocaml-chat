open Lwt.Syntax

(* Set up raw mode for terminal *)
let setup_raw_terminal () =
  let open Unix in
  let term = tcgetattr stdin in
  let raw_term = {term with c_icanon = false; c_echo = false} in
  tcsetattr stdin TCSAFLUSH raw_term;
  term

(* Restore terminal settings *)
let restore_terminal term =
  let open Unix in
  tcsetattr stdin TCSAFLUSH term

let clear = "\027[2K" ^ "\027[1000D"

let current_input = ref ""
let prompt = ref "> "

(* Track cursor position *)
let cursor_pos = ref (String.length !current_input)

(* Redraw input line with cursor positioning *)
let redraw_input () = 
  let len = String.length !prompt in
  let* () = 
    if len > 2 then
      Lwt_io.write Lwt_io.stdout (clear ^ "\027[1;37m" ^ !prompt ^ "\027[0m" ^ !current_input)
    else
      Lwt_io.write Lwt_io.stdout (clear ^ !prompt ^ !current_input)
  in
  (* Move the cursor to the correct position *)
  let move_cursor = Printf.sprintf "\027[%dG" (!cursor_pos + len + 1) in 
  let* () = Lwt_io.write Lwt_io.stdout move_cursor in
  Lwt_io.flush Lwt_io.stdout


(* General input handling loop *)
let rec input_loop () =

  (* Parse character *)
  let read_char_sequence () =
    let* char = Lwt_io.read_char_opt Lwt_io.stdin in
    match char with
    (* Escape sequence *)
    | Some '\027' ->
        let* next1 = Lwt_io.read_char_opt Lwt_io.stdin in
	(* OPTION modifiers *)
	if next1 = Some '\027' then
	  let* _ = Lwt_io.read_char_opt Lwt_io.stdin in
	  let* _ = Lwt_io.read_char_opt Lwt_io.stdin in
	  Lwt.return (Some "NA")
	(* F1-F4 *)
	else if next1 = Some 'O' then
	  let* _ = Lwt_io.read_char_opt Lwt_io.stdin in
	  Lwt.return (Some "NA")
	(* Other sequences *)
        else if next1 = Some '[' then
          let* next2 = Lwt_io.read_char_opt Lwt_io.stdin in
          match next2 with
          | Some 'D' -> Lwt.return (Some "LEFT")  (* Left arrow *)
          | Some 'C' -> Lwt.return (Some "RIGHT")  (* Right arrow *)
          | Some c ->
	      if Char.code c > 47 && Char.code c < 55 then
		begin
                  let* next3 = Lwt_io.read_char_opt Lwt_io.stdin in
	          match next3 with
		  (* Delete key *)
                  | Some '~' when c = '3' ->  
                      Lwt.return (Some "DELETE")
		  (* CTRL/COMMAND or SHIFT modifiers, and F13-F16 *)
                  | Some ';' -> 
		      let* _ = Lwt_io.read_char_opt Lwt_io.stdin in
		      let* _ = Lwt_io.read_char_opt Lwt_io.stdin in
		      Lwt.return (Some "NA")
		  | Some c' ->
		    if Char.code c' > 47 && Char.code c' < 58 then 
		      let* next4 = Lwt_io.read_char_opt Lwt_io.stdin in
		      (* F5-F12 *)
		      if next4 = Some '~' then
                        Lwt.return (Some "NA")
		      (* F17-F19 *)
		      else if next4 = Some ';' then
		        let* _ = Lwt_io.read_char_opt Lwt_io.stdin in
		        let* _ = Lwt_io.read_char_opt Lwt_io.stdin in
		        Lwt.return (Some "NA")
		      else  (* Everything else *)
			Lwt.return (Some "NA")
		    else  (* More everything else *)
	      	      Lwt.return (Some "NA")
	          | None -> Lwt.return None
		end
	      else          
		Lwt.return (Some "NA")  (* Even more everything else *)
	  | None -> Lwt.return None
        else
          Lwt.return (Some "NA") (* Everything else that was not everything else before *)
    | Some c -> Lwt.return (Some (String.make 1 c)) (* Normal character *)
    | None -> Lwt.return None
  in

  (* Handle character *)
  let* seq = read_char_sequence () in
  match seq with
  | Some "LEFT" ->
      if !cursor_pos > 0 then
        cursor_pos := !cursor_pos - 1;
      let* () = redraw_input () in
      input_loop ()
  | Some "RIGHT" ->
      if !cursor_pos < String.length !current_input then
        cursor_pos := !cursor_pos + 1;
      let* () = redraw_input () in
      input_loop ()
  | Some "DELETE" ->
      if !cursor_pos < String.length !current_input then
        begin
          current_input := 
            String.sub !current_input 0 !cursor_pos ^
            String.sub !current_input (!cursor_pos + 1) (String.length !current_input - !cursor_pos - 1);
          let* () = redraw_input () in
          input_loop ()
        end
      else
        input_loop ()
  | Some "NA" ->
      input_loop ()
  | Some "\127" -> (* Backspace *)
      if !cursor_pos > 0 then
        begin
          current_input := 
            String.sub !current_input 0 (!cursor_pos - 1) ^
            String.sub !current_input !cursor_pos (String.length !current_input - !cursor_pos);
          cursor_pos := !cursor_pos - 1;
          let* () = redraw_input () in
          input_loop ()
        end
      else
        input_loop ()
  | Some "\n" ->
      let input = !current_input in
      current_input := "";
      cursor_pos := 0;
      let* () = Lwt_io.write_line Lwt_io.stdout "" in
      Lwt.return (Some (String.trim input))
  | Some str ->
      if String.length str = 1 && Char.code str.[0] >= 32 then
        begin
          current_input := 
            String.sub !current_input 0 !cursor_pos ^ str ^
            String.sub !current_input !cursor_pos (String.length !current_input - !cursor_pos);
          cursor_pos := !cursor_pos + 1;
	  let* () = Lwt_io.write Lwt_io.stdout str in
          let* () = redraw_input () in
          input_loop ()
        end
      else
        input_loop ()
  | None -> Lwt.return None
