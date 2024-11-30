val setup_raw_terminal : unit -> Unix.terminal_io
val restore_terminal : Unix.terminal_io -> unit

val clear : string

val current_input : string ref
val prompt : string ref

val cursor_pos : int ref

val redraw_input : unit -> unit Lwt.t
val input_loop : unit -> string option Lwt.t

