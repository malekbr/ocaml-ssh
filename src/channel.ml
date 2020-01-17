open! Core
open! Async

type t = {
    id : int
  ; mutable window_size : int
  ; server_id : int
  ; server_max_packet_size : int
  ; server_window_size : int
  ; state : (State.t[@sexp.opaque])
  ; data_pipe : string Pipe.Reader.t
  ; exit_status : Unix.Exit.t Set_once.t
}
[@@deriving sexp_of]

let create state =
  let%map id, { server_max_packet_size; server_window_size; server_id }, reader
      =
    State.request_session_channel state
  in
  let data_pipe, data_write = Pipe.create () in
  let t =
    {
      id
    ; server_id
    ; server_max_packet_size
    ; server_window_size
    ; state
    ; window_size = Channel_request.initial_window_size
    ; data_pipe
    ; exit_status = Set_once.create ()
    }
  in
  Pipe.iter_without_pushback reader ~f:(function
    | Adjust bytes -> t.window_size <- t.window_size + bytes
    | Data data -> Pipe.write_without_pushback data_write data
    | Eof -> Pipe.close data_write
    | Close ->
        Pipe.close data_write;
        State.close_channel state id
    | Request (Exit_status exit_status) ->
        Set_once.set_exn t.exit_status [%here] exit_status
    | Request request ->
        print_s
          [%message "Received request" (request : Channel_request.Request.t)]
    | Success -> (* TODO handle success *) ())
  |> don't_wait_for;
  t
;;

let request_pty t ~term ~width ~height =
  State.request_pty t.state t.id ~server_id:t.server_id ~term ~width ~height
;;

let request_exec t ~command =
  State.request_exec t.state t.id ~server_id:t.server_id ~command;
  let%map result = Pipe.to_list t.data_pipe in
  (String.concat result, Set_once.get t.exit_status)
;;
