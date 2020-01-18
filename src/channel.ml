open! Core
open! Async

module Request_pipe = struct
  type t = unit Or_error.t Pipe.Reader.t * unit Or_error.t Pipe.Writer.t
  [@@deriving sexp_of]

  let create = Pipe.create

  let wait (reader, _) =
    match%map Pipe.read reader with
    | `Ok response -> response
    | `Eof -> error_s [%message "Received EOF before response"]
  ;;

  let register_success (_, writer) = Pipe.write_without_pushback writer (Ok ())

  let register_failure (_, writer) =
    Pipe.write_without_pushback writer (error_s [%message "Request failed"])
  ;;
end

type t = {
    id : int
  ; window_size : int
  ; server_id : int
  ; server_max_packet_size : int
  ; mutable server_window_size : int
  ; state : (State.t[@sexp.opaque])
  ; data_pipe : (string Pipe.Reader.t[@sexp.opaque])
  ; stderr_pipe : (string Pipe.Reader.t[@sexp.opaque])
  ; exit_status : Unix.Exit.t Set_once.t
  ; request_responses : Request_pipe.t
}
[@@deriving sexp_of]

let create state =
  let%map id, { server_max_packet_size; server_window_size; server_id }, reader
      =
    State.request_session_channel state
  in
  let data_pipe, data_write = Pipe.create () in
  let stderr_pipe, stderr_write = Pipe.create () in
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
    ; request_responses = Request_pipe.create ()
    ; stderr_pipe
    }
  in
  Pipe.iter_without_pushback reader ~f:(function
    | Adjust bytes ->
        print_s [%message "Adjusting window size" (bytes : int)];
        t.server_window_size <- t.server_window_size + bytes
    | Data data -> Pipe.write_without_pushback data_write data
    | Stderr data -> Pipe.write_without_pushback stderr_write data
    | Eof -> Pipe.close data_write
    | Close ->
        Pipe.close data_write;
        State.close_channel state id
    | Request (Exit_status exit_status) ->
        Set_once.set_exn t.exit_status [%here] exit_status
    | Request request ->
        print_s
          [%message "Received request" (request : Channel_request.Request.t)]
    | Success -> Request_pipe.register_success t.request_responses
    | Failure -> Request_pipe.register_failure t.request_responses)
  |> don't_wait_for;
  t
;;

let request_shell t =
  State.request_shell t.state t.id ~server_id:t.server_id;
  let%map.Deferred.Or_error () = Request_pipe.wait t.request_responses in
  let read_pipe_reader, read_pipe_writer = Pipe.create () in
  (* TODO end of input *)
  Pipe.iter_without_pushback read_pipe_reader ~f:(fun data ->
      State.send_data t.state t.id ~server_id:t.server_id ~data)
  |> don't_wait_for;
  (t.data_pipe, t.stderr_pipe, read_pipe_writer)
;;

let request_pty t ~term ~width ~height =
  State.request_pty t.state t.id ~server_id:t.server_id ~term ~width ~height;
  Request_pipe.wait t.request_responses
;;

let request_exec t ~command =
  State.request_exec t.state t.id ~server_id:t.server_id ~command;
  let%map result = Pipe.to_list t.data_pipe in
  (String.concat result, Set_once.get t.exit_status)
;;
