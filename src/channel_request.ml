open! Core
open! Async
open! Import

module Id_generator = struct
  type t = Uint32.t ref

  let create () = ref Uint32.zero

  let generate_id t =
    let id = !t in
    (t :=
       let next = Uint32.succ id in
       if Uint32.compare next Uint32.zero = 0 then
         raise_s [%message "Channel id overflowed"]
       else next);
    Uint32.to_int id
  ;;
end

module Request = struct
  type t = Exit_status of Unix.Exit.t | Unknown [@@deriving sexp_of]
end

module Server_message = struct
  type t =
    | Adjust of int
    | Success
    | Failure
    | Data of string
    | Stderr of string
    | Request of Request.t
    | Eof
    | Close
end

module Confirmation = struct
  type t = {
      server_id : int
    ; server_window_size : int
    ; server_max_packet_size : int
  }
  [@@deriving sexp_of]
end

let initial_window_size = Uint32.(to_int max_int)

let request_generic_create generator ~channel_type write_buffer =
  let id = Id_generator.generate_id generator in
  Write_buffer.message_id write_buffer Channel_open;
  Write_buffer.string write_buffer channel_type;
  Write_buffer.uint32 write_buffer id;
  Write_buffer.uint32 write_buffer initial_window_size;
  Write_buffer.uint32 write_buffer Write_buffer.max_size;
  id
;;

let request_channel_session_create =
  request_generic_create ~channel_type:"session"
;;

let handle_confirmation read_buffer =
  let server_id = Read_buffer.uint32 read_buffer in
  let id = Read_buffer.uint32 read_buffer in
  let server_window_size = Read_buffer.uint32 read_buffer in
  let server_max_packet_size = Read_buffer.uint32 read_buffer in
  (id, Confirmation.{ server_id; server_window_size; server_max_packet_size })
;;

let request_pty ~server_id ~want_reply ~term ~width ~height write_buffer =
  Write_buffer.message_id write_buffer Channel_request;
  Write_buffer.uint32 write_buffer server_id;
  Write_buffer.string write_buffer "pty-req";
  Write_buffer.bool write_buffer want_reply;
  Write_buffer.string write_buffer term;
  Write_buffer.uint32 write_buffer width;
  Write_buffer.uint32 write_buffer height;
  Write_buffer.uint32 write_buffer 0;
  Write_buffer.uint32 write_buffer 0;
  Write_buffer.string write_buffer "\x00"
;;

let request_shell ~server_id ~want_reply write_buffer =
  Write_buffer.message_id write_buffer Channel_request;
  Write_buffer.uint32 write_buffer server_id;
  Write_buffer.string write_buffer "shell";
  Write_buffer.bool write_buffer want_reply
;;

let request_exec ~server_id ~want_reply ~command write_buffer =
  Write_buffer.message_id write_buffer Channel_request;
  Write_buffer.uint32 write_buffer server_id;
  Write_buffer.string write_buffer "exec";
  Write_buffer.bool write_buffer want_reply;
  Write_buffer.string write_buffer command
;;

let send_data ~server_id ~data write_buffer =
  Write_buffer.message_id write_buffer Channel_data;
  Write_buffer.uint32 write_buffer server_id;
  Write_buffer.string write_buffer data
;;

let handle_window_adjust read_buffer =
  let id = Read_buffer.uint32 read_buffer in
  let byte_count = Read_buffer.uint32 read_buffer in
  (id, Server_message.Adjust byte_count)
;;

let handle_success read_buffer =
  let id = Read_buffer.uint32 read_buffer in
  (id, Server_message.Success)
;;

let handle_failure read_buffer =
  let id = Read_buffer.uint32 read_buffer in
  (id, Server_message.Failure)
;;

let handle_data read_buffer =
  let id = Read_buffer.uint32 read_buffer in
  let data = Read_buffer.string read_buffer in
  (id, Server_message.Data data)
;;

let handle_extended_data read_buffer =
  let id = Read_buffer.uint32 read_buffer in
  let stream = Read_buffer.uint32 read_buffer in
  assert (stream = 1);
  let data = Read_buffer.string read_buffer in
  (id, Server_message.Stderr data)
;;

let handle_eof read_buffer =
  let id = Read_buffer.uint32 read_buffer in
  (id, Server_message.Eof)
;;

let handle_close read_buffer =
  let id = Read_buffer.uint32 read_buffer in
  (id, Server_message.Close)
;;

let handle_request read_buffer =
  let id = Read_buffer.uint32 read_buffer in
  let request = Read_buffer.string read_buffer in
  let _want_reply = Read_buffer.bool read_buffer in
  let request =
    match request with
    | "exit-status" ->
        let exit_status = Read_buffer.uint32 read_buffer |> Unix.Exit.of_code in
        Request.Exit_status exit_status
    | _ -> Request.Unknown
  in
  (id, Server_message.Request request)
;;
