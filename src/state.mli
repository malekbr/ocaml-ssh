open! Core
open! Async

type t

type send_message = {
    send_message : 'a. (Write_buffer.t -> [ `Write_complete of 'a ]) -> 'a
}
[@@unboxed]

val create :
     Transport_config.t
  -> send_message
  -> update_packet_writer:((Packet_writer.t -> unit) -> unit)
  -> update_packet_reader:((Packet_reader.t -> unit) -> unit)
  -> on_keys_exchanged:(unit Or_error.t -> unit)
  -> server_identification:string
  -> t

val handle_message : payload:string -> t -> Read_buffer.t -> unit

val request_service : t -> service_name:string -> unit Deferred.t

val request_auth :
  t -> username:string -> User_auth.Mode.t -> User_auth.Auth_result.t Deferred.t

val request_session_channel :
     t
  -> ( int
     * Channel_request.Confirmation.t
     * Channel_request.Server_message.t Pipe.Reader.t )
     Deferred.t

val request_shell : t -> int -> server_id:int -> unit

val request_pty :
  t -> int -> server_id:int -> term:string -> width:int -> height:int -> unit

val request_exec : t -> int -> server_id:int -> command:string -> unit

val send_data : t -> int -> server_id:int -> data:string -> unit

val close_channel : t -> int -> unit
