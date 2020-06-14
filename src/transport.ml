open! Core
open! Async

let ssh_version = "2.0"

let software_version = "0.1"

let implementation = sprintf "ocaml_ssh_%s" software_version

let message = sprintf "SSH-%s-%s nocomment\r\n" ssh_version implementation

type t = { state : State.t; closed : unit Deferred.t } [@@deriving fields]

let debug_raw = false

let create ~transport_config ~where_to_connect =
  let%bind () = Init_rng.reseed () in
  let%bind _socket, reader, writer = Tcp.connect where_to_connect in
  let send s =
    if debug_raw then (
      printf "sending %d bytes\n" (String.length s);
      String.Hexdump.to_string_hum s |> print_endline );
    Writer.write writer s
  in
  send message;
  let packet_writer = Packet_writer.create () in
  let packet_reader = Packet_reader.create () in
  let packet_message = Write_buffer.create () in
  let connection_established = Ivar.create () in
  let%bind s = Reader.read_line reader in
  let server_identification =
    match s with `Ok s -> s | `Eof -> failwith "EOF"
  in
  let state =
    State.create transport_config
      {
        send_message =
          (fun writer ->
            let (`Write_complete result) = writer packet_message in
            Write_buffer.consume_to_string packet_message
            |> Packet_writer.generate_message packet_writer
            |> send;
            result)
      }
      (fun update -> update packet_writer)
      (fun update -> update packet_reader)
      ~on_connection_established:(Ivar.fill connection_established)
      ~server_identification
  in
  let closed =
    Reader.pipe reader
    |> Pipe.iter_without_pushback ~f:(fun s ->
           if debug_raw then (
             printf "received %d bytes\n" (String.length s);
             String.Hexdump.to_string_hum s |> print_endline );
           Packet_reader.process_string packet_reader s
             ~f:(fun ~sequence_number ~payload ->
             function
             | `Failed_to_verify ->
                 print_s [%message "failed to verify" (sequence_number : int)]
             | `Message buffer -> State.handle_message ~payload state buffer))
  in
  let%map.Deferred.Or_error () = Ivar.read connection_established in
  { state; closed }
;;

let request_auth { state; _ } =
  State.request_service state ~service_name:User_auth.service_name
;;

let request_auth_none ~username { state; _ } =
  State.request_auth state ~username User_auth.Mode.none
;;

let request_password ~username ~password { state; _ } =
  State.request_auth state ~username (User_auth.Mode.password password)
;;

let request_keyboard_interactive ~username ?(submethods = []) ~respond
    { state; _ } =
  State.request_auth state ~username
    (User_auth.Mode.keyboard_interactive ~submethods ~respond)
;;

let request_channel { state; _ } = Channel.create state
