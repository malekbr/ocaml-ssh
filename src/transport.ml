open! Core
open! Async

let ssh_version = "2.0"

let software_version = "0.1"

let implementation = sprintf "ocaml_ssh_%s" software_version

let message = sprintf "SSH-%s-%s nocomment\r\n" ssh_version implementation

module Buffers = struct
  type t = {
      send_buffer : Buffer.t
    ; input_buffer : Buffer.t
    ; work_buffer : Buffer.t
  }
end

module Sender = struct
  type t = { buffers : Buffers.t }
end

let tcp_connection ~where_to_connect =
  Nocrypto.Rng.reseed (Cstruct.of_string "hellothere");
  let%bind _socket, reader, writer = Tcp.connect where_to_connect in
  let send s =
    printf "sending %d bytes\n" (String.length s);
    String.Hexdump.to_string_hum s |> print_endline;
    Writer.write writer s
  in
  send message;
  let packet_writer = Packet_writer.create () in
  let packet_reader = Packet_reader.create () in
  let packet_message = Write_buffer.create () in
  let state =
    State.create Transport_config.default (fun writer ->
        writer packet_message;
        Write_buffer.consume_to_string packet_message
        |> Packet_writer.generate_message packet_writer
        |> send)
  in
  let%bind s = Reader.read_line reader in
  print_s [%sexp (s : string Reader.Read_result.t)];
  Reader.pipe reader
  |> Pipe.iter_without_pushback ~f:(fun s ->
         (* TODO malekbr: fix this *)
         printf "received %d bytes\n" (String.length s);
         String.Hexdump.to_string_hum s |> print_endline;
         Packet_reader.process_string packet_reader s
           ~f:(fun ~sequence_number ->
           function
           | `Failed_to_verify ->
               print_s [%message "failed to verify" (sequence_number : int)]
           | `Message buffer -> State.handle_message state buffer))
;;
