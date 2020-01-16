open! Core
open! Import
open! Packet_bin_helpers

type t = {
    packet_buffer : Write_buffer.t
  ; mutable compression : Compression.t
  ; mutable encryption : Encryption.Mode.encrypt Encryption.t
  ; mutable mac : Mac.t
  ; mutable sequence_number : Uint32.t
}

let create () =
  let packet_buffer = Write_buffer.create () in
  {
    packet_buffer
  ; compression = Compression.Method.(create none)
  ; encryption =
      Encryption.Method.(create_encrypt none ~initialization_vector:"" ~key:"")
  ; mac = Mac.Method.(create none ~key:"")
  ; sequence_number = Uint32.zero
  }
;;

let set_compression t compression = t.compression <- compression

let set_encryption t encryption = t.encryption <- encryption

let set_mac t mac = t.mac <- mac

let packet_buffer = Write_buffer.create ()

let padding_size size ~block_size =
  let padding = -size % block_size in
  if padding < 4 then padding + block_size else padding
;;

let generate_padding padding_size =
  Nocrypto.Rng.generate padding_size |> Cstruct.to_string
;;

let generate_message
    ({ packet_buffer; compression; encryption; mac; sequence_number } as t)
    message =
  (* Compress the message *)
  let message = Compression.compress compression message in
  (* Compute the padding *)
  let length = String.length message in
  (* length + uint8 for padding size *)
  let total_length_not_padded = length + 1 in
  (* total_length_not_padded + uint32 for unpadded size *)
  let padding_size =
    padding_size
      ~block_size:(Encryption.block_size encryption)
      (total_length_not_padded + 4)
  in
  Write_buffer.uint32 packet_buffer (total_length_not_padded + padding_size);
  Write_buffer.uint8 packet_buffer padding_size;
  Write_buffer.bytes packet_buffer message;
  Write_buffer.bytes packet_buffer (generate_padding padding_size);
  let unencrypted_message = Write_buffer.consume_to_string packet_buffer in
  (* Compute the mac *)
  let mac_message =
    generate_mac_string_to_verify ~sequence_number
      ~clean_work_buffer:packet_buffer unencrypted_message
  in
  let mac = Mac.signature mac mac_message in
  (* Increment the sequence number *)
  t.sequence_number <- Uint32.succ sequence_number;
  (* Encrypt the message *)
  let message = Encryption.encrypt encryption unencrypted_message in
  (* Write the message *)
  Write_buffer.bytes packet_buffer message;
  Write_buffer.bytes packet_buffer mac;
  Write_buffer.consume_to_string packet_buffer
;;
