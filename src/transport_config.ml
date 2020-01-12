open! Core

type t = {
    kex : Kex.Method.t list
  ; public_keys : Public_key_algorithm.t list
  ; encryption : Encryption.Method.t list
  ; mac : Mac.Method.t list
  ; compression : Compression.Method.t list
}

let default =
  {
    kex = Kex.Method.all
  ; public_keys = Public_key_algorithm.all
  ; encryption = Encryption.Method.all
  ; mac = Mac.Method.all
  ; compression = Compression.Method.all
  }
;;

let write t packet_message =
  Write_buffer.uint8 packet_message 20;
  Nocrypto.Rng.generate 16 |> Cstruct.to_string
  |> Write_buffer.bytes packet_message;
  let kex = List.map t.kex ~f:Kex.Method.name in
  let public_keys = List.map t.public_keys ~f:Public_key_algorithm.name in
  let encryption = List.map t.encryption ~f:Encryption.Method.name in
  let mac = List.map t.mac ~f:Mac.Method.name in
  let compression = List.map t.compression ~f:Compression.Method.name in
  Write_buffer.name_list packet_message kex;
  Write_buffer.name_list packet_message public_keys;
  Write_buffer.name_list packet_message encryption;
  Write_buffer.name_list packet_message encryption;
  Write_buffer.name_list packet_message mac;
  Write_buffer.name_list packet_message mac;
  Write_buffer.name_list packet_message compression;
  Write_buffer.name_list packet_message compression;
  Write_buffer.name_list packet_message [];
  Write_buffer.name_list packet_message [];
  Write_buffer.bool packet_message false;
  Write_buffer.uint32 packet_message 0
;;

module Received = struct
  type t = {
      kex : string list
    ; public_keys : string list
    ; encryption_c_to_s : string list
    ; encryption_s_to_c : string list
    ; mac_c_to_s : string list
    ; mac_s_to_c : string list
    ; compression_c_to_s : string list
    ; compression_s_to_c : string list
    ; first_kex_follows : bool
  }
  [@@deriving sexp_of]

  let parse buffer =
    let _cookie = Read_buffer.bytes buffer ~len:16 in
    let kex = Read_buffer.name_list buffer in
    let public_keys = Read_buffer.name_list buffer in
    let encryption_c_to_s = Read_buffer.name_list buffer in
    let encryption_s_to_c = Read_buffer.name_list buffer in
    let mac_c_to_s = Read_buffer.name_list buffer in
    let mac_s_to_c = Read_buffer.name_list buffer in
    let compression_c_to_s = Read_buffer.name_list buffer in
    let compression_s_to_c = Read_buffer.name_list buffer in
    let _lang_c_to_s = Read_buffer.name_list buffer in
    let _s_to_c = Read_buffer.name_list buffer in
    let first_kex_follows = Read_buffer.bool buffer in
    let t =
      {
        kex
      ; public_keys
      ; encryption_c_to_s
      ; encryption_s_to_c
      ; mac_c_to_s
      ; mac_s_to_c
      ; compression_c_to_s
      ; compression_s_to_c
      ; first_kex_follows
      }
    in
    print_s [%message (t : t)];
    t
  ;;
end

let chosen_kex t (received : Received.t) =
  print_endline "choosing kex";
  List.find t.kex ~f:(fun kex ->
      List.exists received.kex ~f:(fun server_kex ->
          String.equal (Kex.Method.name kex) server_kex))
  |> Option.map ~f:Kex.Method.create
;;
