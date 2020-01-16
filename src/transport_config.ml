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

let generate_cookie () = Nocrypto.Rng.generate 16 |> Cstruct.to_string

let write t ~cookie packet_message =
  Write_buffer.message_id packet_message Key_exchange_init;
  Write_buffer.bytes packet_message cookie;
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
  ;;
end

module Negotiated_algorithms = struct
  type 'mode encryption =
    initialization_vector:string -> key:string -> 'mode Encryption.t

  type mac = key:string -> Mac.t

  type t = {
      kex : Kex.t
    ; encryption_c_to_s : Encryption.Method.t
    ; encryption_s_to_c : Encryption.Method.t
    ; mac_c_to_s : Mac.Method.t
    ; mac_s_to_c : Mac.Method.t
    ; compression_c_to_s : Compression.Method.t
    ; compression_s_to_c : Compression.Method.t
  }
end

let negotiate_algorithm client server name =
  List.find client ~f:(fun client ->
      List.exists server ~f:(fun server -> String.equal (name client) server))
;;

let negotiate t (received : Received.t) =
  print_endline "negotiating";
  let open Option.Let_syntax in
  let%bind kex =
    negotiate_algorithm t.kex received.kex Kex.Method.name
    |> Option.map ~f:Kex.Method.create
  in
  let%bind encryption_c_to_s =
    negotiate_algorithm t.encryption received.encryption_c_to_s
      Encryption.Method.name
  in
  let%bind encryption_s_to_c =
    negotiate_algorithm t.encryption received.encryption_s_to_c
      Encryption.Method.name
  in
  let%bind mac_c_to_s =
    negotiate_algorithm t.mac received.mac_c_to_s Mac.Method.name
  in
  let%bind mac_s_to_c =
    negotiate_algorithm t.mac received.mac_s_to_c Mac.Method.name
  in
  let%bind compression_c_to_s =
    negotiate_algorithm t.compression received.compression_c_to_s
      Compression.Method.name
  in
  let%map compression_s_to_c =
    negotiate_algorithm t.compression received.compression_s_to_c
      Compression.Method.name
  in
  Negotiated_algorithms.
    {
      kex
    ; encryption_c_to_s
    ; encryption_s_to_c
    ; mac_c_to_s
    ; mac_s_to_c
    ; compression_c_to_s
    ; compression_s_to_c
    }
;;
