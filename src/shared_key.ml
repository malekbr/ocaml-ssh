open! Core

let rec compute_until kex ~scratch_pad ~size ~key ~hash data =
  if String.length data >= size then String.prefix data size
  else (
    Write_buffer.mpint scratch_pad key;
    Write_buffer.bytes scratch_pad hash;
    Write_buffer.bytes scratch_pad data;
    let addition = Write_buffer.consume_to_string scratch_pad |> Kex.hash kex in
    let data = data ^ addition in
    compute_until kex ~scratch_pad ~size ~key ~hash data )
;;

let compute middle kex (kex_result : Kex.Kex_result.t) ~scratch_pad ~session_id
    ~size =
  let key = kex_result.shared_key in
  let hash = kex_result.shared_hash in
  Write_buffer.mpint scratch_pad key;
  Write_buffer.bytes scratch_pad hash;
  Write_buffer.byte scratch_pad middle;
  Write_buffer.bytes scratch_pad session_id;
  let hash =
    Write_buffer.consume_to_string scratch_pad
    |> Kex.hash kex
    |> compute_until kex ~scratch_pad ~size ~key ~hash
  in
  print_s [%message (hash : String.Hexdump.t) (middle : char)];
  hash
;;

let compute_iv_c_to_s = compute 'A'

let compute_iv_s_to_c = compute 'B'

let compute_enc_key_c_to_s = compute 'C'

let compute_enc_key_s_to_c = compute 'D'

let compute_mac_key_c_to_s = compute 'E'

let compute_mac_key_s_to_c = compute 'F'
