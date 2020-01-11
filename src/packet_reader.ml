open! Core
open! Import
open! Packet_bin_helpers

module State = struct
  type t =
    (* Writing to work buffer the decryption *)
    | Building_decryption of { size : int option }
    (* work buffer not used *)
    | Waiting_for_mac of { compressed_message : string }
  [@@deriving sexp_of]
end

type t = {
    work_buffer : Write_buffer.t
  ; input_buffer : Growable_buffer.t
  ; mutable read_buffer : Read_buffer.t
  ; mutable state : State.t
  ; mutable compression : Compression.t
  ; mutable encryption : Encryption.t
  ; mutable mac : Mac.t
  ; mutable sequence_number : Uint32.t
}

let available_in_work_buffer t =
  Write_buffer.wrote t.work_buffer - Read_buffer.read t.read_buffer
;;

let create () =
  let work_buffer = Write_buffer.create () in
  {
    work_buffer
  ; input_buffer = Growable_buffer.create ()
  ; state = Building_decryption { size = None }
  ; read_buffer = Write_buffer.read_buffer work_buffer
  ; compression = Compression.Method.(create none)
  ; encryption =
      Encryption.Method.(create none ~initialization_vector:"" ~key:"")
  ; mac = Mac.Method.(create none ~key:"")
  ; sequence_number = Uint32.zero
  }
;;

let reset_work_buffer t =
  Write_buffer.reset t.work_buffer;
  t.read_buffer <- Write_buffer.read_buffer t.work_buffer
;;

let set_compression t compression = t.compression <- compression

let set_encryption t compression = t.compression <- compression

let set_mac t compression = t.compression <- compression

let request_next_block t =
  Growable_buffer.request t.input_buffer
    ( match t.state with
    | Building_decryption _ -> Encryption.block_size t.encryption
    | Waiting_for_mac _ -> Mac.signature_length t.mac )
;;

let rec process_block t ~f =
  match t.state with
  | Building_decryption { size = None } ->
      let size = Read_buffer.uint32 t.read_buffer in
      t.state <- Building_decryption { size = Some size };
      process_block t ~f
  | Building_decryption { size = Some size } ->
      if available_in_work_buffer t >= size (* should be just size *) then (
        let padding_length = Read_buffer.uint8 t.read_buffer in
        let padding_length_length = 1 in
        let compressed_message =
          Read_buffer.bytes t.read_buffer
            ~len:(size - padding_length - padding_length_length)
        in
        reset_work_buffer t;
        t.state <- Waiting_for_mac { compressed_message } )
  | Waiting_for_mac { compressed_message } ->
      let signature = Write_buffer.consume_to_string t.work_buffer in
      let message =
        generate_mac_string_to_verify ~sequence_number:t.sequence_number
          ~clean_work_buffer:t.work_buffer compressed_message
      in
      let sequence_number = Uint32.to_int t.sequence_number in
      t.sequence_number <- Uint32.succ t.sequence_number;
      t.state <- Building_decryption { size = None };
      if Mac.verify t.mac ~message ~signature then (
        let message = Compression.decompress t.compression compressed_message in
        Write_buffer.bytes t.work_buffer message;
        f ~sequence_number
          (`Message
            (Read_buffer.narrow t.read_buffer (available_in_work_buffer t)));
        reset_work_buffer t )
      else f ~sequence_number `Failed_to_verify
;;

let write_block t block =
  Write_buffer.bytes t.work_buffer
    ( match t.state with
    | Building_decryption _ -> Encryption.decrypt t.encryption block
    | Waiting_for_mac _ -> block )
;;

let process_string t string ~f =
  Growable_buffer.store t.input_buffer string;
  let rec loop () =
    match request_next_block t with
    | None -> ()
    | Some block ->
        write_block t block;
        process_block t ~f;
        loop ()
  in
  loop ()
;;
