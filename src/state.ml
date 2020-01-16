open! Core

type state =
  | Sending_transport_config of { cookie : string }
  | Key_exchange of {
        algorithms : Transport_config.Negotiated_algorithms.t
      ; server_kex_payload : string
      ; client_kex_payload : string
    }
  | Exchanged_keys of {
        negotiated : Kex.Kex_result.t
      ; algorithms : Transport_config.Negotiated_algorithms.t
    }
  | No_key_exchange

type t = {
    mutable state : state
  ; send_message : (Write_buffer.t -> unit) -> unit
  ; scratch_pad : Write_buffer.t
  ; update_packet_writer : (Packet_writer.t -> unit) -> unit
  ; update_packet_reader : (Packet_reader.t -> unit) -> unit
  ; session_id : string Set_once.t
  ; server_identification : string
  ; on_connection_established : unit -> unit
  ; transport_config : Transport_config.t
  ; service_response_queue : Service_request.Response_queue.t
}

let send t f =
  let result = Set_once.create () in
  t.send_message (fun write_buffer ->
      f write_buffer |> Set_once.set_exn result [%here]);
  Set_once.get_exn result [%here]
;;

let create transport_config send_message update_packet_writer
    update_packet_reader ~on_connection_established ~server_identification =
  let cookie = Transport_config.generate_cookie () in
  send_message (Transport_config.write ~cookie transport_config);
  {
    state = Sending_transport_config { cookie }
  ; send_message
  ; scratch_pad = Write_buffer.create ()
  ; session_id = Set_once.create ()
  ; update_packet_reader
  ; update_packet_writer
  ; on_connection_established
  ; server_identification
  ; transport_config
  ; service_response_queue = Service_request.Response_queue.create ()
  }
;;

let set_algorithm_s_to_c t negotiated
    (algorithms : Transport_config.Negotiated_algorithms.t) =
  let session_id = Set_once.get_exn t.session_id [%here] in
  t.update_packet_reader (fun packet_reader ->
      Encryption.Method.create_decrypt algorithms.encryption_s_to_c
        ~initialization_vector:
          (Shared_key.compute_iv_s_to_c algorithms.kex negotiated
             ~scratch_pad:t.scratch_pad
             ~size:(Encryption.Method.key_size algorithms.encryption_s_to_c)
             ~session_id)
        ~key:
          (Shared_key.compute_enc_key_s_to_c algorithms.kex negotiated
             ~scratch_pad:t.scratch_pad
             ~size:(Encryption.Method.key_size algorithms.encryption_s_to_c)
             ~session_id)
      |> Packet_reader.set_encryption packet_reader;
      Mac.Method.create algorithms.mac_s_to_c
        ~key:
          (Shared_key.compute_mac_key_s_to_c algorithms.kex negotiated
             ~scratch_pad:t.scratch_pad
             ~size:(Mac.Method.key_length algorithms.mac_s_to_c)
             ~session_id)
      |> Packet_reader.set_mac packet_reader;
      Compression.Method.create algorithms.compression_s_to_c
      |> Packet_reader.set_compression packet_reader)
;;

let set_algorithm_c_to_s t negotiated
    (algorithms : Transport_config.Negotiated_algorithms.t) =
  let session_id = Set_once.get_exn t.session_id [%here] in
  t.update_packet_writer (fun packet_writer ->
      Encryption.Method.create_encrypt algorithms.encryption_c_to_s
        ~initialization_vector:
          (Shared_key.compute_iv_c_to_s algorithms.kex negotiated
             ~scratch_pad:t.scratch_pad
             ~size:(Encryption.Method.key_size algorithms.encryption_c_to_s)
             ~session_id)
        ~key:
          (Shared_key.compute_enc_key_c_to_s algorithms.kex negotiated
             ~scratch_pad:t.scratch_pad
             ~size:(Encryption.Method.key_size algorithms.encryption_c_to_s)
             ~session_id)
      |> Packet_writer.set_encryption packet_writer;
      Mac.Method.create algorithms.mac_c_to_s
        ~key:
          (Shared_key.compute_mac_key_c_to_s algorithms.kex negotiated
             ~scratch_pad:t.scratch_pad
             ~size:(Mac.Method.key_length algorithms.mac_c_to_s)
             ~session_id)
      |> Packet_writer.set_mac packet_writer;
      Compression.Method.create algorithms.compression_c_to_s
      |> Packet_writer.set_compression packet_writer)
;;

let rec respond_kex t (algorithms : Transport_config.Negotiated_algorithms.t)
    ~server_kex_payload ~client_kex_payload =
  match Kex.negotiate algorithms.kex with
  | Nothing_to_send -> ()
  | Negotiating f ->
      t.send_message f;
      respond_kex t algorithms ~server_kex_payload ~client_kex_payload
  | Negotiated compute_result ->
      let negotiated =
        compute_result ~scratch_pad:t.scratch_pad
          ~client_identification_string:Constants.identification_string
          ~server_identification_string:t.server_identification
          ~client_kex_payload ~server_kex_payload
        |> Or_error.ok_exn
      in
      Set_once.set_if_none t.session_id [%here] negotiated.shared_hash;
      t.send_message (fun writer -> Write_buffer.message_id writer New_keys);
      t.state <- Exchanged_keys { negotiated; algorithms }
;;

let handle_key_exchange_init t ~server_kex_payload read_buffer =
  match t.state with
  | Sending_transport_config { cookie } ->
      (* We don't get the message id in the payload *)
      let client_kex_payload =
        Transport_config.write ~cookie t.transport_config t.scratch_pad;
        Write_buffer.consume_to_string t.scratch_pad
      in
      let server_config = Transport_config.Received.parse read_buffer in
      let algorithms =
        Transport_config.negotiate t.transport_config server_config
        |> Option.value_exn
      in
      respond_kex t algorithms ~server_kex_payload ~client_kex_payload;
      t.state <-
        Key_exchange { algorithms; server_kex_payload; client_kex_payload }
  | _ -> assert false
;;

let handle_key_exchange t read_buffer message_type =
  match t.state with
  | Key_exchange { algorithms; server_kex_payload; client_kex_payload } ->
      Kex.receive algorithms.kex message_type read_buffer;
      respond_kex t algorithms ~server_kex_payload ~client_kex_payload
  | _ -> assert false
;;

let handle_new_keys t =
  match t.state with
  | Exchanged_keys { algorithms; negotiated } ->
      set_algorithm_s_to_c t negotiated algorithms;
      set_algorithm_c_to_s t negotiated algorithms;
      t.on_connection_established ();
      t.state <- No_key_exchange
  | _ -> assert false
;;

let handle_message ~payload t read_buffer =
  let message_type = Read_buffer.message_id read_buffer in
  match message_type with
  | Key_exchange_init ->
      handle_key_exchange_init ~server_kex_payload:payload t read_buffer
  | New_keys -> handle_new_keys t
  | Key_exchange_algorithm_specific _ ->
      handle_key_exchange t read_buffer message_type
  | Service_accept ->
      Service_request.respond t.service_response_queue read_buffer
  | _ ->
      raise_s
        [%message "Unimplemented message type" (message_type : Message_id.t)]
;;

let request_service t ~service_name =
  send t (Service_request.request t.service_response_queue ~service_name)
;;
