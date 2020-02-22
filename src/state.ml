open! Core
open! Async

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
      ; verification : [ `Success | `Failure ] Deferred.t
    }
  | Refused_host
  | No_key_exchange

module Channel_distributor = struct
  module State = struct
    type t =
      | Creating of
          ( Channel_request.Confirmation.t
          * Channel_request.Server_message.t Pipe.Reader.t )
          Ivar.t
      | Created of Channel_request.Server_message.t Pipe.Writer.t

    let write_pipe = function Created writer -> writer | _ -> assert false
  end

  type t = State.t Int.Table.t

  let create () = Int.Table.create ()

  let create_channel (t : t) id =
    let result = Ivar.create () in
    Hashtbl.add_exn t ~key:id ~data:(Creating result);
    Ivar.read result
  ;;

  let confirm_created (t : t) read_buffer =
    let id, confirmation = Channel_request.handle_confirmation read_buffer in
    Hashtbl.update t id ~f:(function
      | None -> raise_s [%message "Unknown channel id" (id : int)]
      | Some (Created _) -> raise_s [%message "Channel confirmed twice"]
      | Some (Creating ivar) ->
          let reader, writer = Pipe.create () in
          Ivar.fill ivar (confirmation, reader);
          Created writer)
  ;;

  let close t id =
    Hashtbl.change t id
      ~f:
        (Option.bind ~f:(function
          | State.Created writer ->
              Pipe.close writer;
              None
          | Creating _ -> assert false))
  ;;

  let handle_window_adjust (t : t) read_buffer =
    let id, message = Channel_request.handle_window_adjust read_buffer in
    let write_pipe = Hashtbl.find_exn t id |> State.write_pipe in
    Pipe.write_without_pushback write_pipe message
  ;;

  let handle_success (t : t) read_buffer =
    let id, message = Channel_request.handle_success read_buffer in
    let write_pipe = Hashtbl.find_exn t id |> State.write_pipe in
    Pipe.write_without_pushback write_pipe message
  ;;

  let handle_failure (t : t) read_buffer =
    let id, message = Channel_request.handle_success read_buffer in
    let write_pipe = Hashtbl.find_exn t id |> State.write_pipe in
    Pipe.write_without_pushback write_pipe message
  ;;

  let handle_close (t : t) read_buffer =
    let id, message = Channel_request.handle_close read_buffer in
    let write_pipe = Hashtbl.find_exn t id |> State.write_pipe in
    Pipe.write_without_pushback write_pipe message
  ;;

  let handle_eof (t : t) read_buffer =
    let id, message = Channel_request.handle_eof read_buffer in
    let write_pipe = Hashtbl.find_exn t id |> State.write_pipe in
    Pipe.write_without_pushback write_pipe message
  ;;

  let handle_extended_data (t : t) read_buffer =
    let id, message = Channel_request.handle_extended_data read_buffer in
    let write_pipe = Hashtbl.find_exn t id |> State.write_pipe in
    Pipe.write_without_pushback write_pipe message
  ;;

  let handle_data (t : t) read_buffer =
    let id, message = Channel_request.handle_data read_buffer in
    let write_pipe = Hashtbl.find_exn t id |> State.write_pipe in
    Pipe.write_without_pushback write_pipe message
  ;;

  let handle_request (t : t) read_buffer =
    let id, message = Channel_request.handle_request read_buffer in
    let write_pipe = Hashtbl.find_exn t id |> State.write_pipe in
    Pipe.write_without_pushback write_pipe message
  ;;
end

type t = {
    mutable state : state
  ; send_message : (Write_buffer.t -> unit) -> unit
  ; scratch_pad : Write_buffer.t
  ; update_packet_writer : (Packet_writer.t -> unit) -> unit
  ; update_packet_reader : (Packet_reader.t -> unit) -> unit
  ; session_id : string Set_once.t
  ; server_identification : string
  ; on_connection_established : unit Or_error.t -> unit
  ; transport_config : Transport_config.t
  ; service_response_queue : Service_request.Response_queue.t
  ; mutable user_auth : User_auth.t option
  ; channel_id_generator : Channel_request.Id_generator.t
  ; channel_distributor : Channel_distributor.t
}

(* Send cannot be async. If you need to do work that is async, call send
 * after *)
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
  ; user_auth = None
  ; channel_id_generator = Channel_request.Id_generator.create ()
  ; channel_distributor = Channel_distributor.create ()
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
      let public_key =
        Public_key_algorithm.Method.create algorithms.public_key
          ~certificate:negotiated.public_host_key ~scratch_pad:t.scratch_pad
      in
      let ready = Ivar.create () in
      t.state <-
        Exchanged_keys
          { negotiated; algorithms; verification = Ivar.read ready };
      don't_wait_for
        (let%map verified =
           Public_key_algorithm.verify public_key ~scratch_pad:t.scratch_pad
             negotiated
         in
         if verified then Ivar.fill ready `Success else Ivar.fill ready `Failure)
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
      print_s [%message (server_config : Transport_config.Received.t)];
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
  | Exchanged_keys { algorithms; negotiated; verification } ->
      don't_wait_for
        ( match%map verification with
        | `Success ->
            Set_once.set_if_none t.session_id [%here] negotiated.shared_hash;
            t.send_message (fun writer ->
                Write_buffer.message_id writer New_keys);
            print_s [%message (negotiated : Kex.Kex_result.t)];
            set_algorithm_s_to_c t negotiated algorithms;
            set_algorithm_c_to_s t negotiated algorithms;
            t.on_connection_established (Ok ());
            t.state <- No_key_exchange
        | `Failure ->
            t.state <- Refused_host;
            t.on_connection_established
              (error_s [%message "Refused host connection"]) )
  | _ -> assert false
;;

let handle_global_request t read_buffer =
  let request_name = Read_buffer.string read_buffer in
  let want_reply = Read_buffer.bool read_buffer in
  print_s
    [%message
      "Received global request" (request_name : string) (want_reply : bool)];
  if want_reply then
    send t (fun write_buffer ->
        Write_buffer.message_id write_buffer Request_failure)
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
  | Userauth_failure ->
      User_auth.register_denied (Option.value_exn t.user_auth) read_buffer
  | Userauth_success ->
      User_auth.register_accepted (Option.value_exn t.user_auth)
  | Userauth_algorithm_specific message_id ->
      User_auth.register_negotiation
        (Option.value_exn t.user_auth)
        ~message_id read_buffer
  | Global_request -> handle_global_request t read_buffer
  | Channel_open_confirmation ->
      Channel_distributor.confirm_created t.channel_distributor read_buffer
  | Channel_window_adjust ->
      Channel_distributor.handle_window_adjust t.channel_distributor read_buffer
  | Channel_success ->
      Channel_distributor.handle_success t.channel_distributor read_buffer
  | Channel_failure ->
      Channel_distributor.handle_failure t.channel_distributor read_buffer
  | Channel_data ->
      Channel_distributor.handle_data t.channel_distributor read_buffer
  | Channel_extended_data ->
      Channel_distributor.handle_extended_data t.channel_distributor read_buffer
  | Channel_request ->
      Channel_distributor.handle_request t.channel_distributor read_buffer
  | Channel_eof ->
      Channel_distributor.handle_eof t.channel_distributor read_buffer
  | Channel_close ->
      Channel_distributor.handle_close t.channel_distributor read_buffer
  | _ ->
      raise_s
        [%message "Unimplemented message type" (message_type : Message_id.t)]
;;

let request_service t ~service_name =
  send t (Service_request.request t.service_response_queue ~service_name)
;;

let request_auth t ~username mode =
  let user_auth = User_auth.create mode in
  t.user_auth <- Some user_auth;
  let%bind result = send t (User_auth.request user_auth ~username) in
  Deferred.repeat_until_finished result (function
    | User_auth.Auth_response.Accepted ->
        `Finished User_auth.Auth_result.Accepted |> return
    | Refused { alternatives } ->
        `Finished (User_auth.Auth_result.Refused { alternatives }) |> return
    | Authenticating authenticating ->
        let%bind write_response =
          User_auth.respond_negotation user_auth authenticating
        in
        let%map response = send t write_response in
        `Repeat response)
;;

let request_session_channel t =
  let id =
    send t
      (Channel_request.request_channel_session_create t.channel_id_generator)
  in
  let%map reader, confirmation =
    Channel_distributor.create_channel t.channel_distributor id
  in
  (id, reader, confirmation)
;;

let request_shell t (_id : int) ~server_id =
  send t (Channel_request.request_shell ~server_id ~want_reply:true)
;;

let request_pty t (_id : int) ~server_id ~term ~width ~height =
  send t
    (Channel_request.request_pty ~server_id ~term ~width ~height
       ~want_reply:true)
;;

let request_exec t (_id : int) ~server_id ~command =
  send t (Channel_request.request_exec ~server_id ~command ~want_reply:true)
;;

let send_data t (_id : int) ~server_id ~data =
  send t (Channel_request.send_data ~server_id ~data)
;;

let close_channel t id = Channel_distributor.close t.channel_distributor id
