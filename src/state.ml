open! Core

type state =
  | Sending_transport_config of Transport_config.t
  | Key_exchange of Kex.t
  | Exchanged_keys

type t = {
    mutable state : state
  ; mutable result : Kex.Kex_result.t option
  ; send_message : (Write_buffer.t -> unit) -> unit
}

let create config send_message =
  send_message (Transport_config.write config);
  { state = Sending_transport_config config; result = None; send_message }
;;

let rec respond_kex t kex =
  match Kex.negotiate kex with
  | Nothing_to_send -> ()
  | Negotiating f ->
      t.send_message f;
      respond_kex t kex
  | Negotiated negotiated ->
      print_s [%message (negotiated : Kex.Kex_result.t)];
      t.result <- Some negotiated;
      t.state <- Exchanged_keys;
      ()
;;

let handle_key_exchange_init t read_buffer =
  match t.state with
  | Sending_transport_config transport_config ->
      let server_config = Transport_config.Received.parse read_buffer in
      let kex =
        Transport_config.chosen_kex transport_config server_config
        |> Option.value_exn
      in
      respond_kex t kex;
      t.state <- Key_exchange kex
  | _ -> assert false
;;

let handle_key_exchange t read_buffer message_type =
  match t.state with
  | Key_exchange kex ->
      Kex.receive kex message_type read_buffer;
      respond_kex t kex
  | _ -> assert false
;;

let handle_message t read_buffer =
  let message_type = Read_buffer.message_id read_buffer in
  match message_type with
  | Key_exchange_init -> handle_key_exchange_init t read_buffer
  | Key_exchange_algorithm_specific _ ->
      handle_key_exchange t read_buffer message_type
  | _ ->
      raise_s
        [%message "Unimplemented message type" (message_type : Message_id.t)]
;;
