open! Core
open! Async

let service_name = "ssh-userauth"

let request_service_name = "ssh-connection"

module Keyboard_authentication = struct
  module Prompt = struct
    type t = { prompt : string; echo : bool } [@@deriving sexp_of]
  end

  let request_code = 60

  let response_code = 61

  module Request = struct
    type t = { name : string; instruction : string; prompts : Prompt.t list }
    [@@deriving fields, sexp_of]

    let parse read_buffer =
      let name = Read_buffer.string read_buffer in
      let instruction = Read_buffer.string read_buffer in
      let (_ : string) = Read_buffer.string read_buffer in
      let nprompts = Read_buffer.uint32 read_buffer in
      let prompts =
        Array.init nprompts ~f:(fun (_ : int) ->
            let prompt = Read_buffer.string read_buffer in
            let echo = Read_buffer.bool read_buffer in
            Prompt.{ prompt; echo })
        |> Array.to_list
      in
      { name; instruction; prompts }
    ;;

    let respond t f : string list Deferred.t =
      Deferred.List.map t.prompts ~f:(fun { prompt; echo } -> f ~prompt ~echo)
    ;;
  end

  module Response = struct
    type t = string list

    let write t write_buffer =
      Write_buffer.message_id write_buffer
        (Userauth_algorithm_specific response_code);
      Write_buffer.uint32 write_buffer (List.length t);
      List.iter t ~f:(Write_buffer.string write_buffer)
    ;;
  end

  type t = Request.t [@@deriving sexp_of]
end

module Auth_method_response = struct
  type t = Keyboard_interactive of Keyboard_authentication.t
  [@@deriving sexp_of]
end

module Mode = struct
  type t =
    | Keyboard_interactive of {
          submethods : string list
        ; respond : Keyboard_authentication.Request.t -> string list Deferred.t
      }
    | None
    | Password of string
  [@@deriving variants]

  let method_ = function
    | Keyboard_interactive _ -> "keyboard-interactive"
    | Password _ -> "password"
    | None -> "none"
  ;;

  let handle_negotiation t ~message_id read_buffer =
    match t with
    | None | Password _ ->
        raise_s [%message "Unexpected negotation message" (message_id : int)]
    | Keyboard_interactive { submethods = _; respond = _ } ->
        assert (message_id = Keyboard_authentication.request_code);
        Keyboard_authentication.Request.parse read_buffer
        |> Auth_method_response.Keyboard_interactive
  ;;

  let handle_response t (response : Auth_method_response.t) =
    match (t, response) with
    | (Password _ | None), _ ->
        raise_s [%message "Unexpected response auth for auth moed"]
    | ( Keyboard_interactive { submethods = _; respond }
      , Keyboard_interactive authentication ) ->
        let%map response = respond authentication in
        Keyboard_authentication.Response.write response
  ;;

  let write_request t write_buffer =
    match t with
    | None -> ()
    | Password password ->
        Write_buffer.bool write_buffer false;
        Write_buffer.string write_buffer password
    | Keyboard_interactive { submethods; respond = _ } ->
        Write_buffer.string write_buffer "";
        Write_buffer.name_list write_buffer submethods
  ;;
end

module Auth_response = struct
  (* TODO implement failure *)
  type t =
    | Accepted
    | Authenticating of Auth_method_response.t
    | Refused of { alternatives : string list }
  [@@deriving sexp_of]
end

type t = { mode : Mode.t; response : Auth_response.t Mvar.Read_write.t }
(** Use one per connection until success *)

let create mode = { mode; response = Mvar.create () }

let with_mvar t ~f =
  if not (Mvar.is_empty t.response) then
    raise_s [%message "Did not receive response for last auth request"];
  f ();
  Mvar.take t.response
;;

let request_generic ~username ~method_ write_buffer =
  Write_buffer.message_id write_buffer Userauth_request;
  Write_buffer.string write_buffer username;
  Write_buffer.string write_buffer request_service_name;
  Write_buffer.string write_buffer method_
;;

let request t ~username write_buffer =
  with_mvar t ~f:(fun () ->
      let method_ = Mode.method_ t.mode in
      request_generic ~username ~method_ write_buffer;
      Mode.write_request t.mode write_buffer)
;;

let respond_negotation t response =
  let%map write = Mode.handle_response t.mode response in
  fun write_buffer -> with_mvar t ~f:(fun () -> write write_buffer)
;;

let register_negotiation t ~message_id read_buffer =
  Mode.handle_negotiation t.mode ~message_id read_buffer
  |> Auth_response.Authenticating |> Mvar.set t.response
;;

let register_denied t read_buffer =
  let alternatives = Read_buffer.name_list read_buffer in
  Mvar.set t.response (Auth_response.Refused { alternatives })
;;

let register_accepted t = Mvar.set t.response Auth_response.Accepted

module Auth_result = struct
  type t = Accepted | Refused of { alternatives : string list }
  [@@deriving sexp]
end
