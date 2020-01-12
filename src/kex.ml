open! Core

module Kex_result = struct
  type t = {
      public_host_data : String.Hexdump.t
    ; shared_key : String.Hexdump.t
    ; hash : String.Hexdump.t
  }
  [@@deriving sexp_of]
end

module Negotation = struct
  type t =
    | Negotiating of (Write_buffer.t -> unit)
    | Negotiated of Kex_result.t
    | Nothing_to_send
end

module type S = sig
  type t

  val name : string

  val create : unit -> t

  val receive : t -> Message_id.t -> Read_buffer.t -> unit

  val negotiate : t -> Negotation.t
  (** Will get called repeatedly until nothing to send, or negotiated *)
end

type t = State : 'a * (module S with type t = 'a) -> t

module Method = struct
  type t = T : (module S with type t = 'a) -> t

  module DH_generic = struct
    module Code = struct
      let kex_init = Message_id.of_code 30

      let kex_reply = Message_id.of_code 31
    end
  end

  module DH_G1_SHA1 = struct
    type t

    let name = "diffie-hellman-group1-sha1"

    let create () = assert false

    let receive _ _ = assert false

    let negotiate _ = assert false
  end

  let diffie_hellman_group1_sha1 : t = T (module DH_G1_SHA1)

  module DH_G14_SHA1 = struct
    type t = {
        secret : Nocrypto.Dh.secret
      ; e : Z.t
      ; mutable sent : bool
      ; result : Kex_result.t Set_once.t
    }

    let name = "diffie-hellman-group14-sha1"

    let group = Nocrypto.Dh.Group.oakley_14

    let create () =
      let secret, e = Nocrypto.Dh.gen_key group in
      let e = Nocrypto.Numeric.Z.of_cstruct_be e in
      { secret; e; sent = false; result = Set_once.create () }
    ;;

    let receive t code read_buffer =
      assert (Message_id.equal code DH_generic.Code.kex_reply);
      let public_host_data = Read_buffer.string read_buffer in
      let f =
        Read_buffer.mpint read_buffer |> Nocrypto.Numeric.Z.to_cstruct_be
      in
      let hash = Read_buffer.string read_buffer in
      let shared_key =
        Nocrypto.Dh.shared group t.secret f
        |> Option.value_exn |> Cstruct.to_string
      in
      Set_once.set_exn t.result [%here] { public_host_data; hash; shared_key }
    ;;

    let negotiate t =
      match Set_once.get t.result with
      | Some result -> Negotation.Negotiated result
      | None ->
          if t.sent then Nothing_to_send
          else (
            t.sent <- true;
            Negotation.Negotiating
              (fun write_buffer ->
                Write_buffer.message_id write_buffer DH_generic.Code.kex_init;
                Write_buffer.mpint write_buffer t.e) )
    ;;
  end

  let diffie_hellman_group14_sha1 : t = T (module DH_G14_SHA1)

  let name (T (module M)) = M.name

  let create (T ((module M) as t)) = State (M.create (), t)

  let all = [ diffie_hellman_group1_sha1; diffie_hellman_group14_sha1 ]
end

let negotiate (State (t, (module M))) = M.negotiate t

let receive (State (t, (module M))) = M.receive t
