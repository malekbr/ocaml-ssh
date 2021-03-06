open! Core

module Kex_result = struct
  module Z = struct
    type t = Z.t

    let sexp_of_t t =
      sexp_of_string
        (Mirage_crypto_pk.Z_extra.to_cstruct_be t |> Cstruct.to_string)
    ;;
  end

  type t = {
      public_host_key : String.Hexdump.t
    ; shared_key : Z.t
    ; shared_hash : String.Hexdump.t
    ; hash_signature : String.Hexdump.t
  }
  [@@deriving sexp_of]
end

module Negotation = struct
  type compute_kex_result =
       scratch_pad:Write_buffer.t
    -> client_identification_string:string
    -> server_identification_string:string
    -> client_kex_payload:string
    -> server_kex_payload:string
    -> Kex_result.t Or_error.t

  type t =
    | Negotiating of (Write_buffer.t -> [ `Write_complete of unit ])
    | Negotiated of compute_kex_result
    | Nothing_to_send
end

module type S = sig
  type t

  val name : string

  val create : unit -> t

  val receive : t -> Message_id.t -> Read_buffer.t -> unit

  val negotiate : t -> Negotation.t
  (** Will get called repeatedly until nothing to send, or negotiated *)

  val hash : string -> string
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

  module Fixed_group_sha (M : sig
    val group_number : int

    val group : Mirage_crypto_pk.Dh.group

    val sha_type : int

    val hash_digest : Cstruct.t -> Cstruct.t
  end) =
  struct
    type t = {
        secret : Mirage_crypto_pk.Dh.secret
      ; e : Z.t
      ; mutable sent : bool
      ; result : Negotation.compute_kex_result Set_once.t
    }

    let name = sprintf "diffie-hellman-group%d-sha%d" M.group_number M.sha_type

    let group = M.group

    let create () =
      let secret, e = Mirage_crypto_pk.Dh.gen_key group in
      let e = Mirage_crypto_pk.Z_extra.of_cstruct_be e in
      { secret; e; sent = false; result = Set_once.create () }
    ;;

    let hash string =
      M.hash_digest (Cstruct.of_string string) |> Cstruct.to_string
    ;;

    let compute_shared_hash ~e ~f ~shared_key ~hash_signature ~public_host_key
        ~scratch_pad ~client_identification_string ~server_identification_string
        ~client_kex_payload ~server_kex_payload =
      Write_buffer.string scratch_pad client_identification_string;
      Write_buffer.string scratch_pad server_identification_string;
      Write_buffer.string scratch_pad client_kex_payload;
      Write_buffer.string scratch_pad server_kex_payload;
      Write_buffer.string scratch_pad public_host_key;
      Write_buffer.mpint scratch_pad e;
      Write_buffer.mpint scratch_pad f;
      Write_buffer.mpint scratch_pad shared_key;
      let shared_hash = Write_buffer.consume_to_string scratch_pad in
      let shared_hash = hash shared_hash in
      Ok Kex_result.{ public_host_key; shared_key; shared_hash; hash_signature }
    ;;

    let receive t code read_buffer =
      assert (Message_id.equal code DH_generic.Code.kex_reply);
      let public_host_key = Read_buffer.string read_buffer in
      let f = Read_buffer.mpint read_buffer in
      (* TODO malekbr: verify *)
      let hash_signature = Read_buffer.string read_buffer in
      let shared_key =
        Mirage_crypto_pk.Dh.shared t.secret
          (Mirage_crypto_pk.Z_extra.to_cstruct_be f)
        |> Option.value_exn |> Mirage_crypto_pk.Z_extra.of_cstruct_be
      in
      Set_once.set_exn t.result [%here]
        (compute_shared_hash ~hash_signature ~e:t.e ~f ~shared_key
           ~public_host_key)
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
                Write_buffer.mpint write_buffer t.e;
                `Write_complete ()) )
    ;;
  end

  module DH_G14_SHA1 = Fixed_group_sha (struct
    let group_number = 14

    let sha_type = 1

    let group = Mirage_crypto_pk.Dh.Group.oakley_14

    let hash_digest = Mirage_crypto.Hash.SHA1.digest
  end)

  module DH_G14_SHA256 = Fixed_group_sha (struct
    let group_number = 14

    let sha_type = 256

    let group = Mirage_crypto_pk.Dh.Group.oakley_14

    let hash_digest = Mirage_crypto.Hash.SHA256.digest
  end)

  module DH_G16_SHA512 = Fixed_group_sha (struct
    let group_number = 16

    let sha_type = 512

    let group = Mirage_crypto_pk.Dh.Group.oakley_16

    let hash_digest = Mirage_crypto.Hash.SHA512.digest
  end)

  module DH_G18_SHA512 = Fixed_group_sha (struct
    let group_number = 18

    let sha_type = 512

    let group = Mirage_crypto_pk.Dh.Group.oakley_18

    let hash_digest = Mirage_crypto.Hash.SHA512.digest
  end)

  let diffie_hellman_group14_sha1 : t = T (module DH_G14_SHA1)

  let diffie_hellman_group14_sha256 : t = T (module DH_G14_SHA256)

  let diffie_hellman_group16_sha512 : t = T (module DH_G16_SHA512)

  let diffie_hellman_group18_sha512 : t = T (module DH_G18_SHA512)

  let name (T (module M)) = M.name

  let create (T ((module M) as t)) = State (M.create (), t)

  let all =
    [
      diffie_hellman_group18_sha512
    ; diffie_hellman_group16_sha512
    ; diffie_hellman_group14_sha256
    ; diffie_hellman_group14_sha1
    ]
  ;;
end

let negotiate (State (t, (module M))) = M.negotiate t

let receive (State (t, (module M))) = M.receive t

let hash (State (_, (module M))) = M.hash
