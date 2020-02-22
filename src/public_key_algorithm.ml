open! Core
open! Async

module type S = sig
  type t

  type host_validator

  val name : string

  val create : certificate:string -> scratch_pad:Write_buffer.t -> t

  (* TODO Or_error *)
  val verify :
       t
    -> host_validator
    -> scratch_pad:Write_buffer.t
    -> Kex.Kex_result.t
    -> bool Deferred.t
end

type t =
  | State :
      'a
      * 'host_validator
      * (module S with type t = 'a and type host_validator = 'host_validator)
      -> t

module Method = struct
  type t =
    | T :
        'host_validator
        * (module S with type t = 'a and type host_validator = 'host_validator)
        -> t

  let name (T (_, (module M))) = M.name

  let create (T (host_validator, ((module M) as t))) ~certificate ~scratch_pad =
    State (M.create ~certificate ~scratch_pad, host_validator, t)
  ;;

  module Make_ssh_rsa (Hash : sig
    val oid : string

    val name : string

    val digest : Cstruct.t -> Cstruct.t
  end) =
  struct
    type t = Nocrypto.Rsa.pub

    type host_validator = Nocrypto.Rsa.pub -> bool Deferred.t

    let name = Hash.name

    let create ~certificate ~scratch_pad : t =
      print_s [%message (certificate : String.Hexdump.t)];
      let read_buffer = Write_buffer.read_buffer scratch_pad in
      Write_buffer.bytes scratch_pad certificate;
      let algorithm = Read_buffer.string read_buffer in
      assert (String.equal algorithm "ssh-rsa");
      let e = Read_buffer.mpint read_buffer in
      let n = Read_buffer.mpint read_buffer in
      Write_buffer.reset scratch_pad;
      { e; n }
    ;;

    (* https://tools.ietf.org/html/rfc8017 *)
    let oid = Cstruct.of_string Hash.oid

    let verify t host_validator ~scratch_pad (kex_result : Kex.Kex_result.t) =
      let read_buffer = Write_buffer.read_buffer scratch_pad in
      Write_buffer.bytes scratch_pad kex_result.hash_signature;
      let algorithm = Read_buffer.string read_buffer in
      assert (String.equal algorithm name);
      let signed = Read_buffer.string read_buffer in
      Write_buffer.reset scratch_pad;
      let key_valid =
        Nocrypto.Rsa.PKCS1.sig_decode ~key:t (Cstruct.of_string signed)
        |> Option.value_map ~default:false ~f:(fun decoded ->
               let personal_signed =
                 Cstruct.of_string kex_result.shared_hash
                 |> Hash.digest |> Cstruct.append oid
               in
               print_s
                 [%message
                   (Cstruct.to_string personal_signed : String.Hexdump.t)
                     (Cstruct.to_string decoded : String.Hexdump.t)];
               Cstruct.equal decoded personal_signed)
      in
      Deferred.map (host_validator t) ~f:(( && ) key_valid)
    ;;
  end

  module Ssh_rsa = Make_ssh_rsa (struct
    let oid = "\x30\x21\x30\x09\x06\x05\x2b\x0e\x03\x02\x1a\x05\x00\x04\x14"

    let digest = Nocrypto.Hash.SHA1.digest

    let name = "ssh-rsa"
  end)

  module Ssh_rsa_sha256 = Make_ssh_rsa (struct
    let oid =
      "\x30\x31\x30\x0d\x06\x09\x60\x86\x48\x01\x65\x03\x04\x02\x01\x05\x00\x04\x20"
    ;;

    let digest = Nocrypto.Hash.SHA256.digest

    let name = "rsa-sha2-256"
  end)

  module Ssh_rsa_sha512 = Make_ssh_rsa (struct
    let oid =
      "\x30\x51\x30\x0d\x06\x09\x60\x86\x48\x01\x65\x03\x04\x02\x03\x05\x00\x04\x40"
    ;;

    let digest = Nocrypto.Hash.SHA512.digest

    let name = "rsa-sha2-512"
  end)

  let ssh_rsa ~host_validator : t = T (host_validator, (module Ssh_rsa))

  let ssh_rsa_sha256 ~host_validator : t =
    T (host_validator, (module Ssh_rsa_sha256))
  ;;

  let ssh_rsa_sha512 ~host_validator : t =
    T (host_validator, (module Ssh_rsa_sha512))
  ;;

  let fingerprint (pub : Nocrypto.Rsa.pub) ~validate =
    let output = Buffer.create 0 in
    Cstruct.concat
      [
        Nocrypto.Numeric.Z.to_cstruct_be pub.e
      ; Nocrypto.Numeric.Z.to_cstruct_be pub.n
      ]
    |> Nocrypto.Hash.SHA256.digest
    |> Cstruct.hexdump_to_buffer output;
    validate ~fingerprint:(Buffer.To_string.subo output)
  ;;

  let all ~validate =
    [
      ssh_rsa_sha512 ~host_validator:(fingerprint ~validate)
    ; ssh_rsa_sha256 ~host_validator:(fingerprint ~validate)
    ; ssh_rsa ~host_validator:(fingerprint ~validate)
    ]
  ;;
end

let verify (State (t, host_validator, (module M))) = M.verify t host_validator
