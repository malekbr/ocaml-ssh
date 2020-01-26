open! Core

module type S = sig
  type t

  type host_validator

  val name : string

  val create : certificate:string -> scratch_pad:Write_buffer.t -> t

  (* TODO Or_error *)
  val verify : t -> scratch_pad:Write_buffer.t -> Kex.Kex_result.t -> bool
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

  module Ssh_rsa = struct
    type t = Nocrypto.Rsa.pub

    type host_validator = unit

    let name = "ssh-rsa"

    let create ~certificate ~scratch_pad : t =
      print_s [%message (certificate : String.Hexdump.t)];
      let read_buffer = Write_buffer.read_buffer scratch_pad in
      Write_buffer.bytes scratch_pad certificate;
      let algorithm = Read_buffer.string read_buffer in
      assert (String.equal algorithm name);
      let e = Read_buffer.mpint read_buffer in
      let n = Read_buffer.mpint read_buffer in
      Write_buffer.reset scratch_pad;
      { e; n }
    ;;

    (* https://tools.ietf.org/html/rfc8017 *)
    let oid =
      "\x30\x21\x30\x09\x06\x05\x2b\x0e\x03\x02\x1a\x05\x00\x04\x14"
      |> Cstruct.of_string
    ;;

    let verify t ~scratch_pad (kex_result : Kex.Kex_result.t) =
      let read_buffer = Write_buffer.read_buffer scratch_pad in
      Write_buffer.bytes scratch_pad kex_result.hash_signature;
      let algorithm = Read_buffer.string read_buffer in
      assert (String.equal algorithm name);
      let signed = Read_buffer.string read_buffer in
      Write_buffer.reset scratch_pad;
      Nocrypto.Rsa.PKCS1.sig_decode ~key:t (Cstruct.of_string signed)
      |> Option.value_map ~default:false ~f:(fun decoded ->
             let personal_signed =
               Cstruct.of_string kex_result.shared_hash
               |> Nocrypto.Hash.SHA1.digest |> Cstruct.append oid
             in
             print_s
               [%message
                 (Cstruct.to_string personal_signed : String.Hexdump.t)
                   (Cstruct.to_string decoded : String.Hexdump.t)];
             Cstruct.equal decoded personal_signed)
    ;;
  end

  let ssh_rsa ~host_validator : t = T (host_validator, (module Ssh_rsa))

  let all = [ ssh_rsa ~host_validator:() ]
end

let verify (State (t, _, (module M))) = M.verify t
