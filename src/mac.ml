open! Core

module type S = sig
  type t

  val name : string

  val signature : t -> string -> string

  val verify : t -> message:string -> signature:string -> bool

  val signature_length : int

  val key_length : int

  val create : key:string -> t
end

type t = State : 'a * (module S with type t = 'a) -> t

module Method = struct
  type t = T : (module S with type t = 'a) -> t

  let name (T (module M)) = M.name

  let create (T ((module M) as t)) ~key = State (M.create ~key, t)

  let key_length (T (module M)) = M.key_length

  let none : t =
    T
      ( module struct
        type t = unit

        let name = "none"

        let signature () (_ : string) = ""

        let verify () ~message:_ ~signature:_ = true

        let signature_length = 0

        let key_length = 0

        let create ~key:_ = ()
      end )
  ;;

  module Hmac_sha1 = struct
    type t = string -> string

    let name = "hmac-sha1"

    let signature t = t

    let verify t ~message ~signature = t message |> String.equal signature

    let signature_length = 20

    let key_length = 20

    let create ~key =
      let hash = Nocrypto.Hash.mac `SHA1 ~key:(Cstruct.of_string key) in
      fun input -> Cstruct.of_string input |> hash |> Cstruct.to_string
    ;;
  end

  let hmac_sha1 : t = T (module Hmac_sha1)

  module Hmac_md5 = struct
    type t = string -> string

    let name = "hmac-md5"

    let signature t = t

    let verify t ~message ~signature = t message |> String.equal signature

    let signature_length = 16

    let key_length = 16

    let create ~key =
      let hash = Nocrypto.Hash.mac `MD5 ~key:(Cstruct.of_string key) in
      fun input -> Cstruct.of_string input |> hash |> Cstruct.to_string
    ;;
  end

  let hmac_md5 : t = T (module Hmac_md5)

  let all = [ hmac_sha1 ]
end

let signature_length (State (_, (module M))) = M.signature_length

let signature (State (t, (module M))) = M.signature t

let verify (State (t, (module M))) = M.verify t
