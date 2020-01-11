open! Core

module type S = sig
  type t

  val name : string

  val signature : t -> string -> string

  val verify : t -> message:string -> signature:string -> bool

  val signature_length : int

  val create : key:string -> t
end

type t = State : 'a * (module S with type t = 'a) -> t

module Method = struct
  type t = T : (module S with type t = 'a) -> t

  let name (T (module M)) = M.name

  let create (T ((module M) as t)) ~key = State (M.create ~key, t)

  let none : t =
    T
      ( module struct
        type t = unit

        let name = "none"

        let signature () (_ : string) = ""

        let verify () ~message:_ ~signature:_ = true

        let signature_length = 0

        let create ~key:_ = ()
      end )
  ;;

  module Hmac_sha1 = struct
    type t

    let name = "hmac-sha1"

    let signature _ _ = assert false

    let verify _ ~message:_ ~signature:_ = assert false

    let signature_length = 20

    let create ~key:_ = assert false
  end

  let hmac_sha1 : t = T (module Hmac_sha1)

  let all = [ hmac_sha1 ]
end

let signature_length (State (_, (module M))) = M.signature_length

let signature (State (t, (module M))) = M.signature t

let verify (State (t, (module M))) = M.verify t
