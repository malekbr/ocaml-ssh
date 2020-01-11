module type S = sig
  type t

  val name : string

  val block_size : int

  val encrypt : t -> string -> string

  val decrypt : t -> string -> string

  val create : initialization_vector:string -> key:string -> t
end

type t = State : 'a * (module S with type t = 'a) -> t

module Method = struct
  type t = T : (module S with type t = 'a) -> t

  let name (T (module M)) = M.name

  let create (T ((module M) as t)) ~initialization_vector ~key =
    State (M.create ~initialization_vector ~key, t)
  ;;

  module Aes128_crt = struct
    type t

    let name = "aes128-crt"

    let block_size = 16

    let encrypt _ _ = assert false

    let decrypt _ _ = assert false

    let create ~initialization_vector:_ ~key:_ = assert false
  end

  let aes128_crt : t = T (module Aes128_crt)

  let none : t =
    T
      ( module struct
        type t = unit

        let name = "none"

        (* Minimum requirement *)
        let block_size = 8

        let encrypt () s = s

        let decrypt () s = s

        let create ~initialization_vector:_ ~key:_ = ()
      end )
  ;;

  let all = [ aes128_crt ]
end

let encrypt (State (t, (module M))) = M.encrypt t

let decrypt (State (t, (module M))) = M.decrypt t

let block_size (State (_, (module M))) = M.block_size
