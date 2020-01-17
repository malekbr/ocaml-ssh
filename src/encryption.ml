open! Core

module type S = sig
  module Encrypt : sig
    type t
  end

  module Decrypt : sig
    type t
  end

  val name : string

  val block_size : int

  val key_size : int

  val encrypt : Encrypt.t -> string -> string

  val decrypt : Decrypt.t -> string -> string

  val create_encrypt : initialization_vector:string -> key:string -> Encrypt.t

  val create_decrypt : initialization_vector:string -> key:string -> Decrypt.t
end

module Mode = struct
  type encrypt = Encrypt

  type decrypt = Decrypt
end

type 'mode t =
  | State_enc : 'a * (module S with type Encrypt.t = 'a) -> Mode.encrypt t
  | State_dec : 'a * (module S with type Decrypt.t = 'a) -> Mode.decrypt t

module Method = struct
  type t =
    | T : (module S with type Encrypt.t = 'a and type Decrypt.t = 'b) -> t

  let name (T (module M)) = M.name

  let create_encrypt (T ((module M) as t)) ~initialization_vector ~key =
    State_enc
      ( M.create_encrypt ~initialization_vector ~key
      , (t :> (module S with type Encrypt.t = M.Encrypt.t)) )
  ;;

  let create_decrypt (T ((module M) as t)) ~initialization_vector ~key =
    State_dec
      ( M.create_decrypt ~initialization_vector ~key
      , (t :> (module S with type Decrypt.t = M.Decrypt.t)) )
  ;;

  let key_size (T (module M)) = M.key_size

  module Aes128_ctr = struct
    open! Nocrypto.Cipher_block.AES.CTR

    module T = struct
      type t = { key : key; mutable ctr : ctr }

      let create ~initialization_vector ~key =
        let key = of_secret (Cstruct.of_string key) in
        let ctr = ctr_of_cstruct (Cstruct.of_string initialization_vector) in
        { key; ctr }
      ;;
    end

    module Encrypt = T
    module Decrypt = T

    let name = "aes128-ctr"

    let block_size = 16

    let key_size = 16

    let encrypt (t : Encrypt.t) s =
      let result =
        Nocrypto.Cipher_block.AES.CTR.encrypt ~key:t.key ~ctr:t.ctr
          (Cstruct.of_string s)
      in
      t.ctr <- next_ctr ~ctr:t.ctr result;
      Cstruct.to_string result
    ;;

    let decrypt (t : Decrypt.t) s =
      let result =
        Nocrypto.Cipher_block.AES.CTR.decrypt ~key:t.key ~ctr:t.ctr
          (Cstruct.of_string s)
      in
      t.ctr <- next_ctr ~ctr:t.ctr result;
      Cstruct.to_string result
    ;;

    let create_encrypt = T.create

    let create_decrypt = T.create
  end

  let aes128_ctr : t = T (module Aes128_ctr)

  let none : t =
    T
      ( module struct
        module Encrypt = struct
          type t = unit
        end

        module Decrypt = struct
          type t = unit
        end

        let name = "none"

        (* Minimum requirement *)
        let block_size = 8

        let key_size = 0

        let encrypt () s = s

        let decrypt () s = s

        let create_encrypt ~initialization_vector:_ ~key:_ = ()

        let create_decrypt ~initialization_vector:_ ~key:_ = ()
      end )
  ;;

  let all = [ aes128_ctr; none ]
end

let encrypt (State_enc (t, (module M))) = M.encrypt t

let decrypt (State_dec (t, (module M))) = M.decrypt t

let block_size (type mode) (t : mode t) =
  match t with
  | State_enc (_, (module M)) -> M.block_size
  | State_dec (_, (module M)) -> M.block_size
;;
