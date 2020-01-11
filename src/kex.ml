module type S = sig
  type t

  val name : string
end

type t = (module S)

let name (module M : S) = M.name

let diffie_hellman_group1_sha1 : t =
  ( module struct
    type t

    let name = "diffie-hellman-group1-sha1"
  end )
;;

let diffie_hellman_group14_sha1 : t =
  ( module struct
    type t

    let name = "diffie-hellman-group14-sha1"
  end )
;;

let all = [ diffie_hellman_group1_sha1; diffie_hellman_group14_sha1 ]
