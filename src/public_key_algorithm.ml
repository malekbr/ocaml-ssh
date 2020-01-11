module type S = sig
  type t

  val name : string
end

type t = (module S)

let name (module M : S) = M.name

let ssh_dss : t =
  ( module struct
    type t

    let name = "ssh-dss"
  end )
;;

let ssh_rsa : t =
  ( module struct
    type t

    let name = "ssh-rsa"
  end )
;;

let all = [ ssh_rsa; ssh_dss ]
