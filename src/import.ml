module Uint32 = struct
  include Stdint.Uint32

  let sexp_of_t t = Core.Sexp.Atom (to_string t)
end

module Uint64 = struct
  include Stdint.Uint64

  let sexp_of_t t = Core.Sexp.Atom (to_string t)
end
