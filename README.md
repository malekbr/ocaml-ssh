This is an implementation of the SSH client protocol in OCaml that:
- Uses async.
- Is configurable in terms of what encryption/compression/verification/key exchange algorithms
it supports, i.e. algorithms can be added from outside the library.

This is a proof of concept, and is in no way clean code. Compression has not been tested.
