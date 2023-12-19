open Core

module Point : sig
  type t = int * int

  val compare : t -> t -> int
  val sexp_of_t : t -> Sexp.t
  val hash : t -> int
end = struct
  type t = int * int

  let compare x y = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare x y
  let sexp_of_t = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t
  let hash = Hashtbl.hash
end
