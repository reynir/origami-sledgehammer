module type S = sig
  val put : string -> int64
  val get : int64 -> string option
end

module Make :
  functor (Max : sig val max : int end) ->
    S
