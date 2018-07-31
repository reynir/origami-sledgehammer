module type S = sig
  val put : string -> int64
  val get : int64 -> string option
end

module Make(Max : sig val max : int end) : S =
struct
  open Max

  let counter = ref (-1)
  let alloc () =
    incr counter; !counter mod max

  let store : string option array = Array.make max None

  let put s : int64 =
    let idx = alloc () mod max in
    store.(idx) <- Some s;
    Int64.of_int idx

  let get idx : string option =
    let idx = Int64.to_int idx in
    if idx >= 0 && idx < max
    then store.(idx)
    else None
end
