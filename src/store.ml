module Make(Max : sig val max : int end) : sig
  val put : string -> int
  val get : int -> string option
end = struct
  open Max

  let counter = ref (-1)
  let alloc () =
    incr counter; !counter mod max

  let store : string option array = Array.make max None

  let put s : int =
    let idx = alloc () mod max in
    store.(idx) <- Some s;
    idx

  let get idx : string option =
    if idx >= 0 && idx < max
    then store.(idx)
    else None
end
