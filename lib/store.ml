module type S = sig
  val put : string -> int64
  val get : int64 -> string option
end

module Make(Max : sig val max : int end) : S =
struct
  open Max

  let counter = ref (-1L)
  let alloc () =
    Int64.(counter := rem (add !counter 1L) (of_int max));
    !counter

  let store : string option array = Array.make max None

  let put s : int64 =
    let idx = alloc () in
    store.(Int64.to_int idx) <- Some s;
    idx

  let get idx : string option =
    let idx = Int64.to_int idx in
    if idx >= 0 && idx < max
    then store.(idx)
    else None
end
