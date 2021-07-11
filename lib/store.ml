module type S = sig
  val put : string -> int64
  val get : int64 -> string option
end

module Make(Max : sig val max : int end) : S =
struct
  open Max

  let wrap_around =
    let max_int_pred = Int64.sub (Int64.of_int max_int) 1L in
    Int64.(sub max_int_pred (rem max_int_pred (of_int max)))

  let counter = ref (-1L)
  let alloc () =
    Int64.(counter := rem (add !counter 1L) wrap_around);
    !counter

  let store : string option array = Array.make max None

  let put s : int64 =
    let idx = alloc () in
    store.(Int64.to_int idx mod max) <- Some s;
    idx

  let get idx : string option =
    if Int64.to_int idx >= 0 &&
       Int64.(sub !counter (of_int max) < idx) &&
       idx <= !counter &&
       Int64.of_int idx < wrap_around
    let idx = Int64.to_int idx in
    then store.(idx mod max)
    else None
end
