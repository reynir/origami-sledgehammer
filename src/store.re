module type S = {let put: string => int64; let get: int64 => option(string);};

module Make = (Max: {let max: int;}) : S => {
  open Max;

  let counter = ref(-1L);
  let alloc = () => {
    Int64.(counter := rem(add(counter^, 1L), of_int(max)));
    counter^;
  };

  let store: array(option(string)) = Array.make(max, None);

  let put = s : int64 => {
    let idx = alloc();
    store[Int64.to_int(idx)] = Some(s);
    idx;
  };

  let get = idx : option(string) => {
    let idx = Int64.to_int(idx);
    if (idx >= 0 && idx < max) {
      store[idx];
    } else {
      None;
    };
  };
};
