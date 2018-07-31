module type S = {let put: string => int64; let get: int64 => option(string);};

module Make: (Max: {let max: int;}) => S;
