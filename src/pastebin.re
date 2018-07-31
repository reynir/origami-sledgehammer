open Result;

module Pastes =
  Store.Make({
    let max = 128;
  });

module ECB = Nocrypto.Cipher_block.DES.ECB;

let () = ignore @@ Nocrypto_entropy_lwt.initialize();

let key = {
  let secret = Nocrypto.Rng.generate(ECB.key_sizes[0]);
  ECB.of_secret(secret);
};

let cs_of_int64 = n => {
  let buf = Cstruct.create(8);
  let () = Cstruct.BE.set_uint64(buf, 0, n);
  buf;
};

let int64_of_cs = buf => Cstruct.BE.get_uint64(buf, 0);

let re_b64_digit =
  Re.(alt([digit, rg('a', 'z'), rg('A', 'Z'), char('/'), char('+')]));

let re_b64_num = {
  let num_digits =
    ceil(float_of_int(ECB.block_size) *. 4. /. 3.) |> int_of_float;
  let padding_size = 3 - ECB.block_size mod 3;
  let padding = Re.repn(Re.char('='), padding_size, Some(padding_size));
  Re.seq([
    Re.repn(re_b64_digit, num_digits, Some(num_digits)),
    Re.opt(padding),
  ]);
};

let tyre_b64_num =
  Tyre.conv(
    x => B64.decode(x) |> Cstruct.of_string |> int64_of_cs,
    x => cs_of_int64(x) |> Cstruct.to_string |> B64.encode(~pad=false),
    Tyre.regex(re_b64_num),
  );

let tyre_resource = Tyre.(str("/p/") *> tyre_b64_num);
let tyre_resource_re = Tyre.compile(tyre_resource);

let callback =
    (_conn, req: Cohttp.Request.t, body: Cohttp_lwt.Body.t)
    : Lwt.t((Cohttp.Response.t, Cohttp_lwt.Body.t)) =>
  switch (req.Cohttp.Request.meth) {
  | `GET =>
    let resource = req.Cohttp.Request.resource;
    let x = Tyre.exec(tyre_resource_re, resource);
    switch (x) {
    | Ok(n) =>
      let idx = cs_of_int64(n) |> ECB.decrypt(~key) |> int64_of_cs;
      switch (Pastes.get(idx)) {
      | Some(s) =>
        Lwt.return((
          Cohttp.Response.make(
            ~headers=Cohttp.Header.init_with("Content-Type", "text/plain"),
            (),
          ),
          Cohttp_lwt.Body.of_string(s),
        ))
      | None =>
        Lwt.return((
          Cohttp.Response.make(~status=`Not_found, ()),
          Cohttp_lwt.Body.of_string("Not found\n"),
        ))
      };
    | Error(_) =>
      Lwt.return((
        Cohttp.Response.make(~status=`Bad_request, ()),
        Cohttp_lwt.Body.of_string("Bad Request\n"),
      ))
    };
  | `POST =>
    let%lwt body = Cohttp_lwt.Body.to_string(body);
    let idx = Pastes.put(body);
    let path =
      cs_of_int64(idx)
      |> ECB.encrypt(~key)
      |> int64_of_cs
      |> Tyre.eval(tyre_resource);
    let req_url = Cohttp.Request.uri(req);
    let url = Uri.with_scheme(Uri.with_path(req_url, path), Some("http"));
    Lwt.return((
      Cohttp.Response.make(~status=`Created, ()),
      Cohttp_lwt.Body.of_string(Uri.to_string(url) ++ "\n"),
    ));
  | _ =>
    Lwt.return((
      Cohttp.Response.make(~status=`Bad_request, ()),
      Cohttp_lwt.Body.of_string("Bad request\n"),
    ))
  };

let server = Cohttp_lwt_unix.Server.make(~callback, ());

let lwt_main = (src, port) => {
  let%lwt ctx = Conduit_lwt_unix.init(~src?, ());
  let ctx = Cohttp_lwt_unix.Net.init(~ctx, ());
  Cohttp_lwt_unix.Server.create(~ctx, ~mode=`TCP(`Port(port)), server);
};
