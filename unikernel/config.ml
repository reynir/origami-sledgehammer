open Mirage

let stack = generic_stackv4 default_network
let http_srv = http_server @@ conduit_direct stack

let http_port =
  let doc = Key.Arg.info ~doc:"Listening HTTP port." ["http"] in
  Key.(create "http_port" Arg.(opt int 8081 doc))

let main =
  let packages = [
    package "origami-sledgehammer";
  ] in
  let keys = [ Key.abstract http_port ] in
  foreign
    ~deps:[abstract nocrypto]
    ~packages ~keys
    "Unikernel.Main" (http @-> job)

let () =
  register "origami-sledgehammer" [main $ http_srv]
