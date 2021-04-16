open Cmdliner
open Lwt.Infix

module ECB = Mirage_crypto.Cipher_block.DES.ECB

let lwt_main proxy_scheme_header src port =
  let () = Mirage_crypto_rng_lwt.initialize () in
  let key =
    let secret = Mirage_crypto_rng.generate ECB.key_sizes.(0) in
    ECB.of_secret secret in
  let server =
    let callback = Pastebin.callback ?proxy_scheme_header key in
    Cohttp_lwt_unix.Server.make ~callback () in
  Conduit_lwt_unix.init ?src () >>= fun ctx ->
  let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
  Cohttp_lwt_unix.Server.create ~ctx ~mode:(`TCP (`Port port)) server

let host =
  let doc = "Source interface to bind to" in
  Arg.(value & opt (some string) (Some "localhost") & info ["host"] ~doc)

let port =
  let doc = "Port to listen on" in
  Arg.(value & opt int 8081 & info ["port"; "p"] ~doc)

let proxy_scheme_header =
  let doc = "Header with scheme when behind a proxy" in
  Arg.(value & opt (some string) None & info ["proxy-scheme-header"] ~doc)

let main =
  Term.(pure lwt_main $ proxy_scheme_header $ host $ port)

let info =
  let doc = "Run pastebin server" in
  let man = [`S "BUGS";
             `P "Submit bugs at https://github.com/reynir/origami-sledgehammer/issues"]
  in Term.info "origami-sledgehammer" ~doc ~man

let () =
  match Term.eval (main, info) with
  | `Help | `Version -> exit 1
  | `Error _ -> exit 1
  | `Ok main -> Lwt_main.run main
