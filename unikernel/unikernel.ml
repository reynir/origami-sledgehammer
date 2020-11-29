module ECB = Mirage_crypto.Cipher_block.DES.ECB

let http_src = Logs.Src.create "http" ~doc:"HTTP server"
module Http_log = (val Logs.src_log http_src : Logs.LOG)

module Main (S : Cohttp_lwt.S.Server) = struct

  let start http =
    let http_port = Key_gen.http_port () in
    let tcp = `TCP http_port in
    let key =
      let secret = Mirage_crypto_rng.generate ECB.key_sizes.(0) in
      ECB.of_secret secret in
    let callback = Pastebin.callback key in
    Http_log.info (fun f -> f "Listening on %d/TCP" http_port);
    http tcp @@ S.make ~callback ()

end
