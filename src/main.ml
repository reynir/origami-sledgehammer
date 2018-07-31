open Cmdliner

let host =
  let doc = "Source interface to bind to" in
  Arg.(value & opt (some string) (Some "localhost") & info ["host"] ~doc)

let port =
  let doc = "Port to listen on" in
  Arg.(value & opt int 8081 & info ["port"; "p"] ~doc)

let main =
  Term.(pure Pastebin.lwt_main $ host $ port)

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
