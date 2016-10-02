open Result
open Lwt.Infix

module Pastes = Store.Make(struct let max = 128 end)

module ECB = Nocrypto.Cipher_block.DES.ECB

let () = ignore @@ Nocrypto_entropy_lwt.initialize ()

let secret = Nocrypto.Rng.generate ECB.key_sizes.(0)
let key = ECB.of_secret secret

let cs_of_int64 n =
  let buf = Cstruct.create 8 in
  let () = Cstruct.BE.set_uint64 buf 0 n in
  buf
  
let int64_of_cs buf =
  Cstruct.BE.get_uint64 buf 0

let re_b64_digit =
  Re.(alt [digit; rg 'a' 'z'; rg 'A'  'Z'; char '/'; char '+'])

let re_b64_num =
  let num_digits = ceil (float_of_int ECB.block_size /. (3. /. 4.)) |> int_of_float in
  let padding = 3 - (ECB.block_size mod 3) in
  Re.seq [Re.repn re_b64_digit num_digits (Some num_digits);
          Re.repn (Re.char '=') padding (Some padding)]


let tyre_b64_num = Tyre.conv ~name:"hexadecimal"
    (fun x -> Cstruct.of_string x |> Nocrypto.Base64.decode)
    (fun x -> Nocrypto.Base64.encode x |> Cstruct.to_string)
    (Tyre.regex re_b64_num)

let tyre_resource = Tyre.("/" *> tyre_b64_num)
let tyre_resource_re = Tyre.compile tyre_resource

let callback conn (req : Cohttp.Request.t) (body : Cohttp_lwt_body.t)
  : (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t =
  match req.Cohttp.Request.meth with
  | `GET ->
    let resource = req.Cohttp.Request.resource in
    let x = Tyre.exec tyre_resource_re resource in
    begin match x with
      | Ok n -> 
        let idx = n
                  |> ECB.decrypt ~key
                  |> int64_of_cs
                  |> Int64.to_int in
        begin match Pastes.get idx with
          | Some s ->
            Lwt.return (Cohttp.Response.make ~headers:(Cohttp.Header.init_with "Content-Type" "text/plain") (),
             Cohttp_lwt_body.of_string s)
          | None ->
            Lwt.return (Cohttp.Response.make ~status:`Not_found (),
                        Cohttp_lwt_body.of_string "Not found\n")
        end
      | Error e ->
        Lwt.return (Cohttp.Response.make ~status:`Bad_request (),
                    Cohttp_lwt_body.of_string "Bad Request\n")
    end
  | `POST ->
    let%lwt body = Cohttp_lwt_body.to_string body in
    let idx = Pastes.put body in
    let path = Int64.of_int idx
              |> cs_of_int64
              |> ECB.encrypt ~key
              |> Tyre.eval tyre_resource in
    let req_url = Cohttp.Request.uri req in
    let url =  Uri.with_scheme (Uri.with_path req_url path) (Some "http") in
    Lwt.return (Cohttp.Response.make ~status:`Created (),
                Cohttp_lwt_body.of_string (Uri.to_string url ^ "\n"))
  | _ ->
    Lwt.return (Cohttp.Response.make ~status:`Bad_request (),
                Cohttp_lwt_body.of_string "Bad request\n")

let server =
  Cohttp_lwt_unix.Server.make ~callback ()

let lwt_main () =
  let%lwt ctx = Conduit_lwt_unix.init ~src:"localhost" () in
  let ctx = Cohttp_lwt_unix_net.init ~ctx () in
Cohttp_lwt_unix.Server.create ~ctx ~mode:(`TCP (`Port 8081)) server

let () = Lwt_main.run @@ lwt_main ()
