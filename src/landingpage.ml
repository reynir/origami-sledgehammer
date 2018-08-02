open Tyxml

let html url =
  (* Any way to detect the cheme? *)
  let url' = Uri.with_scheme url (Some "http") in
  let url = Uri.to_string url in
  let url' = Uri.to_string url' in
  let open Html in
  let mytitle = title (pcdata "Origami Sledgehammer Pastebin Service")
  and mybody = [
    p [pcdata "To use this service, simply make a ";
       code [pcdata "HTTP POST"];
       pcdata " request with the desied contents of the paste.";
       pcdata " E.g. using ";
       code [pcdata "curl"];
       pcdata ":"];
    pre [pcdata "echo Hello, World | curl --data-binary @- ";
         a ~a:[a_href url] [pcdata url']];
  ]
  in html
    (head mytitle [])
    (body mybody)

let html_string url =
  let buf = Buffer.create 512 in
  let buf_formatter = Format.formatter_of_buffer buf in
  Tyxml.Html.pp () buf_formatter (html url);
  Format.pp_print_flush buf_formatter ();
  Buffer.contents buf
