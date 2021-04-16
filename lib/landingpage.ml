open Tyxml

let html url =
  let url = Uri.to_string url in
  let open Html in
  let mytitle = title (Html.txt "Origami Sledgehammer Pastebin Service")
  and mybody = [
    p [Html.txt "To use this service, simply make a ";
       code [Html.txt "HTTP POST"];
       Html.txt " request with the desied contents of the paste.";
       Html.txt " E.g. using ";
       code [Html.txt "curl"];
       Html.txt ":"];
    pre [Html.txt "echo Hello, World | curl --data-binary @- ";
         a ~a:[a_href url] [Html.txt url]];
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
