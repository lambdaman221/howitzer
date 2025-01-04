(* stress_tester.ml *)

open Lwt.Infix
open Cohttp_lwt_unix

let send_request url =
  Client.get (Uri.of_string url) >>= fun (resp, body) ->
  let status = Cohttp.Response.status resp in
  body |> Cohttp_lwt.Body.to_string >>= fun body_str ->
  Lwt_io.printf "Response status: %s\nResponse body: %s\n"
    (Cohttp.Code.string_of_status status) body_str

let rec stress_test url count =
  if count <= 0 then
    Lwt.return ()
  else
    send_request url >>= fun () ->
    stress_test url (count - 1)

let () =
  let url = "https://acme.org" in  (* URL of target *)
  let request_count = 500 in           (* Number of requests to send *)
  let _ = Lwt_main.run (stress_test url request_count) in
  ()
