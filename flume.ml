open Core.Std
open Async.Std

type t = {url: Uri.t;
          bulk: Uri.t;
          single: Uri.t;
          token: String.t}

let base_url = Uri.of_string "https://logs-01.loggly.com/"

let msg_to_iso8601_string msg =
  Time.format (Log.Message.time msg) "%Y-%m-%dT%H:%M:%SZ"

let body_of_msg msg =
  Flume_entry_j.string_of_t
  @@ (("timestamp", msg_to_iso8601_string msg)::
      ("message", String.strip @@ Log.Message.message msg)::
      (Log.Message.tags msg))

let process_tags base =
  List.fold ~init:base ~f:(fun acc el -> acc ^ el ^ ",")

let construct_bulk_url sys tags =
  Uri.with_path sys.bulk @@ (Uri.path sys.bulk) ^ tags

let publish_log_message headers body url =
  Cohttp_async.Client.post ~headers ~body url
  >>= fun (response, _) ->
  match response.status with
  | `OK -> return ()
  | _ ->
    Cohttp_async.Body.to_string body
    >>= fun str_body ->
    print_string
    @@ Sexp.to_string
    @@ Sexp.List [Sexp.Atom str_body;
                  Cohttp_async.Response.sexp_of_t response];
    return ()

let syslog_messages sys tags msgs =
  let str_body = Queue.fold ~init:"" ~f:(fun acc msg ->
      let str = body_of_msg msg in
      acc ^ str ^ "\n") msgs in
  let headers = Cohttp.Header.of_list [("content-type", "application/json")] in
  let body = Cohttp_async.Body.of_string str_body in
  let url = construct_bulk_url sys tags in
  Deferred.don't_wait_for @@ publish_log_message headers body url;
  return ()

let create_log ?url
    ?level:(level=`Info)
    ?tags:(tags=[])
    (app_name:String.t)
    token =
  let root_url = match url with
    | Some tmp_root -> tmp_root
    | None -> base_url in
  let bulk = Uri.with_path root_url ("/bulk/" ^ token ^ "/tags/") in
  let single = Uri.with_path root_url ("/inputs/" ^ token ^ "/tags/") in
  let tags =  process_tags app_name tags in
  let sys = {url = root_url; bulk; single; token} in
  Log.create ~level ~output:[Log.Output.create (syslog_messages sys tags)]
