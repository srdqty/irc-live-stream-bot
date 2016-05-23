open Core.Std
open Async.Std

let spec =
  let open Command.Spec in
  empty
  +> flag "-devkey" (required string) ~doc:"devkey Ustream developer key"
  +> anon ("filename" %: string)

let command =
  Command.async_basic
  ~summary:"Poll the stream list for live streams"
  ~readme:(fun () -> "Given a file with a ustream stream channel name on each line, the program determines which ones are currently live")
  spec
  (fun devkey filename () -> 
    Async.Std.Reader.file_lines filename >>= fun streams ->
    Ustream_api.get_live_streams ~devkey streams >>| fun live_streams ->
      begin match live_streams with
      | Result.Error error -> printf "Error: %s\n" (Error.to_string_hum error)
      | Result.Ok live_streams ->
        List.iter live_streams ~f:(fun x -> printf "%s is live\n" x)
      end)

let () = Command.run command
