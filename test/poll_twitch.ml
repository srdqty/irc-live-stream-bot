open Core.Std
open Async.Std

let spec =
  let open Command.Spec in
  empty
  +> anon ("filename" %: string)

let command =
  Command.async_basic
  ~summary:"Poll the stream list for live streams"
  ~readme:(fun () -> "Given a file with a twitch stream channel name on each line, the program determines which ones are currently live")
  spec
  (fun filename () -> 
    Async.Std.Reader.file_lines filename >>= fun streams ->
    Twitch_api.get_live_streams streams >>| fun live_streams ->
      begin match live_streams with
      | Result.Error error -> printf "Error: %s\n" (Error.to_string_hum error)
      | Result.Ok live_streams ->
        List.iter live_streams ~f:(fun x -> printf "%s is live\n" x)
      end)

let () = Command.run command
