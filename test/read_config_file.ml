open Core.Std
open Async.Std

let spec =
  let open Command.Spec in
  empty
  +> anon ("filename" %: string)

let or_none v =
  Option.value v ~default:"<None>"

let command =
  Command.async_basic
  ~summary:"Display config file options"
  ~readme:(fun () -> "Display config file options")
  spec
  (fun filename () -> 
    Config_file.read_config_file filename >>| fun config ->
      begin match config with
      | Result.Error error -> printf "Error: %s\n" (Error.to_string_hum error)
      | Result.Ok config ->
          printf "Server: %s\n" config.server;
          printf "Port: %d\n" config.port;
          printf "Channel: %s\n\n" config.channel;

          printf "Nickname: %s\n" config.nickname;
          printf "Username: %s\n" config.username;
          printf "Real name: %s\n" config.realname;
          printf "Password: %s\n\n" (or_none config.password);

          printf "Twitch file: %s\n" (or_none config.twitch_streams_file);
          printf "Ustream file: %s\n" (or_none config.ustream_streams_file);
          printf "Ustream dev key: %s\n" (or_none config.ustream_devkey);
          printf "Hitbox file: %s\n" (or_none config.hitbox_streams_file);
      end)

let () = Command.run command
