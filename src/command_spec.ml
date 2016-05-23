open Core.Std
open Async.Std

let config_file_spec =
  let open Command.Spec in
  empty
  +> anon ("config-file" %: file)
  +> flag "-debug" no_arg ~doc:" Print debugging messages"

let cli_spec =
  let open Command.Spec in
  step 
      (fun m server
             nickname
             channel
             twitch_streams_file
             ustream_streams_file
             ustream_devkey
             hitbox_streams_file
             username
             realname
             password
             port
             debug ->
        m ~server ~nickname ~channel 
          ?twitch_streams_file ?ustream_streams_file ?ustream_devkey ?hitbox_streams_file
          ?username ?realname ?password ?port ~debug)
  +> flag "-server" (required string)
          ~doc:"hostname The IRC server hostname or ip address"
  +> flag "-nickname" (required string )
          ~doc:"nickname The nickname sent to the IRC server"
  +> flag "-channel" (required string)
          ~doc:( "channel-name The IRC channel to send notices of recently live"
               ^ " streams")
  +> flag "-twitch-streams-file" (optional file)
          ~doc:( "filename The file containing the Twitch stream"
               ^ " channels to poll")
  +> flag "-ustream-streams-file" (optional file)
          ~doc:( "filename The file containing the Ustream stream"
               ^ " channels to poll")
  +> flag "-ustream-devkey" (optional file)
          ~doc:( "devkey Ustream devkey to use with Ustream API")
  +> flag "-hitbox-streams-file" (optional file)
          ~doc:( "filename The file containing the Hitbox stream"
               ^ " channels to poll")
  +> flag "-username" (optional string)
          ~doc:"username The username sent to the IRC server"
  +> flag "-realname" (optional string)
          ~doc:"real-name The real name sent to the IRC server"
  +> flag "-password" (optional string)
          ~doc:"password The password sent to the IRC server"
  +> flag "-port" (optional int)
          ~doc:"port The port sent to the IRC server"
  +> flag "-debug" no_arg
          ~doc:" Print debugging messages"

let summary = "IRC bot that polls twitch.tv, ustream.tv, and/or hitbox.tv for live streams."

let readme () = 
  "This is an IRC bot that polls twitch.tv, ustream.tv, and/or hitbox.tv for live streams.\n\
  The bot can idle in a channel and send a message to the channel whenever a\n\
  stream goes live.\n\
  The bot also responds to three commands (sent as private messages to the bot):\n\
  'gimme live streams':   The bot responds with a list of currently live streams.\n\
  'gimme all streams':    The bot responds with a list of all the streams the bot\n\
  \                        polls for live status.\n\
  'gimme polling status': The bot responds with the amount of time since the\n\
  \                        last successful poll for live streams.\n"

let cli_summary = "Use the command line interface to configure and run the bot"

let config_file_summary = "Load a config file to configure and run the bot"

let config_file_readme =
"Config file format (JSON):
{
  \"server\": \"<IRC server hostname or ip address>\" (required),
  \"nickname\": \"<IRC nickname>\" (required),
  \"channel\": \"<IRC channel bot will join>\" (required),

  \"username\": \"<IRC username>\" (optional, defaults to nickname value),
  \"realname\": \"<IRC real name>\" (optional, defaults to nickname value),
  \"password\": \"<IRC password>\" (optional),
  \"port\": <port number (JSON int)> (optional),

  \"twitch-streams-file\": \"<filename>\" (optional),
  \"ustream-streams-file\": \"<filename>\" (optional),
  \"ustream-devkey\": \"<developer key>\" (optional),
  \"hitbox-streams-file\": \"<filename>\" (optional)
}"

let config_file_main main config_file debug () =
  let open Config_file in
    Config_file.read_config_file config_file >>= fun or_error ->
    begin match or_error with
      | Error error ->
          printf "%s" (Error.to_string_hum error); Deferred.unit
      | Ok { server; port; channel; nickname; username; realname; password;
          twitch_streams_file; hitbox_streams_file;
          ustream_streams_file; ustream_devkey } ->
          main ~server ~nickname ~channel
               ?twitch_streams_file ?ustream_streams_file
               ?ustream_devkey ?hitbox_streams_file
               ?username:(Some username) ?realname:(Some realname) ?password ?port:(Some port) ~debug ()
    end

let run_main main =
  let cli_command = 
    Command.async_basic ~summary:cli_summary cli_spec main
  in
  let config_file_command =
    Command.async_basic 
      ~summary:config_file_summary
      ~readme:(fun () -> config_file_readme)
      config_file_spec
      (config_file_main main)
  in
  let command =
    Command.group ~summary ~readme
      ["cli", cli_command; "config-file", config_file_command]
  in
    Command.run command
