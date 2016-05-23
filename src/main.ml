open Core.Std
open Async.Std
module IRC = Easy_irc_client

let main
  ~server
  ~nickname
  ~channel
  ?twitch_streams_file
  ?ustream_streams_file
  ?ustream_devkey
  ?hitbox_streams_file
  ?(username=nickname)
  ?(realname=nickname)
  ?password
  ?(port=6667)
  ~debug
  () =
  let (>>>=) = Deferred.Or_error.Monad_infix.(>>=) in
  begin match debug with
    | true -> Log.Global.set_level `Debug
    | false -> ()
  end;
  begin
    Streams_polling.launch_stream_pollers 
        ?twitch_streams_file
        ?ustream_streams_file
        ?ustream_devkey
        ?hitbox_streams_file () >>>= fun 
      { 
        Streams_polling.get_live_streams;
        Streams_polling.get_all_streams;
        Streams_polling.get_recent_live_streams;
        Streams_polling.get_last_poll_success;
      }  ->
    let irc_writer, irc_connection_deferred =
      IRC.with_connection'
        ~server
        ~nickname
        ~username
        ~realname
        ?password
        ~port
        ~channels:[channel]
        ~process_message:
          (Bot.process_message 
            ~get_live_streams
            ~get_all_streams
            ~get_last_poll_success)
        ()
    in
      List.iter get_recent_live_streams
        ~f:(fun get_recent_live_streams ->
              Bot.launch_recent_live_notifier 
                ~get_recent_live_streams
                ~channel
                irc_writer);
      irc_connection_deferred
  end >>= function
    | Ok () -> 
        Log.Global.debug "Successful termination.";
        Log.Global.flushed ()
    | Error error -> 
        Log.Global.error "%s" (Error.to_string_hum error);
        Log.Global.flushed ()

let () = Command_spec.run_main main
