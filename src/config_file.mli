open Async.Std

type config =
  {
    server: string;
    port: int;
    channel: string;
    
    nickname: string;
    
    username: string;
    realname: string;
    password: string option;

    twitch_streams_file: string option;
    ustream_streams_file: string option;
    ustream_devkey: string option;
    hitbox_streams_file: string option
  }

val read_config_file: string -> config Deferred.Or_error.t
