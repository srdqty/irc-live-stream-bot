open Core.Std
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

let json_from_file filename =
  Deferred.Or_error.try_with 
    (fun () ->
      Reader.file_contents filename >>= fun json_string ->
      Deferred.return (Yojson.Safe.from_string json_string))

let find_required f key = function
  | `Assoc lst ->
      Option.value_map (List.Assoc.find lst key)
        ~default:(Deferred.return
                   (Or_error.errorf
                      "Config file: required field not found: %s."
                       key))
        ~f:(fun v -> f key v)
  | _ -> Deferred.Or_error.error_string 
             "Config file: JSON object expected, but not found."

let find_optional f default key = function
  | `Assoc lst ->
      Option.value_map (List.Assoc.find lst key)
        ~default:(Deferred.Or_error.return default) 
        ~f:(fun v -> f key v)
  | _ -> Deferred.Or_error.error_string 
             "Config file: JSON object expected, but not found."

let is_string field = function
  | `String s -> Deferred.Or_error.return s
  | _ ->  Deferred.return
           (Or_error.errorf
              "Config file: field \"%s\" value is not a string" field)

let is_int field = function
  | `Int s -> Deferred.Or_error.return s
  | _ ->  Deferred.return
           (Or_error.errorf
              "Config file: field \"%s\" value is not an int" field)

let is_some_string field = function
  | `String s -> Deferred.Or_error.return (Some s)
  | _ ->  Deferred.return
           (Or_error.errorf
              "Config file: field \"%s\" value is not a string" field)

let read_config_file filename =
  let (>>=) = Deferred.Or_error.Monad_infix.(>>=) in
  json_from_file filename >>= fun json ->
  find_required is_string "server" json >>= fun server ->
  find_required is_string "nickname" json >>= fun nickname ->
  find_required is_string "channel" json >>= fun channel ->
  
  find_optional is_int 6667 "port" json >>= fun port ->

  find_optional is_string nickname "username" json >>= fun username ->
  find_optional is_string nickname "realname" json >>= fun realname ->
  find_optional is_some_string None "password" json >>= fun password ->

  find_optional is_some_string None "twitch-streams-file" json 
    >>= fun twitch_streams_file ->
  find_optional is_some_string None "ustream-streams-file" json
    >>= fun ustream_streams_file ->
  find_optional is_some_string None "twitch-streams-file" json
    >>= fun ustream_devkey ->
  find_optional is_some_string None "hitbox-streams-file" json
    >>= fun hitbox_streams_file ->
  
  Deferred.Or_error.return
  {
    server; nickname; channel; port; username; realname; password;
    twitch_streams_file;
    ustream_streams_file; ustream_devkey;
    hitbox_streams_file
  }
