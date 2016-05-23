open Core.Std
open Async.Std

type stream_polling_query = {
  get_live_streams : unit -> string list;
  get_recent_live_streams : (unit -> string list Deferred.t) list;
  get_all_streams : unit -> (string * string list) list;
  get_last_poll_success : unit -> (string * Time.t option) list;
}

val launch_stream_pollers :
  ?twitch_streams_file:string ->
  ?ustream_streams_file:string ->
  ?ustream_devkey:string ->
  ?hitbox_streams_file:string ->
  unit ->
  stream_polling_query Deferred.Or_error.t
