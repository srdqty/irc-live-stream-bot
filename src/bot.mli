open Core.Std
open Async.Std

val process_message :
  get_live_streams:(unit -> string list) ->
  get_all_streams:(unit -> (string * string list) list) ->
  get_last_poll_success:(unit -> (string * Time.t option) list) ->
  Easy_irc_client.irc_writer -> Easy_irc_client.message -> unit Deferred.Or_error.t

val launch_recent_live_notifier :
  get_recent_live_streams:(unit -> string list Deferred.t) ->
  channel:string ->
  Easy_irc_client.irc_writer -> unit
