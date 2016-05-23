(*
 * Given a list of ustream stream channel names, returns
 * a list of stream channel urls for the stream channels
 * that are currently live.
 *)
val get_live_streams :
  devkey:string -> 
  string list ->
  string list Async.Std.Deferred.Or_error.t
