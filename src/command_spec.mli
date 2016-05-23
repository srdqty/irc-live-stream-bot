val run_main :
  (server:string ->
   nickname:string ->
   channel:string ->
   ?twitch_streams_file:string ->
   ?ustream_streams_file:string ->
   ?ustream_devkey:string ->
   ?hitbox_streams_file:string ->
   ?username:string ->
   ?realname:string ->
   ?password:string ->
   ?port:int -> debug:bool -> unit -> unit Async_kernel.Deferred.t) ->
  unit
