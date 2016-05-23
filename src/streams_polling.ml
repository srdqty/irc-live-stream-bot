open Core.Std
open Async.Std

type stream_polling_query =
  {
    get_live_streams : unit -> string list;
    get_recent_live_streams : (unit -> string list Deferred.t) list;
    get_all_streams : unit -> (string * string list) list;
    get_last_poll_success : unit -> (string * Time.t option) list;
  }

type stream_polling_state =
  {
    recent_live_writer : string list Pipe.Writer.t;
    recent_live_reader : string list Pipe.Reader.t;
    mutable live: string list;
    mutable all : string list;
    mutable last_poll_success : Time.t option;
  }

let lset_difference lst1 lst2 =
  List.filter lst1
    ~f:(fun x -> not (List.mem lst2 x))

let poll_streams streams_state update_live interval =
  Clock.every' interval (fun () -> update_live streams_state) 

let watch_file streams_state filename update_live=
  let file_change_reader = Sys.when_file_changes filename in
  let rec loop () =
    Pipe.read file_change_reader >>= fun read_result ->
    Log.Global.debug "Received file_change_reader event";
    begin match read_result with
      | `Eof -> Deferred.Or_error.error_string
                  "unexpectd end of file change pipe"
      | `Ok _ ->
        Log.Global.debug "Streams file (%s) updated.\n" filename;
        Async.Std.Reader.file_lines filename >>= fun streams ->
        streams_state.all <- streams;
        update_live streams_state >>= fun () ->
        loop ()
    end
  in
    Deferred.don't_wait_for
     (Deferred.Or_error.try_with_join loop >>= fun or_error ->
      begin match or_error with
        | Error error -> Log.Global.error "%s" (Error.to_string_hum error);
                         Log.Global.error "watch_file %s exiting." filename;
                         Log.Global.flushed ()
        | Ok () -> Deferred.unit
      end)

let initialize_streams_state filename =
  let reader, writer = Pipe.create () in
  let streams_state =
    { 
      recent_live_reader = reader;
      recent_live_writer = writer;
      live = [];
      all = [];
      last_poll_success = None
    }
  in
    Async.Std.Reader.file_lines filename >>= fun all_streams ->
    streams_state.all <- all_streams;
    Deferred.Or_error.return streams_state

let launch_streams_poller interval filename update_live_streams =
  let (>>>=) = Deferred.Or_error.Monad_infix.(>>=) in
  Deferred.Or_error.try_with_join (fun () ->
    begin
      initialize_streams_state filename >>>= fun streams_state ->
      let get_recent_live_streams reader =
        Pipe.read reader >>= fun read_result ->
        begin match read_result with
          | `Eof -> Deferred.return []
          | `Ok lst -> Deferred.return lst
        end
      in
        watch_file streams_state filename update_live_streams;
        poll_streams streams_state update_live_streams interval;
        Deferred.Or_error.return
        (
          (fun () -> streams_state.live),
          (fun () -> get_recent_live_streams streams_state.recent_live_reader),
          (fun () -> streams_state.all),
          (fun () -> streams_state.last_poll_success)
        )
    end)

let twitch_update_live streams_state =
  Log.Global.debug "Polling twitch";
  Twitch_api.get_live_streams streams_state.all >>= fun live_streams ->
  begin match live_streams with
    | Result.Error error ->
        Log.Global.error "Twitch: %s" (Error.to_string_hum error);
        Log.Global.flushed ()
    | Result.Ok live ->
        let recent_live = lset_difference live streams_state.live in
          Log.Global.debug "Twitch: Current live streams: %s" 
              (Sexp.to_string 
                  (List.sexp_of_t String.sexp_of_t live));
          Log.Global.debug "Twitch: Recent live streams: %s"
              (Sexp.to_string 
                  (List.sexp_of_t String.sexp_of_t recent_live));
          streams_state.live <- live;
          (* Don't send a recent live list on the first successful
           * poll to avoid spamming the IRC channel when first
           * connecting to the server and building the initial
           * live stream list.
           *)
          begin match (streams_state.last_poll_success, recent_live) with
            | None, _ | Some _, []-> ()
            | Some _, recent_live -> 
                Pipe.write_without_pushback streams_state.recent_live_writer
                                            recent_live
          end;
          streams_state.last_poll_success <- Some (Time.now());
          Deferred.unit
  end

let launch_twitch_poller 
  ?(polling_interval=Time.Span.of_min 5.0)
  ~streams_filename
  ()
=
  launch_streams_poller polling_interval streams_filename twitch_update_live

let ustream_update_live ~devkey streams_state =
  Log.Global.debug "Polling ustream";
  Ustream_api.get_live_streams ~devkey streams_state.all >>= fun live_streams ->
  begin match live_streams with
    | Result.Error error ->
        Log.Global.error "Ustream: %s" (Error.to_string_hum error);
        Log.Global.flushed ()
    | Result.Ok live ->
        let recent_live = lset_difference live streams_state.live in
          Log.Global.debug "Ustream: Current live streams: %s" 
              (Sexp.to_string 
                  (List.sexp_of_t String.sexp_of_t live));
          Log.Global.debug "Ustream: Recent live streams: %s"
              (Sexp.to_string 
                  (List.sexp_of_t String.sexp_of_t recent_live));
          streams_state.live <- live;
          (* Don't send a recent live list on the first successful
           * poll to avoid spamming the IRC channel when first
           * connecting to the server and building the initial
           * live stream list.
           *)
          begin match (streams_state.last_poll_success, recent_live) with
            | None, _ | Some _, []-> ()
            | Some _, recent_live -> 
                Pipe.write_without_pushback streams_state.recent_live_writer
                                            recent_live
          end;
          streams_state.last_poll_success <- Some (Time.now());
          Deferred.unit
  end


let launch_ustream_poller 
  ?(polling_interval=Time.Span.of_min 5.0)
  ~streams_filename
  ~devkey
  ()
=
  launch_streams_poller polling_interval
                        streams_filename
                        (ustream_update_live ~devkey)

let hitbox_update_live streams_state =
  Log.Global.debug "Polling hitbox";
  Hitbox_api.get_live_streams streams_state.all >>= fun live_streams ->
  begin match live_streams with
    | Result.Error error ->
        Log.Global.error "Hitbox: %s" (Error.to_string_hum error);
        Log.Global.flushed ()
    | Result.Ok live ->
        let recent_live = lset_difference live streams_state.live in
          Log.Global.debug "Hitbox: Current live streams: %s" 
              (Sexp.to_string 
                  (List.sexp_of_t String.sexp_of_t live));
          Log.Global.debug "Hitbox: Recent live streams: %s"
              (Sexp.to_string 
                  (List.sexp_of_t String.sexp_of_t recent_live));
          streams_state.live <- live;
          (* Don't send a recent live list on the first successful
           * poll to avoid spamming the IRC channel when first
           * connecting to the server and building the initial
           * live stream list.
           *)
          begin match (streams_state.last_poll_success, recent_live) with
            | None, _ | Some _, []-> ()
            | Some _, recent_live -> 
                Pipe.write_without_pushback streams_state.recent_live_writer
                                            recent_live
          end;
          streams_state.last_poll_success <- Some (Time.now());
          Deferred.unit
  end

let launch_hitbox_poller 
  ?(polling_interval=Time.Span.of_min 5.0)
  ~streams_filename
  ()
=
  launch_streams_poller polling_interval streams_filename hitbox_update_live

let get_twitch_query = function
  | None -> Deferred.Or_error.return []
  | Some streams_filename ->
      Deferred.Or_error.bind
        (launch_twitch_poller ~streams_filename ())
        (fun query -> Deferred.Or_error.return ["Twitch", query])

let get_hitbox_query = function
  | None -> Deferred.Or_error.return []
  | Some streams_filename ->
      Deferred.Or_error.bind
        (launch_hitbox_poller ~streams_filename ())
        (fun query -> Deferred.Or_error.return ["Hitbox", query])

let get_ustream_query devkey filename =
    match (filename, devkey) with
      | (Some streams_filename, Some devkey) ->
          Deferred.Or_error.bind
            (launch_ustream_poller ~devkey ~streams_filename ())
            (fun query -> Deferred.Or_error.return ["Ustream", query])
      | _ -> Deferred.Or_error.return []

let launch_stream_pollers
  ?twitch_streams_file
  ?ustream_streams_file
  ?ustream_devkey
  ?hitbox_streams_file
  ()
=
  let (>>=) = Deferred.Or_error.Monad_infix.(>>=) in
  get_twitch_query twitch_streams_file >>= fun twitch_query ->
  get_hitbox_query hitbox_streams_file >>= fun hitbox_query ->
  get_ustream_query ustream_devkey ustream_streams_file >>= fun ustream_query ->
  let queries = List.join [twitch_query; hitbox_query; ustream_query] in
  let call_get_live (_, (get_live, _, _, _)) = get_live() in
  let call_get_all (site_name, (_, _, get_all, _)) = (site_name, get_all()) in
  let call_get_last_success (site_name, (_, _, _, get_last_success)) = 
    (site_name, get_last_success())
  in
  let get_recent (_, (_, get_recent, _, _)) = get_recent in
  let get_live_streams () = List.join (List.map ~f:call_get_live queries) in
  let get_recent_live_streams = List.map ~f:get_recent queries in
  let get_all_streams () = List.map ~f:call_get_all queries in
  let get_last_poll_success () = List.map ~f:call_get_last_success queries in
  Deferred.Or_error.return
    {
      get_live_streams;
      get_recent_live_streams;
      get_all_streams;
      get_last_poll_success;
    }
