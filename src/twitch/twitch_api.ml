open Core.Std
open Async.Std

let live_stream_poll_uri stream_name_list =
  let base_uri = Uri.of_string "https://api.twitch.tv/kraken/streams" in
  let chans = String.concat ~sep:"," stream_name_list in
  Uri.add_query_param base_uri ("channel", [chans])

let get_live_streams_from_json json =
  let open Or_error.Monad_infix in
  let find key lst =
    begin match lst with
    | `Assoc lst -> 
       Option.value_map (List.Assoc.find lst key)
          ~default:(Or_error.errorf "Key not found: %s" key)
          ~f:(fun v -> Or_error.return v)
    | _ -> Or_error.error_string "JSON object expected, but not found"
    end
  in
  let rec map' f out = function
    | [] -> Or_error.return (List.rev out)
    | hd :: tl -> f hd >>= fun out_hd -> map' f (out_hd :: out) tl
  in
  let map lst f =
    begin match lst with
    | `List lst -> map' f [] lst
    | _ -> Or_error.error_string "JSON array expected, but not found"
    end
  in
  let get_channel_url stream_info =
    find "channel" stream_info >>= fun channel_info ->
    find "url" channel_info >>= fun url ->
    match url with
      | `String s -> Or_error.return s
      | _ -> Or_error.error_string
              "Channel URL (JSON string) expected, but not found"
  in
    find "streams" (Yojson.Safe.from_string json) >>= fun streams_list ->
    map streams_list get_channel_url

let get_live_streams = function
  | [] -> 
    Log.Global.debug "Twitch streams input list is empty";
    Deferred.Or_error.return []
  | streams_list -> 
      Cohttp_async.Client.get (live_stream_poll_uri streams_list)
      >>= fun (_, body) ->
        (Cohttp_async.Body.to_string body)
      >>| fun json_string ->
        (get_live_streams_from_json json_string)

let get_live_streams streams_list =
  Deferred.Or_error.try_with_join (fun () -> get_live_streams streams_list)
