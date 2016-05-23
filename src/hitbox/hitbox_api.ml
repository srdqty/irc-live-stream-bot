open Core.Std
open Async.Std

let live_stream_poll_uri stream_name_list =
  let prefix = "https://api.hitbox.tv/media/live/" in
  let chans = String.concat ~sep:"," stream_name_list in
  Uri.of_string (prefix ^ chans)

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
    | hd :: tl -> 
        f hd >>= fun out_hd -> 
        begin match out_hd with
          | None -> map' f out tl
          | Some chan -> map' f (chan :: out) tl
        end
  in
  let map lst f =
    begin match lst with
    | `List lst -> map' f [] lst
    | _ -> Or_error.error_string "JSON array expected, but not found"
    end
  in
  let get_channel_url stream_info =
    find "media_is_live" stream_info >>= fun live_status ->
    begin match live_status with
      | `String "0" -> Or_error.return None
      | `String "1" ->
          find "channel" stream_info >>= fun channel_info ->
          find "channel_link" channel_info >>= fun url ->
          begin match url with
            | `String s -> Or_error.return (Some s)
            | _ -> Or_error.error_string
                    "Channel URL (JSON string) expected, but not found"
          end

      | _ -> 
          Or_error.error_string 
            "Media_is_live (JSON string \"0\" or \"1\") expected, but not found"
    end
  in
    find "livestream" (Yojson.Safe.from_string json) >>= fun streams_list ->
    map streams_list get_channel_url

let get_live_streams = function
  | [] -> 
    Log.Global.debug "Hitbox streams input list is empty";
    Deferred.Or_error.return []
  | streams_list -> 
      Cohttp_async.Client.get (live_stream_poll_uri streams_list)
      >>= fun (_, body) ->
        (Cohttp_async.Body.to_string body)
      >>| fun json_string ->
        (get_live_streams_from_json json_string)

let get_live_streams streams_list =
  Deferred.Or_error.try_with_join (fun () -> get_live_streams streams_list)
