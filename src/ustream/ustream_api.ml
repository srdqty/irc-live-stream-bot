open Core.Std
open Async.Std

let construct_url ~devkey stream_name_list =
  let prefix = "http://api.ustream.tv/json/channel/" in
  let suffix = "/getValueOf/status?key=" ^ devkey in
  let channels = String.concat ~sep:";" stream_name_list in
  Uri.of_string (prefix ^ channels ^ suffix)

let split_up_list lst =
  let rec loop lst out short_list size =
    begin match lst with
      | [] ->
        begin match short_list with
          | [] -> out
          | short_list -> short_list :: out
        end
      | hd :: tl ->
          if size = 10 then (
            loop lst (short_list :: out) [] 0
          ) else (
            loop tl out (hd :: short_list) (size + 1)
          )
    end
  in
    loop lst [] [] 0
(* Pass in the first stream from the list because
 * the ustream api does not return an indication of
 * the stream channel name when a single stream is
 * queried.
 *)
let get_live_streams_from_json json first_stream =
  let (>>=) = Or_error.Monad_infix.(>>=) in
  let find key lst =
    begin match lst with
    | `Assoc lst -> 
       Option.value_map (List.Assoc.find lst key)
          ~default:(Or_error.errorf "Key not found: %s" key)
          ~f:(fun v -> Or_error.return v)
    | _ -> Or_error.error_string "JSON object expected, but not found"
    end
  in
  (* map doesn't need to call reverse now? because of filter
   * reversing list first? *)
  let rec map' f out = function
    | [] -> Or_error.return out
    | hd :: tl -> f hd >>= fun out_hd -> map' f (out_hd :: out) tl
  in
  (* make sure this is correct *)
  let rec filter out = function
    | [] -> Or_error.return out
    | hd :: tl -> 
        find "result" hd >>= fun result ->
        begin match result with
          | `String s when s = "live" -> 
              filter (hd :: out) tl
          | _ -> filter out tl
        end
  in
  let map (lst : Yojson.Safe.json)
          (f : Yojson.Safe.json -> string Or_error.t)
  =
    begin match lst with
      | `List lst -> 
          filter [] lst >>= fun live_streams ->
          map' f [] live_streams
      | `String s when s = "live" -> 
          Or_error.return ["http://www.ustream.tv/channel/" ^ first_stream]
      | `String s when s = "offline" -> 
          Or_error.return []
      | _ -> Or_error.error_string 
                "JSON array or string expected, but not found"
    end
  in
  let get_channel_url (stream_info : Yojson.Safe.json) =
(*    or_error >>= fun stream_info -> *)
    find "uid" stream_info >>= fun uid ->
    begin match uid with
      | `String uid -> Or_error.return ("http://www.ustream.tv/channel/" ^ uid)
      | _ -> Or_error.error_string "JSON string expected, but not found"
    end
  in
    find "results" (Yojson.Safe.from_string json) >>= fun streams_list ->
    map streams_list get_channel_url

let get_live_streams ~devkey first_stream = function
  | [] -> 
    Log.Global.debug "Ustream streams input short list is empty";
    Deferred.Or_error.return []
  | streams_list -> 
      Cohttp_async.Client.get (construct_url ~devkey streams_list)
      >>= fun (_, body) ->
        (Cohttp_async.Body.to_string body)
      >>| fun json_string ->
        (get_live_streams_from_json json_string first_stream)

let check_10_at_a_time ~devkey = function
  | [] -> 
      Log.Global.debug "Ustream streams input list is empty";
      Deferred.Or_error.return []
  | streams_list ->
      let (>>=) = Deferred.Or_error.Monad_infix.(>>=) in
      let rec map out = function
        | [] -> Deferred.Or_error.return out
        | (((caar :: _)  as hd) :: tl)  -> 
            get_live_streams ~devkey caar hd >>= fun live_streams ->
            map (live_streams :: out) tl
        | [] :: tl -> (map out tl)
      in
      split_up_list streams_list |> fun split_list ->
      map [] split_list >>= fun live_streams ->
      Deferred.Or_error.return (List.join live_streams)

let get_live_streams ~devkey streams_list =
  Deferred.Or_error.try_with_join (fun () -> check_10_at_a_time ~devkey streams_list)
