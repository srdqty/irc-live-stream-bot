open Core.Std
open Async.Std
module IRC = Easy_irc_client

let chunk_string_list ?(sep=" ") lst length =
  let sep_len = String.length sep in
  let buffer = Buffer.create length in
  let append_string str =
    if (Buffer.length buffer) = 0
    then Buffer.add_string buffer str
    else Buffer.add_string buffer sep; Buffer.add_string buffer str in
  let rec loop out inlst =
    begin match inlst with
      | [] -> List.rev ((Buffer.contents buffer) :: out)
      | str :: rest ->
          let strlen = String.length str in
            if strlen > length 
            then loop out rest 
            else if (Buffer.length buffer) + sep_len + strlen > length
              then 
                let string = Buffer.contents buffer in
                  Buffer.clear buffer;
                  Buffer.add_string buffer str;
                  loop (string :: out) rest
            else
              (append_string str; loop out rest)
    end
  in
    begin match lst with
      | [] -> []
      | lst -> loop [] lst
    end

let rec send_privmsg_list writer target = function
  | [] -> Result.ok_unit
  | hd :: tl ->
      Or_error.bind
        (IRC.send_privmsg ~target ~message:hd writer)
        (fun () -> send_privmsg_list writer target tl)

let send_live_streams writer target streams_list =
   send_privmsg_list writer target
    (List.map streams_list
      ~f:(fun stream -> stream ^ " is live"))



let send_all_streams_helper writer target (site_name, streams_list) =
  let message = sprintf "******** %s channels ********" site_name in
    Or_error.bind
      (IRC.send_privmsg writer ~target ~message)
      (fun () -> send_privmsg_list writer target 
                    (chunk_string_list streams_list 80))
let rec send_all_streams writer target = function
  | [] -> Result.ok_unit
  | hd :: tl ->
      Or_error.bind
        (send_all_streams_helper writer target hd)
        (fun () -> send_all_streams writer target tl)

let send_polling_status_helper writer target = function
  | (site_name, None) -> 
      let message = sprintf "%s last successful poll: never" site_name in
        IRC.send_privmsg writer ~target ~message
  | (site_name, Some time) -> 
      let min_ago = Time.diff (Time.now()) time
                    |> Time.Span.to_min
                    |> Float.to_int
      in
      let message = 
          sprintf "%s last successfull poll: %d %s ago" 
                  site_name
                  min_ago
                  (if min_ago = 1 then "minute" else "minutes" )
      in
        IRC.send_privmsg writer ~target ~message
let rec send_polling_status writer target = function
  | [] -> Result.ok_unit
  | hd :: tl ->
      Or_error.bind
        (send_polling_status_helper writer target hd)
        (fun () -> send_polling_status writer target tl)

let prefix_to_nick prefix =
  begin match prefix with
    | None -> Or_error.error_string "No prefix for received PRIVMSG"
    | Some prefix ->
        begin match String.index prefix '!' with
          | None -> Or_error.error_string 
                      "Cannot extract nickname from prefix"
          | Some i -> Or_error.return (String.slice prefix 0 i)
        end
  end

let process_message
  ~get_live_streams
  ~get_all_streams
  ~get_last_poll_success
  writer
  {IRC.prefix; IRC.command; IRC.arguments}
=
  let (>>=) = Or_error.Monad_infix.(>>=) in
  Deferred.return
    begin match command with
      | "PRIVMSG" ->
          Log.Global.debug 
              "Received PRIVMSG(prefix: %S, command: %S, message: %S"
                  (Option.value ~default:"" prefix)
                  command
                  (Option.value ~default:"" (List.last arguments));
          begin match List.last arguments with
            | (Some "gimme live streams") ->
                prefix_to_nick prefix >>= fun nickname ->
                send_live_streams writer nickname (get_live_streams())
            | (Some "gimme all streams") ->
                prefix_to_nick prefix >>= fun nickname ->
                send_all_streams writer nickname (get_all_streams())
            | (Some "gimme polling status") ->
                prefix_to_nick prefix >>= fun nickname ->
                send_polling_status writer nickname (get_last_poll_success())
            | Some _ -> Result.ok_unit
            | None -> Result.ok_unit
          end
      | _ -> Result.ok_unit
    end

let launch_recent_live_notifier
  ~get_recent_live_streams
  ~channel
  irc_writer
=
  let rec loop () =
    get_recent_live_streams () >>= fun recent_live_streams ->
    Log.Global.debug "Got more recent live streams: %s"
        (Sexp.to_string (List.sexp_of_t String.sexp_of_t recent_live_streams));
    Deferred.Or_error.bind 
      (Deferred.return 
        (send_live_streams irc_writer channel recent_live_streams))
      loop
  in
  let wrapped_loop =
    Deferred.Or_error.try_with_join loop >>= fun or_error ->
    begin match or_error with
      | Ok () -> Deferred.unit
      | Error error ->
          Log.Global.error "%s" (Error.to_string_hum error);
          Log.Global.error "recent_live_streams loop exiting";
          Log.Global.flushed ()
    end
  in
    Deferred.don't_wait_for wrapped_loop
