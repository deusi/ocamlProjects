
(* COMMENT: I will provide some basic commentary in here, but most of the comments will be in funSrv part, since funSrv has the same core functionality.
Please note that it's a raw version of the chat which is only supposed to satisfy the project requirements. For fancy chat, take a look at funSrv
 *)

open Lwt.Infix

(* a list associating user nicknames to the output channels that write to their connections *)
(* Once we fix the other functions this should change to []*)

(* COMMENT: Sessions contains a list with user-output tuple. Mainly used to print messages on user's screen. *)
   
let sessions = ref []
exception Quit

(* replace Lwt.return below with code that uses Lwt_list.iter_p to
  print sender: msg on each output channel (excluding the sender's)*)

(* COMMENT: Sends messages to every user except the sender by using sessions. Not much to add here *)
        
let rec send_all sender msg =
  Lwt_list.iter_p (fun (x,y) -> if x = sender then Lwt.return () else Lwt_io.fprintl y (sender ^ ": " ^ msg)) !sessions

(* remove a session from the list of sessions: important so we don't try to
   write to a closed connection *)
let remove_session nn =
  sessions := List.remove_assoc nn !sessions;
  send_all nn "<left chat>" >>= fun () ->
  Lwt.return ()

(* Modify to handle the "Quit" case separately, closing the channels before removing the session *)
let handle_error e nn inp outp = match e with
  | Quit -> Lwt_io.close inp; Lwt_io.close outp; remove_session nn
  | _ -> remove_session nn

(* modify sessions to remove (nn,outp) association, add (new_nn,outp) association.
   also notify other participants of new nickname *)
let change_nn nn outp new_nn =
  let sec = Lwt_mutex.create () in
  send_all !nn ("<changed nick to " ^ new_nn ^ ">");
  Lwt_mutex.lock sec;
  sessions := List.map (fun (x,y) -> if x = !nn then (new_nn,y) else (x,y)) !sessions;
  nn := new_nn;
  Lwt_mutex.unlock sec
  
(*  + obtain initial nick(name),
    + add (nick,outp) to !sessions, and
    + announce join to other users *)

(* COMMENT: Binds user's output to the nickname. At this version, has no additional specifications, so the nickname could be anything. *)
                    
let handle_login nr (inp,outp) =
  let mute = Lwt_mutex.create () in
  (Lwt_io.fprintl outp "Enter initial nick:")
  >>= (fun () -> (Lwt_io.read_line inp))
  >>= (fun l ->
    (nr := l);
    Lwt_mutex.lock mute;
    (sessions := (!nr,outp)::(!sessions));
    Lwt_mutex.unlock mute; send_all !nr "<joined>")

(* COMMENT: This part handles all the input, including self-explanatory cases below. *)

(* modify handle_input below to detect /q, /n, and /l commands *)
let handle_input nr outp l = match (Str.string_before l 2) with
  | "/q" -> Lwt.fail Quit
  | "/l" -> Lwt_list.iter_s (fun (x,y) -> Lwt_io.fprintl outp x) !sessions;
  | "/n" -> Lwt.return (change_nn nr outp (String.trim (Str.string_after l 2)))
  | _ -> send_all !nr l

(* COMMENT: The main loop which handles all the input from the users. Connected it to login part, so users would have to pick a name before entering the chat. *)

let chat_server _ (inp,outp) =
  let nick = ref "" in
  (* replace () below with call to handle_login *)
  let login = handle_login nick (inp,outp) in
  let rec main_loop () =
	  Lwt_io.read_line inp >>= handle_input nick outp >>= main_loop in
  Lwt.catch (fun () -> login >>= main_loop) (fun e -> handle_error e !nick inp outp)
