(* Run with: ocamlfind ocamlc -package lwt -package lwt.unix -linkpkg -o server str.cma funSrv.ml funSrvMain.ml *)

(* COMMENT: Comments in this section mainly describe modification to the original chatServer. For describtion of the original functions, refer to charServer and hw6 description. *)

open Lwt.Infix

(* COMMENT: sessions has been modified to be a (name,(inp,outp)) list. This modification was essention for implementing kick function. *)
let sessions = ref []
             
exception Quit

(* COMMENT: A referrence list which stores names of all the admins. The first admin is the one who entered the chat first. If all the admins are deleted, the next person to enter the chat becomes an admin *)
let admins = ref []         

(* COMMENT: send_all has been modified to check for flag and if the flag is active, it will print a colored message of a form < ... >. Otherwise, prints a regular-colored message for every user. *)
let rec send_all sender msg flag =
  Lwt_list.iter_p (fun (x,y) ->
      if flag then Lwt_io.fprintl (snd y) (sender ^ msg ^ ">\x1b[0m")
      else if x = sender then Lwt.return ()
       else
        Lwt_io.fprintl (snd y) (sender ^ ": " ^ msg ^ "\x1b[0m")) !sessions

let remove_session nn =
  sessions := List.remove_assoc nn !sessions;
  send_all ("\x1b[35m <" ^ nn) " has left the chat" true >>= fun () ->
  Lwt.return ()

let handle_error e nn inp outp = match e with
  | Quit -> Lwt_io.close inp; Lwt_io.close outp; remove_session nn
  | _ -> remove_session nn

(* COMMENT: Modified function to function properly with admins ref list *)
let change_nn nn outp new_nn =
  (let sec = Lwt_mutex.create () in
  send_all ("\x1b[36m <" ^ !nn) (" changed nick to " ^ new_nn) true;
  Lwt_mutex.lock sec;
  sessions := List.map (fun (x,y) -> if x = !nn then (new_nn,y) else (x,y)) !sessions;
  admins := List.map (fun x -> if x = !nn then new_nn else x) !admins;
  nn := new_nn;
  Lwt_mutex.unlock sec)

(* COMMENT: Allows a user to send private messages to another user. The implementation is inefficient, but making it more efficient would require to redefine whole handle_input function. *)
let private_message sdr outp msg = Lwt_io.fprintl outp ("\x1b[34m <Private> \x1b[0m" ^ sdr ^ ": " ^ msg)

let info = "\n Welcome to the chat!
 For different command options, type /h \n"

(* COMMENT: Just a string with all the commands *)
let help = "
Default level commans: 
/l - list all active users 
/n - change current name 
/q - quit the chat 
/p - write a private message 
Expected input for private messages: /p username | message 
(Example: /p Bob | what's up? )

Admin-level Commands: 
/a - add a new member of administration 
/d - delete user from administration list 
/k - kick user from the chat
For every admin-level command, expected input has a form /x username
"         

(* COMMENT: Now a member of administration can kick a person from the chat. Works in the same way as /q, except, it is the kicked user's inp and outp being closed. This function is the main reason why I modified the sessions. *)
let kick nm = Lwt_list.iter_p (fun (x,(y,z)) -> if x = nm then (Lwt_io.close y; Lwt_io.close z) else Lwt.return ()) !sessions;
              send_all ("\x1b[33m <" ^ nm) " got kicked" true >>= fun () ->
  Lwt.return ()

(* COMMENT: Adds a user to admin list. *)
let add_admin nm = admins := nm::(!admins);
                 send_all ("\x1b[36m <" ^ nm) " was added to administration" true >>= fun () ->
  Lwt.return ()

(* COMMENT: Deletes a user from admin list. *)
let delete_admin nm = admins := (List.filter (fun x -> x != nm) (!admins));
                      send_all ( "\x1b[33m <" ^ nm) " was removed from administration" true >>= fun () ->
  Lwt.return ()

(*COMMENT: Added additional restriction to the name requirements. Now, it's impossible for two people to have the same name and to have an empty space or white space name. *)
let rec handle_login nr (inp,outp) =
  let mute = Lwt_mutex.create () in
  Lwt_io.fprint outp "Enter initial nick: "
  >>= (fun () -> (Lwt_io.read_line inp))
  >>= fun l ->
  if List.exists (fun (x,y) -> x = l) !sessions then Lwt_io.fprintl outp "\x1b[31m The name has already been taken \x1b[0m \n" >>= (fun () -> handle_login nr (inp,outp))
  else if String.length (String.trim l) < 1 then Lwt_io.fprintl outp "\x1b[31m Name must be at least one character long \x1b[0m \n" >>= (fun () -> handle_login nr (inp,outp)) else 
    (Lwt.return (nr := l);
    Lwt_mutex.lock mute;
    (if !sessions = [] then admins := !nr::(!admins) else admins := !admins);
    (sessions := (!nr,(inp,outp))::(!sessions));
    Lwt_mutex.unlock mute;
    (Lwt_io.fprintl outp info);
    send_all ("\x1b[32m <" ^ !nr) " has joined the chat" true)

(* COMMENT: Looks quite intimidating, isn't it? It now handles additional commands. *)
let handle_input nr inp outp l = match (Str.string_before l 2) with
  | "/q" -> Lwt.fail Quit
  | "/l" -> Lwt_list.iter_s (fun (x,y) -> Lwt_io.fprintl outp x) !sessions;
  | "/h" -> (Lwt_io.fprintl outp help)
  (* COMMENT: Took me a lot of time to implement this one. Takes the string before |, trims it, looks for name in sessions list. If it was able to find it, trims string after | and sends it as a private message to the user mentioned before |. *)
  | "/p" ->
     if (String.length l < 3) then Lwt_io.fprintl outp "Name is not specified"
     else if (String.contains l '|') = false then Lwt_io.fprintl outp "Wrong format. For help type /h."
       else
     let i = (String.trim (Str.string_after l 2)) in
            let p = (String.trim (Str.string_before i (String.index i '|'))) in
             if List.exists (fun (x,y) -> x = p) !sessions then
              (let s = (String.trim (Str.string_after i ((String.index i '|') + 1))) in
               private_message !nr (snd (List.assoc p !sessions)) s) else Lwt_io.fprintl outp "\x1b[31m Invalid username \x1b[0m"
  (* COMMENT: Checks if the user exists and then closes user's input and output channels. *)
  | "/k" -> if List.mem !nr !admins then
              (let m = (String.trim (Str.string_after l 2)) in if List.exists (fun (x,y) -> x = m) !sessions then kick m else Lwt_io.fprintl outp "\x1b[31m Invalid name \x1b[0m")
            else Lwt_io.fprintl outp "\x1b[31m Permission Denied \x1b[0m"
  (* COMMENT: Implementation of add and delete are straightforward. Add or delete a member of administration. *)
  | "/a" -> if List.mem !nr !admins then
              (let k = (String.trim (Str.string_after l 2)) in if List.exists (fun (x,y) -> x = k) !sessions then add_admin k else Lwt_io.fprintl outp "\x1b[31m Invalid name \x1b[0m")
            else Lwt_io.fprintl outp "\x1b[31m Permission Denied \x1b[0m"
  | "/d" -> if List.mem !nr !admins then
              (let d = (String.trim (Str.string_after l 2)) in if List.exists (fun (x,y) -> x = d) !sessions then delete_admin d else Lwt_io.fprintl outp "\x1b[31m Invalid name \x1b[0m")
            else Lwt_io.fprintl outp "\x1b[31m Permission Denied \x1b[0m"
  (* Now checks if the name is an empty space or already taken. *)
  | "/n" -> let n = (String.trim (Str.string_after l 2)) in
            if String.length n < 1 then Lwt_io.fprintl outp "\x1b[31m Invalid name \x1b[0m"
            else if List.exists (fun (x,y) -> x = n) !sessions then Lwt_io.fprintl outp "\x1b[31m The name is already taken \x1b[0m"
            else Lwt.return (change_nn nr outp n)
  | _ -> send_all !nr l false

let chat_server _ (inp,outp) =
  let nick = ref "" in
  let login = handle_login nick (inp,outp) in
  let rec main_loop () =
	  Lwt_io.read_line inp >>= handle_input nick inp outp >>= main_loop in
  Lwt.catch (fun () -> login >>= main_loop) (fun e -> handle_error e !nick inp outp)
