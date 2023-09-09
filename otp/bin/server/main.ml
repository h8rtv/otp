open Printf
open Sql
open User
open Otp.Crypto

let ( let* ) = Result.bind

let handle_user_found user = 
  let d = CalendarLib.Calendar.now () in
  let seed = hash @@ bytestring (hash user.salt) ^ bytestring user.root_password in
  let hash_list = password_generator seed d in
  let print_human hash = print_endline @@ human_readable 8 hash in
  let () = List.iter print_human hash_list in ()

let login db =
  let username, _ = read_remote_user () in
  let* () = fetch_user db username handle_user_found in
  Result.ok ()

let create_user db =
  let user = read_insert_user () in
  let* () = initial_persist db user in
  let () = print_endline "Success!" in
  let () = printf "Generated salt: %s\n\n" user.salt in
  Result.ok ()

let rec menu db =
  print_endline "1 - Login";
  print_endline "2 - Create user";
  print_endline "3 - Exit";
  match read_int_opt () with
  | Some 1 ->
      let* () = login db in
      menu db
  | Some 2 ->
      let* () = create_user db in
      menu db
  | Some 3 -> Result.ok ()
  | _ -> menu db

let () =
  let res =
    let db_exists = Sys.file_exists "server.db" in
    let db = open_db () in
    let* () =
      match db_exists with false -> create_database db | true -> Result.ok ()
    in
    menu db
  in
  match res with Ok _ -> () | Error err -> prerr_endline err
