open Printf
open Sql
open User
open Otp.Crypto

let ( let* ) = Result.bind

let find_index lst target =
  let rec find_index_helper lst target index =
    match lst with
    | [] -> None
    | hd :: tl ->
        if hd = target then Some index
        else find_index_helper tl target (index + 1)
  in
  find_index_helper lst target 0

let get_password_list d user =
  let seed =
    hash @@ bytestring (hash user.salt) ^ bytestring user.root_password
  in
  let hash_list = password_generator seed d in
  let list = List.map (human_readable 8) hash_list in
  list

let auth db user index date_str =
  let* () = update_password_data db user index date_str in
  let () = print_endline "Access granted." in
  Result.ok ()

let check_auth db user index date =
  let date_str = date_str date in
  match (user.last_used, user.date) with
  | Some last_used, Some usr_date ->
      let already_used = last_used >= index in
      let* () =
        if date_str = usr_date && already_used then
          let () = print_endline "Auth failed. Password already used." in
          Result.ok ()
        else auth db user index date_str
      in
      Result.ok ()
  | _ -> auth db user index date_str

let handle_user_found db input_password user =
  let date = CalendarLib.Calendar.now () in
  let password_list = get_password_list date user in
  let password_opt = find_index password_list input_password in
  match password_opt with
  | Some index -> check_auth db user index date
  | None ->
      let () = print_endline "Auth failed. Wrong password." in
      Result.ok ()

let login db =
  let username, input_password = read_remote_user () in
  let* user_opt = fetch_user db username in
  let* () =
    match user_opt with
    | Some user -> handle_user_found db input_password user
    | None ->
        let () = print_endline "Auth failed. User not found." in
        Result.ok ()
  in
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
