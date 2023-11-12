open Render
open User
open Sql

let ( let* ) = Result.bind

let login db =
  let username, password = read_local_user () in
  let* user_opt = fetch_user db username in
  let () =
    match user_opt with
    | Some user ->
        if String.compare password user.password == 0 then
          password_render user.root_password
        else prerr_endline "Auth failed. Wrong password."
    | None -> prerr_endline "Auth failed. User not found."
  in
  Result.ok ()

let create_user db =
  let user = read_full_user () in
  let* () = persist_user db user in
  Result.ok user

let rec menu db =
  print_endline "1 - Login";
  print_endline "2 - Create user";
  print_endline "3 - Exit";
  match read_int_opt () with
  | Some 1 ->
      let* () = login db in
      menu db
  | Some 2 ->
      let* _ = create_user db in
      menu db
  | Some 3 -> Result.ok ()
  | _ -> menu db

let first_execution () =
  let db = open_db () in
  let* () = create_database db in
  let* user = create_user db in
  let () = password_render user.root_password in
  Result.ok ()

let later_execution () =
  let db = open_db () in
  let* () = menu db in
  Result.ok ()

let () =
  let res =
    match Sys.file_exists "client.db" with
    | false -> first_execution ()
    | true -> later_execution ()
  in
  match res with Ok _ -> () | Error err -> prerr_endline err
