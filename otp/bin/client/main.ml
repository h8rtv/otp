open Render
open User
open Sql

let first_execution () =
  let ( let* ) = Result.bind in

  let db = open_db () in
  let* () = create_database db in
  let user = read_full_user () in
  let* () = initial_persist db user in
  let () = password_render user.root_password in
  Result.ok ()

let later_execution () =
  let ( let* ) = Result.bind in

  let db = open_db () in
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

let () =
  let res =
    match Sys.file_exists "client.db" with
    | false -> first_execution ()
    | true -> later_execution ()
  in
  match res with Ok _ -> () | Error err -> prerr_endline err
