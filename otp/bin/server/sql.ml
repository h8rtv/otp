open Sqlite3
open Printf
open User

let open_db () = db_open "server.db"

let query db sql =
  let res = exec db sql ~cb:(fun _ _ -> ()) in
  match res with
  | Rc.OK -> Result.ok ()
  | r ->
      let err = sprintf "[%s] %s" (Rc.to_string r) (errmsg db) in
      Result.error err

let fetch_user db username cb =
  let sql = sprintf "SELECT * FROM users WHERE username = '%s'" username in
  let res =
    exec db sql ~cb:(fun row _ ->
        match (row.(0), row.(1), row.(2)) with
        | Some username, Some root_password, Some salt ->
            cb { username; root_password; salt }
        | _ -> ())
  in
  match res with
  | Rc.OK -> Result.ok ()
  | r ->
      let err = sprintf "[%s] %s" (Rc.to_string r) (errmsg db) in
      Result.error err

let create_database db =
  let sql =
    "CREATE TABLE users (username varchar(24) UNIQUE, root_password char(128), salt \
     char(128))"
  in
  query db sql

let initial_persist db user =
  let sql =
    sprintf "INSERT INTO users VALUES ('%s', '%s', '%s')" user.username
      user.root_password user.salt
  in
  query db sql
