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

let fetch_user db username =
  let sql = sprintf "SELECT * FROM users WHERE username = '%s'" username in
  let user = ref None in
  let res =
    exec db sql ~cb:(fun row _ ->
        match (row.(0), row.(1), row.(2), row.(3), row.(4)) with
        | Some username, Some root_password, Some salt, None, None ->
            user :=
              Some
                { username; root_password; salt; date = None; last_used = None }
        | ( Some username,
            Some root_password,
            Some salt,
            Some last_used,
            Some date ) ->
            user :=
              Some
                {
                  username;
                  root_password;
                  salt;
                  date = Some date;
                  last_used = Some (int_of_string last_used);
                }
        | _ -> ())
  in
  match res with
  | Rc.OK -> Result.ok !user
  | r ->
      let err = sprintf "[%s] %s" (Rc.to_string r) (errmsg db) in
      Result.error err

let create_database db =
  let sql =
    "CREATE TABLE users (username varchar(24) UNIQUE, root_password char(128), \
     salt varchar(128), last_used integer, date char(11))"
  in
  query db sql

let initial_persist db user =
  let sql =
    sprintf
      "INSERT INTO users (username, root_password, salt) VALUES ('%s', '%s', \
       '%s')"
      user.username user.root_password user.salt
  in
  query db sql

let update_password_data db user last_used date =
  let sql =
    sprintf
      "UPDATE users SET last_used = '%d', date = '%s' WHERE username = '%s'"
      last_used date user.username
  in
  query db sql
