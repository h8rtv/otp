open Printf
open Sqlite3

let read_field field_name =
  printf "%s: " field_name;
  read_line ()

let read_hidden field_name =
  let open Unix in
  let term_attrs = tcgetattr stdin in
  let no_echo_term_attrs = { term_attrs with c_echo = false } in
  tcsetattr stdin TCSANOW no_echo_term_attrs;
  Fun.protect
    (fun () -> read_field field_name)
    ~finally:(fun () ->
      tcsetattr stdin TCSANOW term_attrs;
      print_newline ())

let hash str = str |> Sha512.string |> Sha512.to_hex
let bytestring hex = `Hex hex |> Hex.to_string
let hash_of_hex hex = hex |> bytestring |> hash

let human_readable n hash =
  let encoded = Base32.encode_string @@ bytestring hash in
  String.sub encoded 0 n

let lambort seed n =
  let rec lamport_rec l hash n =
    match n with
    | 0 -> l
    | _ ->
        let new_hash = hash_of_hex hash in
        let new_l = new_hash :: l in
        lamport_rec new_l new_hash (n - 1)
  in
  lamport_rec [] seed n

type user = { username : string; password : string; root_password : string }

let read_local_user () =
  let username = read_field "Username"
  and password = hash @@ read_hidden "Password" in
  (username, password)

let read_full_user () =
  let username, password = read_local_user ()
  and root_password = hash @@ read_hidden "Root password"
  and salt = hash @@ read_field "Salt" in
  let root_password_with_salt =
    hash @@ bytestring salt ^ bytestring root_password
  in
  { username; password; root_password = root_password_with_salt }

let query db sql =
  let res = exec db sql ~cb:(fun _ _ -> ()) in
  match res with
  | Rc.OK -> Result.ok ()
  | r ->
      let err = sprintf "[%s] %s" (Rc.to_string r) (errmsg db) in
      Result.error err

let fetch_user db username cb =
  let sql = sprintf "SELECT * FROM data WHERE username = '%s'" username in
  let res =
    exec db sql ~cb:(fun row _ ->
        match (row.(0), row.(1), row.(2)) with
        | Some username, Some password, Some root_password ->
            cb { username; password; root_password }
        | _ -> ())
  in
  match res with
  | Rc.OK -> Result.ok ()
  | r ->
      let err = sprintf "[%s] %s" (Rc.to_string r) (errmsg db) in
      Result.error err

let create_database db =
  let sql =
    "CREATE TABLE data (username varchar(24), password char(128), \
     root_password char(128))"
  in
  query db sql

let initial_persist db user =
  let sql =
    sprintf "INSERT INTO data VALUES ('%s', '%s', '%s')" user.username
      user.password user.root_password
  in
  query db sql

let password_generator seed d =
  let date_str =
    let open CalendarLib.Calendar in
    sprintf "%d%d%d%d" (year d) (day_of_year d) (hour d) (minute d)
  in
  let time_seed = hash @@ bytestring seed ^ bytestring date_str in
  lambort time_seed 10

let minute_bar () =
  let bar total =
    let open Progress.Line in
    list [ spinner (); bar ~style:`UTF8 total; count_to total ]
  in
  Progress.with_reporter (bar 60) (fun f ->
      let open CalendarLib.Calendar in
      let s = ref @@ second (now ()) in
      let prev = ref @@ !s in
      f !s;
      if !s == 0 then Unix.sleepf 1.;
      while !s != 0 do
        s := second (now ());
        f @@ (!s - !prev);
        prev := !s;
        Unix.sleepf 0.03
      done)

let password_render seed =
  while true do
    let _ = Unix.system "clear" in
    let d = CalendarLib.Calendar.now () in
    let hash_list = password_generator seed d in
    let print_human hash = print_endline @@ human_readable 8 hash in
    let () = List.iter print_human hash_list in
    minute_bar ()
  done

let first_execution () =
  let ( let* ) = Result.bind in

  let db = db_open "client.db" in
  let* () = create_database db in
  let user = read_full_user () in
  let* () = initial_persist db user in
  let () = password_render user.root_password in
  Result.ok ()

let later_execution () =
  let ( let* ) = Result.bind in

  let db = db_open "client.db" in
  let username, password = read_local_user () in
  let* () =
    fetch_user db username (fun user ->
        if String.compare password user.password == 0 then
          password_render user.root_password
        else prerr_endline "Unauthorized")
  in
  Result.ok ()

let () =
  let res =
    match Sys.file_exists "client.db" with
    | false -> first_execution ()
    | true -> later_execution ()
  in
  match res with Ok _ -> () | Error err -> prerr_endline err
