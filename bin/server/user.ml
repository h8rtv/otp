open Otp.Io
open Otp.Crypto

type user = {
  username : string;
  root_password : string;
  salt : string;
  last_used : int option;
  date : string option;
}

let to_hex (n : int64) : string = Printf.sprintf "%016Lx" n

let gen_salt () =
  let () = Random.self_init () in
  let rand = Random.int64 Int64.max_int in
  rand |> to_hex

let read_insert_user () =
  let username = read_field "Username"
  and root_password = hash @@ read_hidden "Root password"
  and salt = gen_salt () in
  { username; root_password; salt; last_used = None; date = None }

let read_remote_user () =
  let username = read_field "Username" and password = read_hidden "Password" in
  (username, password)
