open Otp.Io
open Otp.Crypto

type user = { username : string; root_password : string; salt : string }

let to_hex (n : int64) : string = Printf.sprintf "%016Lx" n

let gen_salt () =
  let () = Random.self_init () in
  let rand = Random.int64 Int64.max_int in
  rand |> to_hex

let read_insert_user () =
  let username = read_field "Username"
  and root_password = hash @@ read_hidden "Root password"
  and salt = gen_salt () in
  { username; root_password; salt }

let read_remote_user () =
  let username = read_field "Username"
  and password = hash @@ read_hidden "Password" in
  (username, password)
