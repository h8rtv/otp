open Otp.Io
open Otp.Crypto

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
