let read_field field_name =
  Printf.printf "%s: " field_name;
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

(* TODO: save as binary on a database *)
let hash str = Base32.encode_string @@ Sha512.to_hex @@ Sha512.string str

let lambort seed n =
  let rec lamport_rec l seed n =
    match n with
    | 0 -> l
    | _ ->
        let new_seed = hash seed in
        let new_l = new_seed :: l in
        lamport_rec new_l new_seed (n - 1)
  in
  lamport_rec [] seed n

let () =
  let _username = read_field "Username"
  and _password = hash @@ read_hidden "Password"
  and root_password = hash @@ read_hidden "Root password"
  and _salt = hash @@ read_field "Salt" in

  let hash_list = lambort root_password 10 in
  print_endline root_password;
  print_newline ();
  List.iter (Printf.printf "%s\n") hash_list
