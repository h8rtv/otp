open Printf

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
