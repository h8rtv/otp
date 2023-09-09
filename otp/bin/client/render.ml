open Otp.Crypto

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
