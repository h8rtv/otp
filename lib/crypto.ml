open Printf

let hash str = str |> Sha512.string |> Sha512.to_hex
let bytestring hex = `Hex hex |> Hex.to_string
let hash_of_hex hex = hex |> bytestring |> hash

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

let date_str d =
  let open CalendarLib.Calendar in
  sprintf "%d%d%d%d" (year d) (day_of_year d) (hour d) (minute d)

let password_generator seed d =
  let date_str = date_str d in
  let time_seed = hash @@ bytestring seed ^ date_str in
  lambort time_seed 10

let human_readable n hash =
  let encoded = Base32.encode_string @@ bytestring hash in
  String.sub encoded 0 n
