
let make_element name v =
  Ocsigen_extensions.Configuration.element
    ~name ?pcdata:(Some (fun s -> v := s)) ()

let logdir_ref = ref ""
let logdir_elt = make_element "logdir" logdir_ref

let datadir_ref = ref ""
let datadir_elt = make_element "datadir" datadir_ref

let config = Eliom_config.parse_config [logdir_elt; datadir_elt]

let logdir = !logdir_ref ^ "/"
let datadir = !datadir_ref ^ "/"

let get_str_localdate () =
  let tm = Unix.localtime (Unix.time ()) in
  let to_str = string_of_int in
  (to_str tm.Unix.tm_mday) ^ "/" ^ (to_str tm.Unix.tm_mon) ^ "/" ^
  (to_str (tm.Unix.tm_year + 1900)) ^ "_" ^ (to_str tm.Unix.tm_hour) ^ "h" ^
  (to_str tm.Unix.tm_min) ^ "m" ^ (to_str tm.Unix.tm_sec) ^ "s"

let get_date_value str_date =
  try
    let to_int = int_of_string in
    let ltime = Str.split (Str.regexp "[_/hms]") str_date in
    let mday = to_int (List.nth ltime 0) in
    let mon = to_int (List.nth ltime 1) in
    let year = to_int (List.nth ltime 2) in
    let hour = to_int (List.nth ltime 3) in
    let min = to_int (List.nth ltime 4) in
    let sec = to_int (List.nth ltime 5) in
    mday, mon, year, hour, min, sec
  with e        -> failwith "Invalide format"
