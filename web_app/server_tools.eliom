
let make_element name v =
  Ocsigen_extensions.Configuration.element
    ~name ?pcdata:(Some (fun s -> v := s)) ()

let logdir_ref = ref ""
let logdir_elt = make_element "logdir" logdir_ref

let datadir_ref = ref ""
let datadir_elt = make_element "datadir" datadir_ref

let () = Eliom_config.parse_config [logdir_elt; datadir_elt]

let logdir = !logdir_ref ^ "/"
let datadir = !datadir_ref ^ "/"

let null_date = "1/1/1992_0h0m0s0"

let get_str_localdate () =

  let get_millisecond () =
    int_of_float ((mod_float (Unix.gettimeofday ()) 1.) *. 1000.)
  in

  let tm = Unix.localtime (Unix.time ()) in
  let to_str = string_of_int in
  (to_str tm.Unix.tm_mday) ^ "/" ^ (to_str (tm.Unix.tm_mon + 1)) ^ "/" ^
  (to_str (tm.Unix.tm_year + 1900)) ^ "_" ^ (to_str tm.Unix.tm_hour) ^ "h" ^
  (to_str tm.Unix.tm_min) ^ "m" ^ (to_str tm.Unix.tm_sec) ^ "s" ^
  (to_str (get_millisecond ()))

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
    (** It is to allow old sys which not log millisecond *)
    let mls = if (List.length ltime) >= 7
      then to_int (List.nth ltime 6)
      else 0
    in
    mday, mon, year, hour, min, sec, mls
  with e        -> failwith "Invalide format"

let sec_of_date (mday, mon, year, hour, min, sec, mls) =
  let tm:Unix.tm =
    { Unix.tm_sec = sec;
      Unix.tm_min = min;
      Unix.tm_hour = hour;
      Unix.tm_mday = mday;
      Unix.tm_mon = mon - 1;
      Unix.tm_year = year - 1900;
      Unix.tm_wday = 0;
      Unix.tm_yday = 0;
      Unix.tm_isdst = true }
  in
  let sec, new_tm = Unix.mktime tm in
  sec +. ((float_of_int mls) /. 1000.)

let date_of_jsdate d =
  let rgd = Str.split (Str.regexp "[-]") d in
  try
    let (^^) a b = a ^ "/" ^ b in
    let y = (List.nth rgd 0) in
    let m = (List.nth rgd 1) in
    let j = (List.nth rgd 2) in
    j ^^ m ^^ y
  with e        -> failwith "Invalide format"

let time_of_jstime t =
  let rgt = Str.split (Str.regexp "[:]") t in
  try
    let h = (List.nth rgt 0) in
    let m = (List.nth rgt 1) in
    h ^ "h" ^ m ^ "m0s0"
  with e        -> failwith "Invalide format"

let datetime_of_jsdatetime js_d js_t =
  let d = date_of_jsdate js_d in
  let t = time_of_jstime js_t in
  d ^ "_" ^ t
