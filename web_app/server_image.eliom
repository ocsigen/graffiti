
open Lwt

(* bus *)

let bus = Eliom_bus.create ~scope:`Site ~name:"drawing"
  ~size:500 Json.t<Shared_tools.messages>

(* file *)

let save_file_name = (Server_tools.logdir ^ "drawing.log")

let output_file =
  Lwt_io.open_file
    ~flags:[Lwt_unix.O_RDWR; Lwt_unix.O_CREAT; Lwt_unix.O_APPEND]
    ~perm:0o644
    ~mode:Lwt_io.output
    save_file_name

(* log tools *)

let input_file () =
  Lwt_io.open_file ~mode:Lwt_io.input save_file_name

let write_log ip (color, brush_size, (oldx, oldy), (x, y)) =
  lwt output = output_file in
  let (^^) a b = a ^ " " ^ b in
  let date = Server_tools.get_str_localdate () in
  let to_str = string_of_float in
  let str = date ^^ ip ^^ color ^^ (to_str brush_size) ^^
    (to_str oldx) ^^ (to_str oldy) ^^ (to_str x) ^^ (to_str y)
  in
  Lwt_io.write_line output str

let read_log input =
  lwt str = Lwt_io.read_line input in

  let lstr = Str.split (Str.regexp " ") str in
  let date = Server_tools.get_date_value (List.nth lstr 0) in
  let ip = List.nth lstr 1 in
  let message =
    let to_flt = float_of_string in
    let color = List.nth lstr 2 in
    let brush_size = to_flt (List.nth lstr 3) in
    let oldx = to_flt (List.nth lstr 4) in
    let oldy = to_flt (List.nth lstr 5) in
    let x = to_flt (List.nth lstr 6) in
    let y = to_flt (List.nth lstr 7) in
    color, brush_size, (oldx, oldy), (x, y)
  in
  Lwt.return (date, ip, message)

(** start_drawing and end_drawing is a string
    in format of server_tools.get_str_localdate

    coef_to_replay by default at 0. to replay instanment
    at 1. replay in real time

    action is function to execute at each getted message *)
let replay_drawing ?(coef_to_replay=0.) start_drawing end_drawing action =
  lwt input = input_file () in
  let s = Server_tools.sec_of_date
    (Server_tools.get_date_value start_drawing)
  in
  let e = Server_tools.sec_of_date
    (Server_tools.get_date_value end_drawing)
  in

  let map start_time end_time coef action =

    let get_and_wait last_time =
      lwt date, _, message = read_log input in
      let current_time = Server_tools.sec_of_date date in
      let time_to_sleep = (current_time -. last_time) *. coef in
      lwt () = if (last_time > 0. && time_to_sleep > 0.)
        then Lwt_unix.sleep time_to_sleep
        else Lwt.return ()
      in Lwt.return (current_time, message)
    in

    let rec check_and_do_action current_time message =
      if (current_time < end_time)
      then
        lwt () = action message in
        aux current_time
      else Lwt.return (message)

    and aux last_time =
      try
        lwt current_time, message = get_and_wait last_time in
        check_and_do_action current_time message
      with Failure "Invalide format"   -> aux last_time
    in aux start_time

  in

  try_lwt
    lwt msg = map 0. s 0. (fun _ -> Lwt.return ()) in
    lwt () = action msg in
    lwt _ = map s e coef_to_replay action in
    lwt () = Lwt_io.close input in
    Lwt.return ()
  with End_of_file      ->
    lwt () = Lwt_io.close input in
    Lwt.return ()


(* surfaces' data *)

let create file_name (width, height) =
  try (* check if file exist, if it okay: create from file *)
    Unix.access file_name [Unix.F_OK; Unix.R_OK];
    Cairo.PNG.create file_name
  with (* else create simple surface *)
    | e -> Cairo.Image.create Cairo.Image.ARGB32 ~width ~height

let save_step = 1000
let store = Ocsipersist.open_store "drawing"
let nb_drawing () = Ocsipersist.make_persistent ~store
  ~name:"nb_drawing" ~default:0
let last_save () = Ocsipersist.make_persistent ~store
  ~name:"last_save" ~default:Server_tools.null_date

let small_name = Server_tools.datadir ^ "small_image.png"
let medium_name = Server_tools.datadir ^ "medium_image.png"
let large_name = Server_tools.datadir ^ "large_image.png"

let small_width = 480
let small_height = Shared_tools.get_min_resolution small_width
let medium_width = 1400
let medium_height = Shared_tools.get_min_resolution medium_width
let large_width = 2800
let large_height = Shared_tools.get_min_resolution large_width

let small_surface = create small_name (small_width, small_height)
let medium_surface = create medium_name (medium_width, medium_height)
let large_surface = create large_name (large_width, large_height)

let small_ctx = Cairo.create small_surface
let medium_ctx = Cairo.create medium_surface
let large_ctx = Cairo.create large_surface

let small_base_size = float_of_int small_height
let medium_base_size = float_of_int medium_height
let large_base_size = float_of_int large_height

let save_image file_name surface =
  Cairo.PNG.write surface file_name

(** save images by step of 1000 drawing  *)
let save_all_images () =
  lwt nb = nb_drawing () in
  lwt current = Ocsipersist.get nb in
  if current >= save_step then
    begin
      save_image small_name small_surface;
      save_image medium_name medium_surface;
      save_image large_name large_surface;

      lwt last = last_save () in
      lwt () = Ocsipersist.set last (Server_tools.get_str_localdate ()) in
      Ocsipersist.set nb 0
    end
  else Lwt.return ()

(* draw tools *)

let rgb_from_string color = (* color is in format "#rrggbb" *)
  let get_color i =
    (float_of_string ("0x"^(String.sub color (1+2*i) 2))) /. 255.
  in
  try get_color 0, get_color 1, get_color 2 with | _ -> 0.,0.,0.

let draw ctx base_size (width, height)
    ((color : string), size, (x1, y1), (x2, y2)) =

  (* Set thickness of brush *)
  Cairo.set_line_width ctx (size *. base_size);
  Cairo.set_line_join ctx Cairo.JOIN_ROUND;
  Cairo.set_line_cap ctx Cairo.ROUND;
  let r, g, b =  rgb_from_string color in
  Cairo.set_source_rgb ctx ~r ~g ~b;

  Cairo.move_to ctx (x1 *. (float_of_int width))
    (y1 *. (float_of_int height));
  Cairo.line_to ctx (x2 *. (float_of_int width))
    (y2 *. (float_of_int height));
  Cairo.Path.close ctx;

  (* Apply the ink *)
  Cairo.stroke ctx

let draw_server savelog data =
  begin
    draw small_ctx small_base_size (small_width, small_height) data;
    draw medium_ctx medium_base_size (medium_width, medium_height) data;
    draw large_ctx large_base_size (large_width, large_height) data;

    (* TODO: Replace 127.0.0.1 by the IP *)
    if savelog then
        lwt () = write_log "127.0.0.1" data in
        lwt nb = nb_drawing () in
        lwt current = Ocsipersist.get nb in
        lwt () = Ocsipersist.set nb (current + 1) in
        save_all_images ()
    else Lwt.return ()
  end

(* init images *)

(* initialize image with white *)
let init_white ctx (width, height) =
  Cairo.set_source_rgb ctx ~r:1. ~g:1. ~b:1.;
  Cairo.rectangle ctx 0. 0. (float_of_int width) (float_of_int height);
  Cairo.stroke_preserve ctx;
  Cairo.fill ctx;

  (* Apply the ink *)
  Cairo.stroke ctx

let init_image file_name surface ctx (width, height) =
  try (* check if file exist, if it okay: do nothing *)
    Unix.access file_name [Unix.F_OK; Unix.R_OK];
  with (* else init image with white and save file *)
    | e ->
      begin
        init_white ctx (width, height);
        save_image file_name surface
      end

let replay_no_save_drawing =
  begin
    (* init images *)
    init_image small_name small_surface small_ctx
      (small_width, small_height);
    init_image medium_name medium_surface medium_ctx
      (medium_width, medium_height);
    init_image large_name large_surface large_ctx
      (large_width, large_height);

    (* redrawing not save drawing *)
    lwt v = last_save () in
    lwt last = Ocsipersist.get v in
    let current = Server_tools.get_str_localdate () in
    replay_drawing last current (draw_server false)
  end

(* catch drawing *)

let _ = Lwt_stream.iter_s (draw_server true) (Eliom_bus.stream bus)

(* get drawing tools *)

let image_string surface =
  let b = Buffer.create 10000 in
  (* Output a PNG in a string *)
  Cairo.PNG.write_to_stream surface (Buffer.add_string b);
  Buffer.contents b

let medium_image_string () = image_string medium_surface
let small_image_string () = image_string small_surface
let large_image_string () = image_string large_surface

let cmp_small w = w <= small_width
let cmp_medium w = w <= medium_width

let download_imageservice =
  Eliom_registration.String.register_service
    ~path:["image"; "graffiti.png"]
    ~get_params:Eliom_parameter.unit
    (fun () () -> Lwt.return (medium_image_string (), "image/png"))

let large_download_imageservice =
  Eliom_registration.String.register_service
    ~path:["image"; "large_graffiti.png"]
    ~get_params:Eliom_parameter.unit
    (fun () () -> Lwt.return (large_image_string (), "image/png"))

let imageservice =
  Eliom_registration.String.register_service
    ~path:["image"; "adaptation.png"]
    ~get_params:Eliom_parameter.(int "width")
    (fun (width) () -> Lwt.return
      ((match width with
        | w when (cmp_small w)  -> small_image_string ()
        | w when (cmp_medium w) -> medium_image_string ()
        | _                     -> large_image_string ()),
      "image/png"))
