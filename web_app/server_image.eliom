
open Lwt

(* values *)

let small_image_width = 480
let small_image_height = Shared_tools.get_min_resolution small_image_width

let medium_image_width = 1400
let medium_image_height = Shared_tools.get_min_resolution medium_image_width

let large_image_width = 2800
let large_image_height = Shared_tools.get_min_resolution large_image_width

(* bus *)

let bus = Eliom_bus.create ~scope:`Site ~name:"drawing"
  ~size:500 Json.t<Shared_tools.messages>

(* file *)

let save_file = Lwt_unix.openfile (Server_tools.logdir ^ "/drawing.log")
  [Lwt_unix.O_RDWR; Lwt_unix.O_CREAT; Lwt_unix.O_APPEND] 0o644

let output_file =
  lwt file = save_file in
  Lwt.return
    (Lwt_io.make
       ~mode:Lwt_io.output
       (Lwt_bytes.write file))

let write_log ip (color, brush_size, (oldx, oldy), (x, y)) =
  lwt output = output_file in
  let (^^) a b = a ^ " " ^ b in
  let date =
    let tm = Unix.localtime (Unix.time ()) in
    let to_str = string_of_int in
    (to_str tm.Unix.tm_mday) ^ "/" ^ (to_str tm.Unix.tm_mon) ^ "/" ^
    (to_str (tm.Unix.tm_year + 1900)) ^^ (to_str tm.Unix.tm_hour) ^ "h" ^
    (to_str tm.Unix.tm_min) ^ "m" ^ (to_str tm.Unix.tm_sec) ^ "s"
  in
  let to_str = string_of_float in
  let str = date ^^ ip ^^ color ^^ (to_str brush_size) ^^
    (to_str oldx) ^^ (to_str oldy) ^^ (to_str x) ^^ (to_str y)
  in
  Lwt_io.write_line output str

let input_file =
  lwt file = save_file in
  Lwt.return
    (Lwt_io.make
       ~mode:Lwt_io.input
       (Lwt_bytes.read file))

let read_log () =
  lwt input = input_file in
  lwt str = Lwt_io.read_line input in

  let lstr = Str.split (Str.regexp " ") str in

  try

    let date =
      let mday = List.nth lstr 0 in
      let mon = List.nth lstr 1 in
      let year = List.nth lstr 2 in
      let hms = List.nth lstr 3 in
      let ltime = Str.split (Str.regexp "[hms]") hms in
      let hour = List.nth ltime 0 in
      let min = List.nth ltime 1 in
      let sec = List.nth ltime 2 in
      mday, mon, year, hour, min, sec
    in

    let ip = List.nth lstr 4 in

    let message =
      let color = List.nth lstr 5 in
      let brush_size = List.nth lstr 6 in
      let oldx = List.nth lstr 7 in
      let oldy = List.nth lstr 8 in
      let x = List.nth lstr 9 in
      let y = List.nth lstr 10 in
      let to_flt = float_of_string in
      color, to_flt brush_size, (to_flt oldx, to_flt oldy), (to_flt x, to_flt y)
    in

    Lwt.return (date, ip, message)

  with e        -> failwith "Invalide format"

(* surfaces *)

let large_surface = Cairo.Image.create Cairo.Image.ARGB32
  ~width:large_image_width ~height:large_image_height

let medium_surface = Cairo.Image.create Cairo.Image.ARGB32
  ~width:medium_image_width ~height:medium_image_height

let small_surface = Cairo.Image.create Cairo.Image.ARGB32
  ~width:small_image_width ~height:small_image_height

(* tool *)

let rgb_from_string color = (* color is in format "#rrggbb" *)
  let get_color i =
    (float_of_string ("0x"^(String.sub color (1+2*i) 2))) /. 255.
  in
  try get_color 0, get_color 1, get_color 2 with | _ -> 0.,0.,0.

(* core *)

let small_ctx = Cairo.create small_surface
let medium_ctx = Cairo.create medium_surface
let ctx = Cairo.create large_surface

let base_size = float_of_int large_image_height

let draw_server ((color : string), size, (x1, y1), (x2, y2)) =

  (* save log *)
  Lwt.async (fun () -> write_log "127.0.0.1" (color, size, (x1, y1), (x2, y2)));

  (* Set thickness of brush *)
  Cairo.set_line_width ctx (size *. base_size);
  Cairo.set_line_join ctx Cairo.JOIN_ROUND;
  Cairo.set_line_cap ctx Cairo.ROUND;
  let r, g, b =  rgb_from_string color in
  Cairo.set_source_rgb ctx ~r ~g ~b;

  Cairo.move_to ctx (x1 *. (float_of_int large_image_width))
    (y1 *. (float_of_int large_image_height));
  Cairo.line_to ctx (x2 *. (float_of_int large_image_width))
    (y2 *. (float_of_int large_image_height));
  Cairo.Path.close ctx;

  (* Apply the ink *)
  Cairo.stroke ctx

let _ = Lwt_stream.iter draw_server (Eliom_bus.stream bus)

let image_string surface =
  let b = Buffer.create 10000 in
  (* Output a PNG in a string *)
  Cairo.PNG.write_to_stream surface (Buffer.add_string b);
  Buffer.contents b

let copy_large_surface_to this_ctx width height =
  Cairo.save this_ctx;
  let x_ratio = ((float_of_int width) /. (float_of_int large_image_width)) in
  let y_ratio = ((float_of_int height) /. (float_of_int large_image_height)) in
  let matrix = Cairo.get_matrix this_ctx in
  Cairo.Matrix.scale ~x:x_ratio ~y:y_ratio matrix;
  Cairo.set_matrix this_ctx matrix;
  Cairo.set_source_surface this_ctx large_surface 0. 0.;
  Cairo.paint this_ctx;
  Cairo.restore this_ctx

let medium_image_string () =
  copy_large_surface_to medium_ctx medium_image_width medium_image_height;
  image_string medium_surface

let small_image_string () =
  copy_large_surface_to small_ctx small_image_width small_image_height;
  image_string small_surface

let large_image_string () =
  image_string large_surface

let cmp_small w = w <= small_image_width
let cmp_medium w = w <= medium_image_width

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
    ~get_params:Eliom_parameter.(int "width" ** int "time")
    (fun (width, _) () -> Lwt.return
      ((match width with
        | w when (cmp_small w)  -> small_image_string ()
        | w when (cmp_medium w) -> medium_image_string ()
        | _                     -> large_image_string ()),
      "image/png"))
