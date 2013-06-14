
open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.D

(* values *)

let small_image_width = 480
let small_image_height = Shared_tools.get_min_resolution small_image_width

let medium_image_width = 1400
let medium_image_height = Shared_tools.get_min_resolution medium_image_width

let large_image_width = 2800
let large_image_height = Shared_tools.get_min_resolution large_image_width

(* bus *)

let bus = Eliom_bus.create ~scope:`Site ~name:"drawing"
  ~size:500 Json.t<Server_tools.messages>

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

let base_size = float_of_int
  (Server_tools.get_smaller large_image_width large_image_height)

let draw_server ((color : string), size, (x1, y1), (x2, y2)) =
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

let cmp_small w h = (w <= small_image_width && h <= small_image_height)
let cmp_medium w h = (w <= medium_image_width && h <= medium_image_height)

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
    ~get_params:Eliom_parameter.(int "width" ** int "height" ** int "time")
    (fun (width, (height, _)) () -> Lwt.return
      (let width', height' = if width >= height
        then width, height
        else height, width
       in
       (match width', height' with
         | w, h when (cmp_small w h)     -> small_image_string ()
         | w, h when (cmp_medium w h)    -> medium_image_string ()
         | _                             -> large_image_string ()),
       "image/png"))
