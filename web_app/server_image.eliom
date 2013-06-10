
open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.D

(* values *)

(* These value are not already real used *)
(* It handle several images' size to avoid long time loading *)
(* But it not work *)
(* And add a problem due to brush size not relatif of image's size *)

let small_image_width = 480
let small_image_height = 960

let medium_image_width = 800 (* 1400 *)
let medium_image_height = 400 (* 2800 *)

let large_image_width = 4200
let large_image_height = 8400

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

(* contex *)

let large_ctx = Cairo.create large_surface
let ctx = Cairo.create medium_surface
let small_ctx = Cairo.create small_surface

(* tool *)

let rgb_from_string color = (* color is in format "#rrggbb" *)
  let get_color i =
    (float_of_string ("0x"^(String.sub color (1+2*i) 2))) /. 255.
  in
  try get_color 0, get_color 1, get_color 2 with | _ -> 0.,0.,0.

(* core *)

let draw_server ((color : string), size, (x1, y1), (x2, y2)) =
  (* Set thickness of brush *)
  Cairo.set_line_width ctx (float size);
  Cairo.set_line_join ctx Cairo.JOIN_ROUND;
  Cairo.set_line_cap ctx Cairo.ROUND;
  let r, g, b =  rgb_from_string color in
  Cairo.set_source_rgb ctx ~r ~g ~b;

  Cairo.move_to ctx (x1 *. (float medium_image_width))
    (y1 *. (float medium_image_height));
  Cairo.line_to ctx (x2 *. (float medium_image_width))
    (y2 *. (float medium_image_height));
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
  let ratio = ( (float_of_int width)
                /. (float_of_int large_image_width) )
  in
  let matrix = Cairo.get_matrix this_ctx in
  Cairo.Matrix.scale ~x:ratio ~y:ratio matrix;
  Cairo.set_matrix this_ctx matrix;
  Cairo.set_source_surface this_ctx large_surface 0. 0.;
  Cairo.paint this_ctx;
  Cairo.restore this_ctx

let medium_image_string () =
  copy_large_surface_to ctx medium_image_width medium_image_height;
  image_string medium_surface

let small_image_string () =
  copy_large_surface_to small_ctx small_image_width small_image_height;
  image_string small_surface

let imageservice name func =
  Eliom_registration.String.register_service
    ~path:["image"; name]
    ~get_params:Eliom_parameter.(int "time")
    (fun _ () -> Lwt.return (func (), "image/png"))

let small_imageservice = imageservice "small" small_image_string

let medium_imageservice = imageservice "medium"
  (fun () -> image_string medium_surface)

let large_imageservice =
  imageservice "large"
    (fun () -> image_string large_surface)

(* Try to include image's source to download it *)
(* But currently not work *)
let download_service =
  Eliom_registration.Html5.register_service
    ~path:["download"]
    ~get_params:Eliom_parameter.
    (string "target" ** string "name" ** string "content_type")
    (fun (target, (name, content_type)) () -> Lwt.return
      (html
         (head (title (pcdata name))
            [meta ~a:[a_http_equiv "content-type";
                      a_content content_type] ();
             meta ~a:[a_http_equiv "content-disposition";
                      a_content ("attachment; filename=" ^ name)] ();
	     meta ~a:[a_http_equiv "cache-control";
                      a_content ("no-cache, must-revalidate")] ()
            ])
         (body [
	   (* script ~a:[a_src (uri_of_string (fun () -> target))] *)
	 ])))
