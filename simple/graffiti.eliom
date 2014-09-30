(* Graffiti
 * http://www.ocsigen.org/graffiti
 * Copyright (C) 2013 Vincent Balat
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

{shared{
  open Eliom_lib.Lwt_ops
  open Eliom_content
  let width = 700
  let height = 300
}}

let _ =
  Eliom_state.set_global_volatile_data_state_timeout
    ~cookie_scope:Eliom_common.comet_client_process_scope (Some 20.)

module My_appl =
  Eliom_registration.App (struct
    let application_name = "graffiti"
  end)

{client{

  let draw ?(alpha = 1.) ctx ((r, g, b), size, (x1, y1), (x2, y2)) =
    let color = CSS.Color.string_of_t (CSS.Color.rgb r g b ~a:alpha) in
    ctx##strokeStyle <- Js.string color;
    ctx##lineWidth <- float size;
    ctx##beginPath();
    ctx##moveTo(float x1, float y1);
    ctx##lineTo(float x2, float y2 +. 0.1); (* The 0.1 is a fix for Chrome
                                               (does not draw lines if the
                                               first and last points are equal) *)
    ctx##stroke()

  let () =
    let c = Eliom_comet.Configuration.new_configuration () in
    Eliom_comet.Configuration.set_active_until_timeout c true



}}

{shared{
  type messages = ((int * int *int) * int * (int * int) * (int * int)) deriving (Json)
  type canvas = Html5_types.flow5 Html5_types.canvas Eliom_content.Html5.elt
}}

let bus : (messages, messages) Eliom_bus.t = Eliom_bus.create ~scope:`Site ~name:"grib" ~size:500 Json.t<messages>

let draw_server, image_string =
  let rgb_ints_to_floats (r, g, b) =
    float r /. 255., float g /. 255., float b /. 255. in (* needed by cairo *)
  let surface = Cairo.Image.create Cairo.Image.ARGB32 ~width ~height in
  let ctx = Cairo.create surface in
  ((fun (rgb, size, (x1, y1), (x2, y2)) ->

    (* Set thickness of brush *)
    let r, g, b = rgb_ints_to_floats rgb in
    Cairo.set_line_width ctx (float size) ;
    Cairo.set_line_join ctx Cairo.JOIN_ROUND ;
    Cairo.set_line_cap ctx Cairo.ROUND ;
    Cairo.set_source_rgb ctx ~r ~g ~b ;

    Cairo.move_to ctx (float x1) (float y1) ;
    Cairo.line_to ctx (float x2) (float y2) ;
    Cairo.Path.close ctx ;

    (* Apply the ink *)
    Cairo.stroke ctx ;
   ),
   (fun () ->
     let b = Buffer.create 10000 in
     (* Output a PNG in a string *)
     Cairo.PNG.write_to_stream surface (Buffer.add_string b);
     Buffer.contents b
   ))

let _ = Lwt_stream.iter draw_server (Eliom_bus.stream bus)

let imageservice =
  Eliom_registration.String.register_service
    ~path:["image"]
    ~get_params:Eliom_parameter.unit
    (fun () () -> Lwt.return (image_string (), "image/png"))

let image_elt =
  Html5.D.img ~alt:"canvas"
    ~src:(Html5.D.make_uri ~service:imageservice ())
    ()

let canvas_elt : canvas =
  Html5.D.canvas ~a:[ Html5.D.a_width width; Html5.D.a_height height ]
           [Html5.D.pcdata "your browser doesn't support canvas";
            Html5.D.br ();
            image_elt]

let canvas2_elt : canvas =
  Html5.D.canvas ~a:[ Html5.D.a_width width; Html5.D.a_height height ] []

let slider = Html5.D.int_input
    ~a:[Html5.D.a_id "slider"; Html5.D.a_input_min 1.; Html5.D.a_input_max 80.]
    ~input_type:`Range ()

let page =
  Html5.D.html
    (Html5.D.head
       (Html5.D.title (Html5.D.pcdata "Graffiti"))
       [ Html5.D.css_link
	   ~uri:(Html5.D.make_uri
		   (Eliom_service.static_dir ()) ["css";"graffiti.css"]) ();
	])
    (Html5.D.body [
       Html5.D.div ~a:[] [canvas_elt; canvas2_elt];
       Html5.D.div ~a:[] [slider]])

{client{

let init_client () =

  let colorpicker = Ow_color_picker.create ~width:150 () in
  Ow_color_picker.append_at (Dom_html.document##body) colorpicker;
  Ow_color_picker.init_handler colorpicker;
  let canvas = Html5.To_dom.of_canvas %canvas_elt in
  let st = canvas##style in
  st##position <- Js.string "absolute";
  st##zIndex <- Js.string "-1";
  let ctx = canvas##getContext (Dom_html._2d_) in
  ctx##lineCap <- Js.string "round";

  (* Another canvas, for second layer *)
  let canvas2 = Html5.To_dom.of_canvas %canvas2_elt in
  canvas2##width <- width; canvas2##height <- height;
  let ctx2 = canvas2##getContext (Dom_html._2d_) in
  ctx2##lineCap <- Js.string "round";

  (* The initial image: *)
  let img = Html5.To_dom.of_img %image_elt in
  let copy_image () = ctx##drawImage(img, 0., 0.) in
  if Js.to_bool (img##complete)
  then copy_image ()
  else img##onload <- Dom_html.handler
    (fun ev -> copy_image (); Js._false);

  let x = ref 0 and y = ref 0 in
  let set_coord ev =
    let x0, y0 = Dom_html.elementClientPosition canvas in
    x := ev##clientX - x0; y := ev##clientY - y0 in
  let compute_line set_coord x y ev =
    let oldx = !x and oldy = !y in
    set_coord ev;
    let rgb = Ow_color_picker.get_rgb colorpicker in
    let size_slider = Html5.To_dom.of_input %slider in
    let size = int_of_string (Js.to_string size_slider##value) in
    (rgb, size, (oldx, oldy), (!x, !y))
  in
  let bus = %bus in
  let line ev =
    let v = compute_line set_coord x y ev in
    let _ = Eliom_bus.write bus v in
    draw ctx v;
    Lwt.return ()
  in
  ignore (Lwt_js.sleep 0.1 >>= fun () -> (* avoid chromium looping cursor *)
          Lwt.catch
            (fun () ->
              Lwt_stream.iter (draw ctx) (Eliom_bus.stream bus))
            (function e (* Eliom_comet.Channel_full *) ->
              Firebug.console##log (e);
              Eliom_client.exit_to
                ~service:Eliom_service.void_coservice' () ();
              Lwt.return ()));
  (*                       | e -> Lwt.fail e)); *)
  Lwt_js_events.(async (fun () ->
    mousedowns canvas2 (fun ev elt ->
      Dom.preventDefault ev;
      set_coord ev;
      lwt () = line ev in
      Lwt.pick [mousemoves Dom_html.document (fun a _ -> line a);
	        lwt ev = mouseup Dom_html.document in line ev])));



  (* The brush *)
  ctx2##globalCompositeOperation <- Js.string "copy";
  let x, y, size = ref 0, ref 0, ref 0 in
  let set_coord ev =
    let x0, y0 = Dom_html.elementClientPosition canvas2 in
    x := ev##clientX - x0; y := ev##clientY - y0 in
  let brush ev _ =
    let (color, newsize, oldv, v) = compute_line set_coord x y ev in
    draw  ~alpha:0. ctx2 ((0, 0, 0), !size+3, oldv, oldv);
    size := newsize;
    draw ctx2 (color, newsize, v, v);
    Lwt.return ()
  in
  ignore Lwt_js_events.(async (fun () -> (mousemoves Dom_html.document brush)))

}}

let main_service =
  My_appl.register_service ~path:[""] ~get_params:Eliom_parameter.unit
    (fun () () ->
      ignore {unit{ init_client () }};
      Lwt.return page)
