{server{

   let _ = Eliom_state.set_global_volatile_data_state_timeout ~scope:Eliom_common.comet_client_process (Some 20.)

 }}

{shared{
  open Eliom_pervasives
  open HTML5.M
  let width = 700
  let height = 300
}}

module My_appl =
  Eliom_output.Eliom_appl (struct
    let application_name = "graffiti"
  end)

{client{

  open Event_arrows
  let draw ctx (color, size, (x1, y1), (x2, y2)) =
    ctx##strokeStyle <- (Js.string color);
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
  type messages = (string * int * (int * int) * (int * int)) deriving (Json)
}}

let bus = Eliom_bus.create ~scope:`Global ~name:"grib" ~size:500 Json.t<messages>

let rgb_from_string color = (* color is in format "#rrggbb" *)
  let get_color i = (float_of_string ("0x"^(String.sub color (1+2*i) 2))) /. 255. in
  try get_color 0, get_color 1, get_color 2 with | _ -> 0.,0.,0.

let draw_server, image_string =
  let surface = Cairo.image_surface_create Cairo.FORMAT_ARGB32 ~width ~height in
  let ctx = Cairo.create surface in
  ((fun ((color : string), size, (x1, y1), (x2, y2)) ->

    (* Set thickness of brush *)
    Cairo.set_line_width ctx (float size) ;
    Cairo.set_line_join ctx Cairo.LINE_JOIN_ROUND ;
    Cairo.set_line_cap ctx Cairo.LINE_CAP_ROUND ;
    let red, green, blue =  rgb_from_string color in
    Cairo.set_source_rgb ctx ~red ~green ~blue ;

    Cairo.move_to ctx (float x1) (float y1) ;
    Cairo.line_to ctx (float x2) (float y2) ;
    Cairo.close_path ctx ;

    (* Apply the ink *)
    Cairo.stroke ctx ;
   ),
   (fun () ->
     let b = Buffer.create 10000 in
     (* Output a PNG in a string *)
     Cairo_png.surface_write_to_stream surface (Buffer.add_string b);
     Buffer.contents b
   ))

let _ = Lwt_stream.iter draw_server (Eliom_bus.stream bus)

let oclosure_script =
    HTML5.M.unique
      (HTML5.M.script
         ~a:[HTML5.M.a_src (HTML5.M.uri_of_string "./graffiti_oclosure.js")]
         (HTML5.M.pcdata ""))

let imageservice =
  Eliom_output.Text.register_service
    ~path:["image"]
    ~get_params:Eliom_parameters.unit
    (fun () () -> Lwt.return (image_string (), "image/png"))

let main_service =
  My_appl.register_service ~path:[""] ~get_params:Eliom_parameters.unit
    (fun () () ->
      let image_elt = unique (img ~alt:"canvas" ~src:(Eliom_output.Html5.make_string_uri
						~service:imageservice ()) ()) in
      let canvas_elt = unique (canvas ~a:[ a_width width; a_height height ]
				 [pcdata "your browser doesn't support canvas"; br ();
				  image_elt]) in
      let canvas2_elt = unique (canvas ~a:[ a_width width; a_height height ] []) in
       Eliom_services.onload
         {{
	   let canvas = Eliom_client.Html5.of_canvas %canvas_elt in
           let st = canvas##style in
           st##position <- Js.string "absolute";
           st##zIndex <- Js.string "-1";
           let ctx = canvas##getContext (Dom_html._2d_) in
           ctx##lineCap <- Js.string "round";

           (* Another canvas, for second layer *)
	   let canvas2 = Eliom_client.Html5.of_canvas %canvas2_elt in
           canvas2##width <- width; canvas2##height <- height;
           let ctx2 = canvas2##getContext (Dom_html._2d_) in
           ctx2##lineCap <- Js.string "round";

           (* The initial image: *)
	   let img = Eliom_client.Html5.of_img %image_elt in
	   let copy_image () = ctx##drawImage(img, 0., 0.) in
	   if Js.to_bool (img##complete)
	   then copy_image ()
	   else img##onload <- Dom_html.handler
	     (fun ev -> copy_image (); Js._false);

           (* Size of the brush *)
           let slider = jsnew Goog.Ui.slider(Js.null) in
           slider##setOrientation(Goog.Ui.SliderBase.Orientation._VERTICAL);
           slider##setMinimum(1.);
           slider##setMaximum(80.);
           slider##setValue(10.);
           slider##setMoveToPointEnabled(Js._true);
           slider##render(Js.some Dom_html.document##body);

           (* The color palette: *)
           let pSmall =
(*VVV Problems with HSVA:
 - the widget gives #rrggbbaa and the canvas expects rgba(rrr, ggg, bbb, a)
 - the point between two lines is displayed twice (=> darker)
*)
             jsnew Goog.Ui.hsvPalette(Js.null, Js.null,
                                      Js.some (Js.string "goog-hsv-palette-sm"))
           in
           pSmall##render(Js.some Dom_html.document##body);

           let x = ref 0 and y = ref 0 in
           let set_coord ev =
             let x0, y0 = Dom_html.elementClientPosition canvas in
             x := ev##clientX - x0; y := ev##clientY - y0 in
           let compute_line set_coord x y ev =
             let oldx = !x and oldy = !y in
             set_coord ev;
             let color = Js.to_string (pSmall##getColor()) in
             let size = int_of_float (Js.to_float (slider##getValue())) in
             (color, size, (oldx, oldy), (!x, !y))
           in
           let (bus:messages Eliom_bus.t) = %bus in
           let line ev =
             let v = compute_line set_coord x y ev in
             let _ = Eliom_bus.write bus v in
             draw ctx v
           in
           ignore (Lwt_js.sleep 0.1 >>= fun () -> (* avoid chromium looping cursor *)
                   Lwt.catch
                     (fun () ->
                       Lwt_stream.iter (draw ctx) (Eliom_bus.stream bus))
                     (function e (* Eliom_comet.Channel_full *) ->
                       Firebug.console##log (e);
                       Eliom_client.exit_to
                         ~service:Eliom_services.void_coservice' () ();
                       Lwt.return ()));
(*                       | e -> Lwt.fail e)); *)
           ignore (run (mousedowns canvas2
                          (arr (fun ev -> set_coord ev; line ev)
                           >>> first [mousemoves Dom_html.document (arr line);
                                      mouseup Dom_html.document >>> (arr line)])) ());



           (* The brush *)
(*VVV bof document.
  Mieux : canvas2 mais il faut gérer la sortie du canvas... *)
           ctx2##globalCompositeOperation <- Js.string "copy";
           let x, y, size = ref 0, ref 0, ref 0 in
           let set_coord ev =
             let x0, y0 = Dom_html.elementClientPosition canvas2 in
             x := ev##clientX - x0; y := ev##clientY - y0 in
           let brush ev =
             let (color, newsize, oldv, v) = compute_line set_coord x y ev in
             draw ctx2 ("rgba(0,0,0,0)", !size+3, oldv, oldv);
             size := newsize;
             draw ctx2 (color, newsize, v, v)
           in
           ignore (run (mousemoves Dom_html.document (arr brush)) ())

         }};
      Lwt.return
        (HTML5.M.html
	  (HTML5.M.head
	    (HTML5.M.title (HTML5.M.pcdata "Graffiti"))
 	    [ HTML5.M.link ~rel:[ `Stylesheet ]
                ~href:(HTML5.M.uri_of_string"./css/graffiti.css")
                ();
              HTML5.M.link ~rel:[ `Stylesheet ]
                ~href:(HTML5.M.uri_of_string"./css/closure/common.css")
                ();
              HTML5.M.link ~rel:[ `Stylesheet ]
                ~href:(HTML5.M.uri_of_string"./css/closure/hsvpalette.css")
                ();
              HTML5.M.link ~rel:[ `Stylesheet ]
                ~href:(HTML5.M.uri_of_string"./css/slider.css")
                ();
              oclosure_script;
            ])
	  (HTML5.M.body [canvas_elt; canvas2_elt]))
   )
