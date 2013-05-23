
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

let bus = Eliom_bus.create ~scope:`Site ~name:"grib" ~size:500 Json.t<messages>

let rgb_from_string color = (* color is in format "#rrggbb" *)
  let get_color i =
    (float_of_string ("0x"^(String.sub color (1+2*i) 2))) /. 255.
  in
  try get_color 0, get_color 1, get_color 2 with | _ -> 0.,0.,0.

let draw_server, image_string =
  let surface = Cairo.Image.create Cairo.Image.ARGB32 ~width ~height in
  let ctx = Cairo.create surface in
  ((fun ((color : string), size, (x1, y1), (x2, y2)) ->

    (* Set thickness of brush *)
    Cairo.set_line_width ctx (float size) ;
    Cairo.set_line_join ctx Cairo.JOIN_ROUND ;
    Cairo.set_line_cap ctx Cairo.ROUND ;
    let r, g, b =  rgb_from_string color in
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
let canvas_elt =
  Html5.D.canvas ~a:[ Html5.D.a_width width; Html5.D.a_height height ]
           [Html5.D.pcdata "your browser doesn't support canvas";
            Html5.D.br ();
            image_elt]
let canvas2_elt =
  Html5.D.canvas ~a:[ Html5.D.a_width width; Html5.D.a_height height ] []

let page =
  Html5.D.html
    (Html5.D.head
       (Html5.D.title (Html5.D.pcdata "Graffiti"))
       [ Html5.D.css_link
	   ~uri:(Html5.D.make_uri
		   (Eliom_service.static_dir ()) ["css";"closure";"common.css"]) ();
	 Html5.D.css_link
	   ~uri:(Html5.D.make_uri
		   (Eliom_service.static_dir ()) ["css";"closure";"hsvpalette.css"]) ();
	 Html5.D.css_link
	   ~uri:(Html5.D.make_uri
		   (Eliom_service.static_dir ()) ["css";"slider.css"]) ();
	 Html5.D.css_link
	   ~uri:(Html5.D.make_uri
		   (Eliom_service.static_dir ()) ["css";"graffiti.css"]) ();
	 Html5.D.js_script
	   ~uri:(Html5.D.make_uri
		   (Eliom_service.static_dir ()) ["graffiti_oclosure.js"]) ();
       ])
    (Html5.D.body [canvas_elt; canvas2_elt])

{client{
let init_client () =

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
      set_coord ev;
      lwt () = line ev in
      Lwt.pick [mousemoves Dom_html.document (fun a _ -> line a);
	        lwt ev = mouseup Dom_html.document in line ev])));



  (* The brush *)
  (*VVV bof document.
    Mieux : canvas2 mais il faut gérer la sortie du canvas... *)
  ctx2##globalCompositeOperation <- Js.string "copy";
  let x, y, size = ref 0, ref 0, ref 0 in
  let set_coord ev =
    let x0, y0 = Dom_html.elementClientPosition canvas2 in
    x := ev##clientX - x0; y := ev##clientY - y0 in
  let brush ev _ =
    let (color, newsize, oldv, v) = compute_line set_coord x y ev in
    draw ctx2 ("rgba(0,0,0,0)", !size+3, oldv, oldv);
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
