
{client{

  open Lwt
  open Eliom_content.Html5.D

  (** This type allow in theory to handle drawing during resize **)
  (** But it does not work **)
  type resized = Noresize | Startresize | Finishresize

  (** Start and handle draw's event  **)
  let rec start body_elt header_elt canvas_elt angle_elt
      slider_elt color_picker =

    (*** Init data***)
    let (window_orientation, size) =
      Client_canvas.init body_elt header_elt canvas_elt angle_elt
    in
    let win_orientation = ref window_orientation in
    let width = ref (float_of_int (fst size)) in
    let height = ref (float_of_int (snd size)) in
    let float_size = ref (!width, !height) in
    let resize = ref Noresize in
    let base_size = ref (min !width !height)
    in

    let dom_canvas = Eliom_content.Html5.To_dom.of_canvas canvas_elt in
    let dom_slider = Eliom_content.Html5.To_dom.of_input slider_elt in

    let ctx = dom_canvas##getContext (Dom_html._2d_) in
    ctx##lineCap <- Js.string "round";

    let x0, y0 = ref 0, ref 0 in

    let get_origine_canvas () =
      let ox, oy = Dom_html.elementClientPosition dom_canvas in
      x0 := ox;
      y0 := oy;
    in get_origine_canvas ();

    let x = ref 0. and y = ref 0. in

    (*** The initial image ***)

    let copy_image dom_img =
      let width', height' = match !win_orientation with
        | Client_tools.Portrait         ->
          ctx##save(); (* Essential to restore context after it *)
          ctx##translate(!width, 0.);
          ctx##rotate(1.57079633); (* 90 degree *)
          !height, !width
        | _                             -> !width, !height
      in
      ctx##drawImage_withSize(dom_img, 0., 0., width', height');
      match !win_orientation with
        | Client_tools.Portrait         ->
          (* It is very important to restore context *)
          (* Else nothing work any more *)
          ctx##restore()
        | _                             -> ()
    in

    (** Handle set and reset image after window resize **)
    let reset_image () =
      let dom_img =
        (* allow to avoid cach image *)
        let attr = Client_tools.get_timestamp () in
        let image_elt =
          img ~alt:("source image")
            ~src:(make_uri ~service:%Server_image.imageservice
                      (int_of_float !width,
                       (int_of_float !height, attr))) ()
        in
        Eliom_content.Html5.To_dom.of_img image_elt
      in

      let check_load_image copy_func img =
        if (Js.to_bool (img##complete))
        then copy_func img
        else img##onload <- Dom_html.handler (fun _ -> copy_func img; Js._true)
      in check_load_image copy_image dom_img;

    in reset_image ();

    (*** Tools ***)
    let set_coord (x2, y2) =
      x := (float_of_int x2 -. float_of_int !x0) /. !width;
      y := (float_of_int y2 -. float_of_int !y0) /. !height
    in

    let compute_line coord =

      let oldx = !x and oldy = !y in

      set_coord coord;

      (** Try to handle drawing during resize but it does not work **)
      let oldx', oldy' =
        match !resize with
          | Finishresize       -> resize := Noresize; (!x, !y)
          | Startresize        -> (!x, !y)
          | Noresize           -> (oldx, oldy)
      in

      let x1, y1, x2, y2 = match !win_orientation with
        | Client_tools.Portrait -> oldy', 1. -. oldx', !y, 1. -. !x
        | _                     -> oldx', oldy', !x, !y
      in

      let color = Color_picker.get_color color_picker in
      let brush_size =
        (float_of_string (Js.to_string dom_slider##value)) /. 500.
      in

      (* Format for canvas and bus *)
      (* It is differente when you are in Portrait view *)
      ((color, brush_size, (oldx', oldy'), (!x, !y)),
       (color, brush_size, (x1, y1), (x2, y2)))

    in

    let line coord =
      let vo, vb = compute_line coord in

      (** Try to handle drawing during resize but it does not work **)
      let _ = match !resize with
        | Startresize  -> ()
        | _            ->
          ignore (Eliom_bus.write %Server_image.bus vb);
          (* Draw in advance to avoid visual lag *)
          Client_canvas.draw ctx !base_size !float_size vo;
      in
      Lwt.return ()
    in

    let bus_draw (color, brush_size, (x1, y1), (x2, y2)) =
      let x1', y1', x2', y2'  = match !win_orientation with
        | Client_tools.Portrait -> 1. -. y1, x1, 1. -. y2, x2
        | _                     -> x1, y1, x2, y2
      in
      Client_canvas.draw ctx !base_size !float_size
        (color, brush_size, (x1', y1'), (x2', y2'))
    in

    let get_mouse_coord ev = (ev##clientX, ev##clientY) in
    (**
       Duplicate code between get_coord (in get_touch_coord below)
       and get_mouse_coord
       Without this, error of type.
       A specialisation is made on mouseevent.
       However have try this:
       < clientX : 'a; clientY : 'b; .. > Js.t in get_mouse_coord
    **)

    (** Generic function for mouse and touch events **)
    let handle_input_event get_coord move_event stop_event event _ =
      set_coord (get_coord event);
      lwt _ = line (get_coord event) in
      Lwt.pick [move_event Dom_html.document (fun ev _ ->
        line (get_coord ev));
                stop_event Dom_html.document >>= (fun ev ->
                  Lwt.return ())]
    in

    (*** Catch events ***)
    Lwt.async (fun () ->
      Lwt_stream.iter bus_draw (Eliom_bus.stream %Server_image.bus));

    (** Mouse drawing events **)
    let mouse_lwt_cancel = ref false in
    let mouse_catch_thread =
      Lwt_js_events.mousedowns dom_canvas
        (handle_input_event get_mouse_coord
           Lwt_js_events.mousemoves Lwt_js_events.mouseup)
    in Lwt.async (fun () -> mouse_catch_thread);

    (** Duplication code with get_mouse_coord, see that above **)
    let get_touch_coord idx event =
      let _ = if (not !mouse_lwt_cancel)
        then (Lwt.cancel mouse_catch_thread;
              mouse_lwt_cancel := true)
      in
      let ev = event##touches##item(idx) in
      let get_coord ev = (ev##clientX, ev##clientY) in
      Js.Optdef.case ev (fun () -> (0, 0)) get_coord
    in

    (** Touch drawing events **)
    Lwt.async
      (fun () ->
        Lwt_js_events.touchstarts dom_canvas
          (handle_input_event (get_touch_coord 0)
             Lwt_js_events.touchmoves Lwt_js_events.touchend));

    (* fix drag and drop to avoid to catch canvas during drawing *)
    ignore (Client_tools.disable_drag_and_drop dom_canvas);

    (* fix scroll on smartphone to avoid moving up and down on browsers *)
    ignore (Client_tools.disable_mobile_scroll ());

    (* resize and orientationchange listenner *)
    (* handle resize of canvas and redraw image *)
    Lwt.async (fun () ->
      Client_tools.limited_onorientationchanges_or_onresizes (fun _ _ ->
        resize := Startresize;
        let (rc_win_o, (rc_width, rc_height)) =
          Client_canvas.init body_elt header_elt canvas_elt angle_elt
        in
        get_origine_canvas ();
        win_orientation := rc_win_o;
        width := float_of_int rc_width;
        height := float_of_int rc_height;
        float_size := (!width, !height);
        base_size := min !width !height;
        ctx##lineCap <- Js.string "round";
        reset_image ();
        resize := Finishresize;
        Lwt.return ()));

    (* return value *)
    Lwt.return ()

}}
