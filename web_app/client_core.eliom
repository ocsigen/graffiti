
{client{

  open Lwt
  open Eliom_content.Html5.D

  (** This type allow in theory to handle drawing during resize **)
  (** But it does not work **)
  type resized = Noresize | Startresize | Finishresize

  (** Start and handle draw's event  **)
  let rec start body_elt header_elt canvas_elt angle_elt slider color_picker =

    (*** Init data***)
    let (window_orientation, size) =
      Client_canvas.init body_elt header_elt canvas_elt angle_elt
    in
    let win_orientation = ref window_orientation in
    let width = ref (float_of_int (fst size)) in
    let height = ref (float_of_int (snd size)) in
    let float_size = ref (!width, !height) in
    let resize = ref Noresize in
    let base_size = ref (min !width !height) in

    let dom_canvas = Eliom_content.Html5.To_dom.of_canvas canvas_elt in

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

      let color = Grf_color_picker.get_color color_picker in
      let brush_size = Client_ext_mod_tools.get_slider_value slider in

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

    (*** Catch events ***)
    Lwt.async (fun () ->
      Lwt_stream.iter bus_draw (Eliom_bus.stream %Server_image.bus));

    (** drawing events **)
    Lwt.async (fun () -> Client_tools.touch_or_mouse_slides dom_canvas
      (fun ev _ -> set_coord (Client_tools.get_slide_coord 0 ev);
                   line (Client_tools.get_slide_coord 0 ev))
      (fun ev _ -> line (Client_tools.get_slide_coord 0 ev))
      (fun ev -> line (Client_tools.get_slide_coord 0 ev)));

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
