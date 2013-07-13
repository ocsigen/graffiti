
{client{

  open Lwt
  open Eliom_content.Html5.D

  module IntOrdered =
  struct
    type t = int
    let compare = compare
  end

  module IntMap = Map.Make(IntOrdered)

  (** Start and handle draw's event  **)
  let rec start body_elt header_elt canvas_elt canvas2_elt slider color_picker =

    (*** Init data***)
    let size =
      Client_canvas.init_size body_elt header_elt canvas_elt canvas2_elt
    in
    let width = ref (float_of_int (fst size)) in
    let height = ref (float_of_int (snd size)) in
    let float_size = ref (!width, !height) in
    let bus_mutex = Lwt_mutex.create () in
    let base_size = ref !height in

    let dom_canvas = Eliom_content.Html5.To_dom.of_canvas canvas_elt in
    let dom_canvas2 = Eliom_content.Html5.To_dom.of_canvas canvas2_elt in

    let ctx = dom_canvas##getContext (Dom_html._2d_) in
    ctx##lineCap <- Js.string "round";
    let ctx2 = dom_canvas2##getContext (Dom_html._2d_) in
    ctx2##lineCap <- Js.string "round";
    ctx2##globalCompositeOperation <- Js.string "copy";

    let x0, y0 = ref 0, ref 0 in

    let get_origine_canvas () =
      let ox, oy = Dom_html.elementClientPosition dom_canvas in
      x0 := ox;
      y0 := oy;
    in get_origine_canvas ();

    (*** The initial image ***)

    Lwt.async (fun () ->
      Client_canvas.init_image ctx bus_mutex (!width, !height));

    (*** Tools ***)
    let set_coord (x, y) (x2, y2) =
      x := (float_of_int x2 -. float_of_int !x0) /. !width;
      y := (float_of_int y2 -. float_of_int !y0) /. !height
    in

    let compute_line (x, y) coord =

      let oldx = !x and oldy = !y in

      set_coord (x, y) coord;

      let color = Grf_color_picker.get_color color_picker in
      let brush_size = Client_tools.get_slider_value slider in

      (* Format for canvas and bus *)
      (color, brush_size, (oldx, oldy), (!x, !y))

    in

    let line (x, y) coord =
      let data = compute_line (x, y) coord in
      ignore (Eliom_bus.write %Server_image.bus data);
      (* Draw in advance to avoid visual lag *)
      Client_canvas.draw ctx !base_size !float_size data;
      Lwt.return ()
    in

    let bus_draw (color, brush_size, (x1, y1), (x2, y2)) =
      lwt () = Lwt_mutex.lock bus_mutex in
      Client_canvas.draw ctx !base_size !float_size
        (color, brush_size, (x1, y1), (x2, y2));
      Lwt_mutex.unlock bus_mutex;
      Lwt.return ()
    in

    (*** Catch events ***)

    (* get bus message *)
    Lwt.async (fun () ->
      Lwt_stream.iter_s bus_draw (Eliom_bus.stream %Server_image.bus));

    (* To avoid double actions of drawing *)
    Client_event_tools.disable_ghost_mousemove Dom_html.document;
    Client_event_tools.disable_ghost_mouse_event dom_canvas2;

    (* drawing events *)
    let x = ref 0. and y = ref 0. in
    let touch_coord = ref IntMap.empty in

    let reset_coord ev =
      touch_coord := IntMap.empty;
      set_coord (x, y) (Client_event_tools.get_slide_coord 0 ev)
    in

    let handle_change_touch action ev =
      let list = ev##changedTouches in
      let length = list##length in

      let get_data n list =
        Js.Optdef.case (list##item(n))
          (fun () -> -1, (0, 0))
          (fun item -> item##identifier, Client_event_tools.get_coord item)
      in

      let insert id (x, y) =
        let tmp = ref 0., ref 0. in
        set_coord tmp (x, y);
        touch_coord := IntMap.add id tmp !touch_coord;
        tmp
      in

      let do_action id new_coord =
        let old_coord =
          if IntMap.exists (fun i _ -> i = id) !touch_coord
          then IntMap.find id !touch_coord
          else insert id new_coord
        in
        action old_coord new_coord
      in

      let rec aux = function
        | -1    -> Lwt.return ()
        | n     ->
          let id, new_coord = get_data n list in
          lwt () = if n >= 0
            then do_action id new_coord
            else Lwt.return ()
          in
          aux (n - 1)
      in aux (length - 1)
    in

    let handler action = function
      | Client_event_tools.Touch_event ev       ->
        handle_change_touch action ev
      | Client_event_tools.Mouse_event ev       ->
        action (x, y) (Client_event_tools.get_coord ev)
    in

    Lwt.async (fun () -> Client_event_tools.touch_or_mouse_slides dom_canvas2
      (fun ev _ -> reset_coord ev; handler line ev)
      (* handler automaticly set_coord for touch here thank to reset *)
      (fun ev _ -> handler line ev)
      (fun ev -> handler line ev));

    (* Handle preview *)
    let x, y, old_size = ref 0., ref 0., ref 0. in
    let preview ev _ =
      let coord = Client_event_tools.get_coord ev in
      let (color, new_size, oldv, v) = compute_line (x, y) coord in

      (* remove old point with transparanse *)
      Client_canvas.draw ctx2 !base_size !float_size
        ("rgba(0,0,0,0)", !old_size +. 0.05, oldv, oldv);
      old_size := new_size;

      (* draw new point *)
      Client_canvas.draw ctx2 !base_size !float_size
        (color, new_size, v, v);
      Lwt.return ()
    in
    Lwt_js_events.async (fun () ->
      (Lwt_js_events.mousemoves Dom_html.document preview));

    (* fix drag and drop to avoid to drag canvas during drawing *)
    (* ignore (Client_event_tools.disable_drag_and_drop dom_canvas); *)

    (* fix scroll on smartphone to avoid moving up and down on browsers *)
    ignore (Client_event_tools.disable_mobile_zoom ());

    (* resize and orientationchange listenner *)
    (* handle resize of canvas and redraw image *)
    Lwt.async (fun () ->
      Client_event_tools.limited_onorientationchanges_or_onresizes (fun _ _ ->
        let rc_width, rc_height =
          Client_canvas.init_size body_elt header_elt canvas_elt canvas2_elt
        in
        get_origine_canvas ();
        width := float_of_int rc_width;
        height := float_of_int rc_height;
        float_size := (!width, !height);
        base_size := !height;
        ctx##lineCap <- Js.string "round";
        ctx2##lineCap <- Js.string "round";
        ctx2##globalCompositeOperation <- Js.string "copy";
        Client_canvas.init_image ctx bus_mutex (!width, !height) ));

    (* return value *)
    Lwt.return ()

}}
