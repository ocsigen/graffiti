
{client{

  open Lwt

  (*** Js' tools ***)

  (* size and orientation *)

  type orientation = Portrait | Landscape

  let get_window_size () =
    let scr = Dom_html.window##screen in
    scr##width, scr##height

  let get_window_orientation () =
    let width, height = get_window_size () in
    if (width <= height) then Portrait else Landscape

  let get_size dom_html =
    dom_html##clientWidth, dom_html##clientHeight

  let get_document_size () =
    get_size Dom_html.document##documentElement

  (* time *)

  let get_timestamp () =
    (let date = jsnew Js.date_now () in
     int_of_float (Js.to_float (date##getTime ())))

  (* position / coordinated *)

  let get_coord ev = ev##clientX, ev##clientY

  let get_touch_coord idx event =
    let ev = event##touches##item(idx) in
    Js.Optdef.case ev (fun () -> (0, 0)) get_coord

  let get_local_event_position dom_elt ev =
    let ox, oy = Dom_html.elementClientPosition dom_elt in
    let x, y =  get_coord ev in
    x - ox, y - oy

  (* mobile tools *)

  let progressive_apply ?(elapsed_time=0.001) ?(step=4)
      current target func =
    let direction = if current < target then 1 else -1 in
    let rec aux old =
      if ((direction > 0 && old >= target) ||
          (direction < 0 && old <= target))
      then Lwt.return ()
      else
        (let newv = old + (step * direction) in
         ignore (func newv);
         lwt _ = Lwt_js.sleep elapsed_time in
         aux newv)
    in aux current

  (*** events's tools ***)

  (* Enable / disable *)

  let disable_event event html_elt =
    Dom_html.addEventListener html_elt event
      (Dom.handler (fun _ -> Js._false)) Js._true

  let enable_event id =
    Dom_html.removeEventListener id

  let enable_events ids =
    let rec enable = function
      | id::t   -> enable_event id; enable t
      | []      -> ()
    in enable ids

  let disable_drag_and_drop html_elt =
    [disable_event Dom_html.Event.drag html_elt;
     disable_event Dom_html.Event.dragstart html_elt;
     disable_event Dom_html.Event.dragenter html_elt;
     disable_event Dom_html.Event.drop html_elt]

  let disable_mobile_scroll () =
    disable_event Dom_html.Event.touchmove Dom_html.document

  (* orientation / resize *)

  let orientationchange = Dom_html.Event.make "orientationchange"

  let onorientationchange () =
    Lwt_js_events.make_event orientationchange Dom_html.document

  let onorientationchange_or_onresize () =
    Lwt.pick [Lwt_js_events.onresize (); onorientationchange ()]

  let onorientationchanges t =
    Lwt_js_events.seq_loop ?cancel_handler:(Some true)
      (fun ?use_capture () -> onorientationchange ()) () t

  let onorientationchanges_or_onresizes t =
    Lwt_js_events.seq_loop ?cancel_handler:(Some true)
      (fun ?use_capture () -> onorientationchange_or_onresize ()) () t

  (* limited *)

  let func_limited_loop event limited_func ?use_capture target handler =
    let count = ref 0 in
    Lwt_js_events.async_loop event ?use_capture target
      (fun ev lt -> incr count;
        let nb = !count in
        lwt _ = limited_func () in
        if (!count = nb)
        then handler ev lt
        else Lwt.return ())

  let limited_loop event ?(elapsed_time=0.05) =
    func_limited_loop event (fun () -> Lwt_js.sleep elapsed_time)

  let limited_onresizes ?elapsed_time t =
    limited_loop
      (fun ?use_capture () -> Lwt_js_events.onresize ()) ?elapsed_time () t

  let limited_onorientationchanges ?elapsed_time t =
    limited_loop
      (fun ?use_capture () -> onorientationchange ()) ?elapsed_time () t

  let limited_onorientationchanges_or_onresizes ?elapsed_time t =
    limited_loop (fun ?use_capture () -> onorientationchange_or_onresize ())
      ?elapsed_time () t

  (* slide *)

  let slide_without_start move_events end_event moves_func end_func =
    Lwt.return (Lwt.async (fun () ->
      Lwt.pick [move_events Dom_html.document moves_func;
                end_event Dom_html.document >>= end_func]))

  let slide_event
      (start_event: #Dom_html.eventTarget Js.t -> 'b Lwt.t)
      slide_without_start
      (dom_elt: #Dom_html.eventTarget Js.t)
      start_func moves_func end_func =

      lwt ev = start_event dom_elt in
      lwt _ = start_func ev in
      slide_without_start moves_func end_func

  let slide_events start_events slide_without_start
      dom_elt starts_func moves_func end_func =

      start_events dom_elt (fun ev lt ->
        lwt _ = starts_func ev lt in
        slide_without_start moves_func end_func)

  let mousedowns (dom_elt: #Dom_html.eventTarget Js.t) t =
    Lwt_js_events.seq_loop ?cancel_handler:(Some true)
      (fun ?use_capture () -> Lwt_js_events.mousedown dom_elt) () t

  let mousemoves (dom_elt: #Dom_html.eventTarget Js.t) t =
    Lwt_js_events.seq_loop ?cancel_handler:(Some true)
      (fun ?use_capture () -> Lwt_js_events.mousemove dom_elt) () t

  let mouseslide_without_start =
    slide_without_start mousemoves Lwt_js_events.mouseup

  let mouseslide (dom_elt: #Dom_html.eventTarget Js.t) =
    slide_event Lwt_js_events.mousedown mouseslide_without_start dom_elt

  let mouseslides (dom_elt: #Dom_html.eventTarget Js.t) =
    slide_events mousedowns mouseslide_without_start dom_elt

  let touchstarts (dom_elt: #Dom_html.eventTarget Js.t) t =
    Lwt_js_events.seq_loop ?cancel_handler:(Some true)
      (fun ?use_capture () -> Lwt_js_events.touchstart dom_elt) () t

  let touchmoves (dom_elt: #Dom_html.eventTarget Js.t) t =
    Lwt_js_events.seq_loop ?cancel_handler:(Some true)
      (fun ?use_capture () -> Lwt_js_events.touchmove dom_elt) () t

  let touchslide_without_start =
    slide_without_start touchmoves Lwt_js_events.touchend

  let touchslide (dom_elt: #Dom_html.eventTarget Js.t) =
    slide_event Lwt_js_events.touchstart touchslide_without_start dom_elt

  let touchslides (dom_elt: #Dom_html.eventTarget Js.t) =
    slide_events touchstarts touchslide_without_start dom_elt

  type slide_event =
      Touch_event of Dom_html.touchEvent Js.t
    | Mouse_event of Dom_html.mouseEvent Js.t

  let get_slide_coord idx = function
    | Touch_event ev    -> get_touch_coord idx ev
    | Mouse_event ev    -> get_coord ev

  let touch_handler func ev = func (Touch_event ev)
  let mouse_handler func ev = func (Mouse_event ev)

  let touch_or_mouse_start (dom_elt: #Dom_html.eventTarget Js.t) =
    Lwt.pick [Lwt_js_events.touchstart dom_elt >>= (fun ev ->
                Lwt.return (Touch_event ev));
              Lwt_js_events.mousedown dom_elt >>= (fun ev ->
                Lwt.return (Mouse_event ev))]

  let touch_or_mouse_without_start event moves_func end_func =
    match event with
      | Touch_event _   -> touchslide_without_start
        (touch_handler moves_func) (touch_handler end_func)
      | Mouse_event _   -> mouseslide_without_start
        (mouse_handler moves_func) (mouse_handler end_func)

  let touch_or_mouse_slide (dom_elt: #Dom_html.eventTarget Js.t)
      start_func moves_func end_func =

    lwt event = touch_or_mouse_start dom_elt in
    lwt _ = start_func event in
    touch_or_mouse_without_start event moves_func end_func


  let touch_or_mouse_slides (dom_elt: #Dom_html.eventTarget Js.t)
      starts_func moves_func end_func =

    Lwt_js_events.seq_loop ?cancel_handler:(Some true)
      (fun ?use_capture () -> touch_or_mouse_start dom_elt) ()
      (fun ev lt ->
        lwt _ = starts_func ev lt in
        touch_or_mouse_without_start ev moves_func end_func)


  (* click *)

  type lc_position =
    | Value of int
    | Max_value of int

  let detect_local_clicks (start_x, end_x, start_y, end_y) func =

    let get_mouse_coord ev = (ev##clientX, ev##clientY) in
    let get_relative_position max = function
      | Value v         -> v
      | Max_value v     -> max + v
    in

    Lwt_js_events.clicks Dom_html.document (fun ev _ ->

      let width, height = get_document_size () in
      let current_x, current_y = get_mouse_coord ev in
      let start_x' = get_relative_position width start_x in
      let start_y' = get_relative_position height start_y in
      let end_x' = get_relative_position width end_x in
      let end_y' = get_relative_position height end_y in

      if (current_x >= start_x' && current_x <= end_x' &&
            current_y >= start_y' && current_y <= end_y')
      then func ()
      else Lwt.return () )

}}
