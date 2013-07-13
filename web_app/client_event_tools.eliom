
{client{

open Lwt

  (* position / coordinated *)

  let get_coord ev = ev##clientX, ev##clientY

  type touch_type = All_touches | Target_touches | Changed_touches

  let get_touch_coord ?(typ=All_touches) idx event =
    let item = match typ with
      | All_touches     -> event##touches##item(idx)
      | Target_touches  -> event##targetTouches##item(idx)
      | Changed_touches -> event##changedTouches##item(idx)
    in
    Js.Optdef.case item (fun () -> (0, 0)) get_coord

  (* A little tool to call get_touch_coord with option value *)
  let get_touch_coord_with_opt = function
    | Some v    -> (get_touch_coord ~typ:v)
    | None      -> (get_touch_coord ~typ:All_touches)

  let get_local_event_coord dom_elt ev =
    let ox, oy = Dom_html.elementClientPosition dom_elt in
    let x, y =  get_coord ev in
    x - ox, y - oy

  let get_local_touch_event_coord ?typ dom_elt idx ev =
    let ox, oy = Dom_html.elementClientPosition dom_elt in
    let x, y =  (get_touch_coord_with_opt typ) idx ev in
    x - ox, y - oy

  let cmp_coord (x1, y1) (x2, y2) = x1 = x2 && y1 = y2

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

  let disable_mobile_zoom () =
    disable_event Dom_html.Event.touchmove Dom_html.document

  let preventEvent
      (prevented_event:
         (?cancel_handler:bool ->
          ?use_capture: bool ->
          (#Dom_html.eventTarget as 'a) Js.t ->
          (Dom_html.mouseEvent Js.t -> unit Lwt.t -> unit Lwt.t) ->
          unit Lwt.t))
      (source_event:
         (?cancel_handler:bool ->
          ?use_capture: bool ->
          (#Dom_html.eventTarget as 'a) Js.t ->
          (Dom_html.touchEvent Js.t -> unit Lwt.t -> unit Lwt.t) ->
          unit Lwt.t))
      target =
    let last_time = ref (Client_js_tools.get_timestamp ()) in
    let last_coord = ref (0, 0) in
    Lwt.async (fun () ->
      source_event ?use_capture:(Some true) target (fun ev _ ->
        last_time := Client_js_tools.get_timestamp ();
        last_coord := get_touch_coord 0 ev;
        Lwt.return (Dom.preventDefault ev)));
    Lwt.async (fun () ->
      prevented_event ?use_capture:(Some true) target (fun ev _ ->
        let time = Client_js_tools.get_timestamp () in
        let coord = get_coord ev in
        (* check if it is at same coord and fired less than 100ms after touch *)
        if (cmp_coord !last_coord coord && (time -. 0.1) <= !last_time)
        then begin Dom.preventDefault ev; Dom_html.stopPropagation ev end;
        Lwt.return () ))

  let disable_ghost_mousedown target =
    preventEvent Lwt_js_events.mousedowns Lwt_js_events.touchstarts target

  let disable_ghost_mousemove target =
    begin
      preventEvent Lwt_js_events.mousemoves Lwt_js_events.touchstarts target;
      preventEvent Lwt_js_events.mousemoves Lwt_js_events.touchmoves target
    end

  let disable_ghost_mouseup target =
    preventEvent Lwt_js_events.mouseups Lwt_js_events.touchends target

  let disable_ghost_mouse_event target =
    begin
      disable_ghost_mousedown target;
      disable_ghost_mousemove target;
      disable_ghost_mouseup target
    end

  (* orientation / resize *)

  let orientationchange = Dom_html.Event.make "orientationchange"

  let onorientationchange () =
    Lwt_js_events.make_event orientationchange Dom_html.document

  let onorientationchange_or_onresize () =
    Lwt.pick [Lwt_js_events.onresize (); onorientationchange ()]

  let onorientationchanges t =
    Lwt_js_events.seq_loop
      (fun ?use_capture () -> onorientationchange ()) () t

  let onorientationchanges_or_onresizes t =
    Lwt_js_events.seq_loop
      (fun ?use_capture () -> onorientationchange_or_onresize ()) () t

  (* limited *)

  let func_limited_loop event limited_func ?use_capture target handler =
    (* Could probably be optimized!! Check! *)
    let count = ref 0 in
    Lwt_js_events.async_loop event ?use_capture target
      (fun ev lt -> incr count;
        let nb = !count in
        lwt _ = limited_func () in
        if (!count = nb)
        then handler ev lt
        else Lwt.return ())

  let limited_loop event ?(elapsed_time=0.1) =
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
    Lwt.pick [move_events Dom_html.document moves_func;
              end_event Dom_html.document >>= end_func]

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

  let mouseslide_without_start =
    slide_without_start Lwt_js_events.mousemoves Lwt_js_events.mouseup

  let mouseslide (dom_elt: #Dom_html.eventTarget Js.t) =
    slide_event Lwt_js_events.mousedown mouseslide_without_start dom_elt

  let mouseslides (dom_elt: #Dom_html.eventTarget Js.t) =
    slide_events Lwt_js_events.mousedowns mouseslide_without_start dom_elt

  let touchslide_without_start =
    slide_without_start Lwt_js_events.touchmoves Lwt_js_events.touchend

  let touchslide (dom_elt: #Dom_html.eventTarget Js.t) =
    slide_event Lwt_js_events.touchstart touchslide_without_start dom_elt

  let touchslides (dom_elt: #Dom_html.eventTarget Js.t) =
    slide_events Lwt_js_events.touchstarts touchslide_without_start dom_elt

  type slide_event =
      Touch_event of Dom_html.touchEvent Js.t
    | Mouse_event of Dom_html.mouseEvent Js.t

  let get_slide_coord ?typ idx = function
    | Touch_event ev    -> (get_touch_coord_with_opt typ) idx ev
    | Mouse_event ev    -> get_coord ev

  let get_local_slide_coord ?typ dom_elt idx = function
    | Touch_event ev    ->
      let touch_func = match typ with
        | Some v        -> (get_local_touch_event_coord ~typ:v)
        | None          -> (get_local_touch_event_coord ~typ:All_touches)
      in touch_func dom_elt idx ev
    | Mouse_event ev    -> get_local_event_coord dom_elt ev

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

    Lwt_js_events.async_loop
      (fun ?use_capture () -> touch_or_mouse_start dom_elt) ()
      (fun ev lt ->
        lwt _ = starts_func ev lt in
        touch_or_mouse_without_start ev moves_func end_func)

  (* click *)

  type lc_position =
    | Value of int
    | Max_value of int

  let detect_local_clicks (start_x, end_x, start_y, end_y) func =

    let get_relative_position max = function
      | Value v         -> v
      | Max_value v     -> max + v
    in

    Lwt_js_events.clicks Dom_html.document (fun ev _ ->

      let width, height = Client_js_tools.get_document_size () in
      let current_x, current_y = get_coord ev in
      let start_x' = get_relative_position width start_x in
      let start_y' = get_relative_position height start_y in
      let end_x' = get_relative_position width end_x in
      let end_y' = get_relative_position height end_y in

      if (current_x >= start_x' && current_x <= end_x' &&
            current_y >= start_y' && current_y <= end_y')
      then func ()
      else Lwt.return () )

}}
