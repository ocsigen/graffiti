
{client{

  open Lwt

  (*** Js' tools ***)

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

  let get_timestamp () =
    (let date = jsnew Js.date_now () in
     int_of_float (Js.to_float (date##getTime ())))

  (*** events's tools ***)

  (** Disable Dom_html.Event with stopping propagation during capture phase **)
  let disable_event event html_elt =
    Dom_html.addEventListener html_elt event
      (Dom.handler (fun _ -> Js._false)) Js._true

  (** Enable Dom_html.Event with id gived by disable_event **)
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

  (** Execute handler with last event from events' queue
  *** separate by the maximum time gived by execute time of limited_func
  ***
  *** Be careful, it is a asynchrone loop, so if you give too little time,
  *** several instances of your handler could be run in same time **)
  let func_limited_loop event limited_func ?use_capture target handler =
    let count = ref 0 in
    Lwt_js_events.async_loop event ?use_capture target
      (fun ev lt -> incr count;
        let nb = !count in
        lwt _ = limited_func () in
        if (!count = nb)
        then handler ev lt
        else Lwt.return ())

  (** Same as func_limited_loop but take time instead of function
  *** By default elapsed_time = 0.2s **)
  let limited_loop event ?(elapsed_time=0.2) =
    func_limited_loop event (fun () -> Lwt_js.sleep elapsed_time)

  let orientationchange = Dom_html.Event.make "orientationchange"

  let onorientationchange () =
    Lwt_js_events.make_event orientationchange Dom_html.document

  let onorientationchanges t =
    Lwt_js_events.seq_loop
      (fun ?use_capture () -> onorientationchange ()) () t

  let limited_onresizes ?elapsed_time t =
    limited_loop
      (fun ?use_capture () -> Lwt_js_events.onresize ()) ?elapsed_time () t

  let limited_onorientationchanges ?elapsed_time t =
    limited_loop
      (fun ?use_capture () -> onorientationchange ()) ?elapsed_time () t

  let onorientationchange_or_onresize () =
    Lwt.pick [Lwt_js_events.onresize (); onorientationchange ()]

  let onorientationchanges_or_onresizes t =
    Lwt_js_events.seq_loop
      (fun ?use_capture () -> onorientationchange_or_onresize ()) () t

  let limited_onorientationchanges_or_onresizes ?elapsed_time t =
    limited_loop (fun ?use_capture () -> onorientationchange_or_onresize ())
      ?elapsed_time () t

  (** local click position type **)
  type lc_position =
    | Value of int      (** Simple value **)
    | Max_value of int  (** Max value + your value **)

  (** Detect click beetween start_x, end_x, start_y and end_y and launch func
  *** Use Max_value constructor to make value relative to screen size **)
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

  let get_slider_value slider = (Grf_slider.get_value slider) /. 5.

}}
