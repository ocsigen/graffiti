
{client{

  open Lwt

  (*** General tools ***)

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


  (*** Smartphone's tool events ***)

  (** Disable Dom_html.Event with stopping propagation during capture phase **)
  let disable_event event html_elt =
    Dom_html.addEventListener html_elt event
      (Dom.handler (fun _ -> Js._false)) Js._true

  (** Enable Dom_html.Event with id gived by disable_event **)
  let enable_event id =
    Dom_html.removeEventListener id

  let enable_events ids =
    let rec enable = function
      | id::t   -> Dom_html.removeEventListener id; enable t
      | []      -> ()
    in enable ids

  (* drag events *)
  let disable_drag_and_drop html_elt =
    [disable_event Dom_html.Event.drag html_elt;
     disable_event Dom_html.Event.dragstart html_elt;
     disable_event Dom_html.Event.dragenter html_elt;
     disable_event Dom_html.Event.drop html_elt]

  (* mobile scroll events *)
  let disable_mobile_scroll () =
    disable_event Dom_html.Event.touchmove Dom_html.document

  (** Execute handler with last event from events' queue
  *** separate by the maximum time gived by elapsed_time
  ***
  *** Be careful, it is a asynchrone loop, so if you give too little time,
  *** several instances of your handler could be run in same time **)
  let limited_loop event ?use_capture ?(elapsed_time=0.2) target handler =
    let count = ref 0 in
    Lwt_js_events.async_loop event ?use_capture target
      (fun ev _ -> incr count;
        let current_nb = !count in
        lwt _ = Lwt_js.sleep elapsed_time in
        if (!count = current_nb) then handler ev () else Lwt.return ())

  (*** window orientationchange and resize ***)
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

}}
