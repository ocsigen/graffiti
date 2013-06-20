
{client{

  open Lwt

  (*** General tools ***)

  type orientation = Portrait | Landscape

  let get_device_size () =
    let scr = Dom_html.window##screen in
    scr##width, scr##height

  let get_size () =
    let doc = Dom_html.document##documentElement in
    doc##clientWidth, doc##clientHeight

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

  let enable_events l_id =
    let rec enable = function
      | id::t   -> Dom_html.removeEventListener id; enable t
      | []      -> ()
    in enable l_id

  (* drag events *)
  let disable_drag_and_drop html_elt =
    [disable_event Dom_html.Event.drag html_elt;
     disable_event Dom_html.Event.dragstart html_elt;
     disable_event Dom_html.Event.dragenter html_elt;
     disable_event Dom_html.Event.drop html_elt]

  (* mobile scroll events *)
  let disable_mobile_scroll () =
    disable_event Dom_html.Event.touchmove Dom_html.document

  (*** window orientationchange and resize ***)

  let orientationchange = Dom_html.Event.make "orientationchange"

  let onorientationchange () =
    Lwt_js_events.make_event orientationchange Dom_html.document

  let onorientationchanges t =
    Lwt_js_events.seq_loop
      (fun ?use_capture () -> onorientationchange ()) () t

  let onorientationchange_or_onresize () =
    Lwt.pick [Lwt_js_events.onresize (); onorientationchange ()]

  let onorientationchanges_or_onresizes t =
    Lwt_js_events.seq_loop
      (fun ?use_capture () -> onorientationchange_or_onresize ()) () t

}}
