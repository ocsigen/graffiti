
{client{

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
    let date = jsnew Js.date_now () in
    Js.to_float (date##getTime ())

  (* position / coordinated *)

  let get_coord ev = ev##clientX, ev##clientY

  let get_touch_coord idx event =
    let ev = event##touches##item(idx) in
    Js.Optdef.case ev (fun () -> (0, 0)) get_coord

  let get_local_event_coord dom_elt ev =
    let ox, oy = Dom_html.elementClientPosition dom_elt in
    let x, y =  get_coord ev in
    x - ox, y - oy

  let get_local_touch_event_coord dom_elt idx ev =
    let ox, oy = Dom_html.elementClientPosition dom_elt in
    let x, y =  get_touch_coord idx ev in
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

  (* others *)

  let js_string_of_px px = Js.string (string_of_int px ^ "px")

  (* css tools *)

  let set_transition dom_elt v =
    (Js.Unsafe.coerce (dom_elt##style))##transition <- Js.string v;
    ignore (Js.Unsafe.set (dom_elt##style)
              (Js.string "-webkit-transition") (Js.string v));


}}
