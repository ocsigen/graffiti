
{client{

  (* size and orientation *)

  type orientation = Portrait | Landscape

  let get_screen_size () =
    let scr = Dom_html.window##screen in
    scr##width, scr##height

  let get_screen_orientation () =
    let width, height = get_screen_size () in
    if (width <= height) then Portrait else Landscape

  let get_size dom_html =
    dom_html##clientWidth, dom_html##clientHeight

  let get_document_size () =
    get_size Dom_html.document##documentElement

  (* time *)

  let get_timestamp () =
    let date = jsnew Js.date_now () in
    Js.to_float (date##getTime ())

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
