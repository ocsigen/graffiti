
{shared{

open Lwt
open Eliom_content.Html5
open Eliom_content.Html5.F

type orientation_t = Vertical | Horizontal

type callback = unit -> unit Lwt.t
type div = [ Html5_types.div ] Eliom_content.Html5.D.elt
type t = (div * div * orientation_t * float ref *
	    callback option ref * callback option ref *
	    callback option ref * callback option ref)

let base_class = "GrfSlider_"
let slider_class = base_class ^ "slider"
let slider_vertical_class = slider_class ^ "_vertical"
let slider_horizontal_class = slider_class ^ "_horizontal"
let dragger_class = base_class ^ "dragger"
let dragger_vertical_class = dragger_class ^ "_vertical"
let dragger_horizontal_class = dragger_class ^ "_horizontal"

let create ?(orientation = Horizontal)
    ?start_slide ?move_slide ?end_slide ?click
    ?(initial_value = 0.5) () =
  if (initial_value < 0. || initial_value > 1.)
  then failwith "Grf_slider.create initial_value have to be between 0. and 1.";
  let dragger_ori_class, slider_ori_class = match orientation with
    | Vertical		-> dragger_vertical_class, slider_vertical_class
    | Horizontal	-> dragger_horizontal_class, slider_horizontal_class
  in
  let dragger = D.div ~a:[a_class[dragger_class; dragger_ori_class]] [] in
  let slider = D.div ~a:[a_class[slider_class; slider_ori_class]] [dragger] in
  let type_t =
    slider, dragger, orientation, ref initial_value,
    ref start_slide, ref move_slide, ref end_slide, ref click
  in
  type_t, slider

}}

{client{

let change_start_slide_callback (_, _, _, _, start_callback, _, _, _) new_callback =
  start_callback := Some new_callback

let remove_start_slide_callback (_, _, _, _, start_callback, _, _, _) =
  start_callback := None

let change_move_slide_callback (_, _, _, _, _, move_callback, _, _) new_callback =
  move_callback := Some new_callback

let remove_move_slide_callback (_, _, _, _, _, move_callback, _, _) =
  move_callback := None

let change_end_slide_callback (_, _, _, _, _, _, end_callback, _) new_callback =
  end_callback := Some new_callback

let remove_end_slide_callback (_, _, _, _, _, _, end_callback, _) =
  end_callback := None

let change_click_callback (_, _, _, _, _, _, _, click_callback) new_callback =
  click_callback := Some new_callback

let remove_click_callback (_, _, _, _, _, _, _, click_callback) =
  click_callback := None

let get_value (_, _, ori, value, _, _, _, _) = match ori with
    | Vertical		-> 1. -. !value
    | Horizontal	-> !value

let start (slider, dragger, ori, value,
	   start_slide, move_slide, end_slide, click) =

  (* get data *)
  let dom_slider = Eliom_content.Html5.To_dom.of_div slider in
  let dom_dragger = Eliom_content.Html5.To_dom.of_div dragger in
  let o_slider_x, o_slider_y = Dom_html.elementClientPosition dom_slider in
  let slider_width, slider_height =
    dom_slider##clientWidth, dom_slider##clientHeight
  in
  let dragger_width, dragger_height =
    dom_dragger##clientWidth, dom_dragger##clientHeight
  in
  let max_width = float_of_int (slider_width - dragger_width) in
  let max_height = float_of_int (slider_height - dragger_height) in

  (* tools *)
  let last_coord = ref (0, 0) in
  let save_coord ev = last_coord := (ev##clientX, ev##clientY) in
  let get_x (x, _) = x in
  let get_y (_, y) = y in
  let diff_coord ev =
    ev##clientX - get_x !last_coord, ev##clientY - get_y !last_coord
  in

  let set_value v =
    if (v < 0.) then (value := 0. ; false)
    else if (v > 1.) then (value := 1. ; false)
    else (value := v ; true)
  in

  let x_value = ref 0 in
  let x_of_value () =
    x_value := int_of_float (max_width *. !value); !x_value
  in
  let value_of_x x = set_value ((float_of_int x) /. max_width) in

  let y_value = ref 0 in
  let y_of_value () =
    y_value := int_of_float (max_height *. !value); !y_value
  in
  let value_of_y y = set_value ((float_of_int y) /. max_height) in

  let set_dragger_position () = match ori with
    | Vertical		-> dom_dragger##style##top <- Js.string
      ((string_of_int (y_of_value ())) ^ "px")
    | Horizontal	-> dom_dragger##style##left <- Js.string
      ((string_of_int (x_of_value ())) ^ "px")
  in

  let set_coord ev =
    let diff_x, diff_y = diff_coord ev in
    let accepted = match ori with
      | Vertical	-> value_of_y (!y_value + diff_y)
      | Horizontal	-> value_of_x (!x_value + diff_x)
    in
    set_dragger_position ();
    if accepted then save_coord ev
  in

  let launch_callback = function
    | Some func	-> Lwt.async func
    | _		-> ()
  in

  (* initialize dragger position *)
  set_dragger_position ();

  (* move actions *)
  Lwt.async (fun () -> Lwt_js_events.mousedowns dom_dragger (fun ev _ ->
    save_coord ev;
    launch_callback !start_slide;
    Lwt.pick [Lwt_js_events.mousemoves Dom_html.document (fun ev _ ->
      set_coord ev; Lwt.return (launch_callback !move_slide));
              Lwt_js_events.mouseup Dom_html.document >>= (fun ev ->
      set_coord ev; Lwt.return (launch_callback !end_slide))]));

  (* click action *)
  Lwt.async (fun () -> Lwt_js_events.clicks dom_slider (fun ev _ ->
    let x, y = ev##clientX, ev##clientY in
    let _ = match ori with
      | Vertical	-> value_of_y (y - o_slider_y - (dragger_height / 2))
      | Horizontal	-> value_of_x (x - o_slider_x - (dragger_width / 2))
    in
    set_dragger_position ();
    Lwt.return (launch_callback !click)
  ))

}}
