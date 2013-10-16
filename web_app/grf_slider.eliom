(* Graffiti
 * http://www.ocsigen.org/graffiti
 * Copyright (C) 2013 Arnaud Parant
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

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

let base_class = "grf_slider_"
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
    | Vertical          -> dragger_vertical_class, slider_vertical_class
    | Horizontal        -> dragger_horizontal_class, slider_horizontal_class
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
    | Vertical          -> 1. -. !value
    | Horizontal        -> !value

let start (slider, dragger, ori, value,
           start_slide, move_slide, end_slide, click) =

  (* get data *)
  let dom_slider = Eliom_content.Html5.To_dom.of_div slider in
  let dom_dragger = Eliom_content.Html5.To_dom.of_div dragger in

  let margin = 4 in
  let slider_width, slider_height =
    ref (dom_slider##clientWidth - margin * 2),
    ref (dom_slider##clientHeight - margin * 2)
  in
  let dragger_width, dragger_height = Ojw_tools.get_size dom_dragger in

  let ox_slider, oy_slider =
    let ox, oy = Dom_html.elementClientPosition dom_slider in
    ref ox, ref oy
  in
  let max_width = ref (float_of_int (!slider_width - dragger_width)) in
  let max_height = ref (float_of_int (!slider_height - dragger_height)) in

  (* tools *)
  let set_value v =
    if (v < 0.) then value := 0.
    else if (v > 1.) then value := 1.
    else value := v
  in

  let x_of_value () = int_of_float (!max_width *. !value) in
  let value_of_x x = set_value ((float_of_int x) /. !max_width) in

  let y_of_value () = int_of_float (!max_height *. !value) in
  let value_of_y y = set_value ((float_of_int y) /. !max_height) in

  let set_dragger_position () = match ori with
    | Vertical          -> dom_dragger##style##top <- Js.string
      ((string_of_int (y_of_value () + margin)) ^ "px")
    | Horizontal        -> dom_dragger##style##left <- Js.string
      ((string_of_int (x_of_value () + margin)) ^ "px")
  in

  let set_coord coord =
    let _ = match ori with
      | Vertical        -> value_of_y ((snd coord) - (dragger_height / 2))
      | Horizontal      -> value_of_x ((fst coord) - (dragger_width / 2))
    in set_dragger_position ()
  in

  let launch_callback = function
    | Some func -> Lwt.async func
    | _         -> ()
  in

  (* initialize dragger position *)
  let _ =
    set_dragger_position ();
    launch_callback !click
  in

  (* move actions *)
  let handle_one_event callback ev =
    let coord =
      let x, y =  Ojw_slide_event.get_slide_coord 0 ev in
      let x', y' = x - !ox_slider, y - !oy_slider in
      x' - margin, y' - margin
    in
    set_coord coord;
    Lwt.return (launch_callback callback)
  in

  Lwt.async (fun () -> Ojw_slide_event.touch_or_mouse_slides dom_dragger
    (fun ev _ -> handle_one_event !start_slide ev)
    (fun ev _ -> handle_one_event !move_slide ev)
    (handle_one_event !end_slide));

  (* click action *)
  Lwt.async (fun () -> Lwt_js_events.clicks dom_slider (fun ev _ ->
    let x, y =
      let x, y = Ojw_event_tools.get_mouse_ev_coord ev in
      let x', y' = x - !ox_slider, y - !oy_slider in
      x' - margin, y' - margin
    in
    let _ = match ori with
      | Vertical        -> value_of_y (y - (dragger_height / 2))
      | Horizontal      -> value_of_x (x - (dragger_width / 2))
    in
    set_dragger_position ();
    Lwt.return (launch_callback !click) ));

  (* resize event *)
  Lwt.async (fun () ->
    Lwt_js_events.limited_onorientationchanges_or_onresizes (fun _ _ ->
      slider_width := (dom_slider##clientWidth - margin * 2);
      slider_height := (dom_slider##clientHeight - margin * 2);
      max_width := (float_of_int (!slider_width - dragger_width));
      max_height := (float_of_int (!slider_height - dragger_height));
      let ox, oy = Dom_html.elementClientPosition dom_slider in
      ox_slider := ox;
      oy_slider := oy;
      Lwt.return (set_dragger_position ()) ))

}}
