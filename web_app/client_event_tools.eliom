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

{client{

open Lwt

  (* Enable / disable *)

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
    let last_time = ref (Ojw_tools.get_timestamp ()) in
    let last_coord = ref (0, 0) in
    Lwt.async (fun () ->
      source_event ?use_capture:(Some true) target (fun ev _ ->
        last_time := Ojw_tools.get_timestamp ();
        last_coord := Ojw_event_tools.get_touch_ev_coord 0 ev;
        Lwt.return (Dom.preventDefault ev)));
    Lwt.async (fun () ->
      prevented_event ?use_capture:(Some true) target (fun ev _ ->
        let time = Ojw_tools.get_timestamp () in
        let coord = Ojw_event_tools.get_mouse_ev_coord ev in
        (* check if it is at same coord and fired less than 350ms after touch *)
        if (Ojw_event_tools.cmp_coord !last_coord coord &&
	      (time -. 0.35) <= !last_time)
        then begin Dom.preventDefault ev; Dom_html.stopPropagation ev end;
        Lwt.return () ))

  let disable_ghost_mousedown target =
    preventEvent Lwt_js_events.mousedowns Lwt_js_events.touchstarts target

  let disable_ghost_mousemove target =
    preventEvent Lwt_js_events.mousemoves Lwt_js_events.touchmoves target

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

  let get_slide_coord ?t_type idx ?p_type = function
    | Touch_event ev    ->
      Ojw_event_tools.get_touch_ev_coord ?t_type idx ?p_type ev
    | Mouse_event ev    ->
      Ojw_event_tools.get_mouse_ev_coord ?p_type ev

  let get_local_slide_coord dom_elt ?t_type idx ?p_type = function
    | Touch_event ev    ->
      Ojw_event_tools.get_local_touch_ev_coord dom_elt ?t_type idx ?p_type ev
    | Mouse_event ev    ->
      Ojw_event_tools.get_local_mouse_ev_coord dom_elt ?p_type ev

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

      let width, height = Ojw_tools.get_document_size () in
      let current_x, current_y = Ojw_event_tools.get_mouse_ev_coord ev in
      let start_x' = get_relative_position width start_x in
      let start_y' = get_relative_position height start_y in
      let end_x' = get_relative_position width end_x in
      let end_y' = get_relative_position height end_y in

      if (current_x >= start_x' && current_x <= end_x' &&
            current_y >= start_y' && current_y <= end_y')
      then func ()
      else Lwt.return () )

}}
