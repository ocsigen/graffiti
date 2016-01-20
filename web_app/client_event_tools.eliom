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

[%%client

open Lwt

  (* Enable / disable *)

  let disable_mobile_zoom () =
    Ow_event_tools.disable_event Dom_html.Event.touchmove Dom_html.document

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
    let last_time = ref (Ow_tools.get_timestamp ()) in
    let last_coord = ref (0, 0) in
    Lwt.async (fun () ->
      source_event ?use_capture:(Some true) target (fun ev _ ->
        last_time := Ow_tools.get_timestamp ();
        last_coord := Ow_event_tools.get_touch_ev_coord 0 ev;
        Lwt.return (Dom.preventDefault ev)));
    Lwt.async (fun () ->
      prevented_event ?use_capture:(Some true) target (fun ev _ ->
        let time = Ow_tools.get_timestamp () in
        let coord = Ow_event_tools.get_mouse_ev_coord ev in
        (* check if it is at same coord and fired less than 350ms after touch *)
        if (Ow_event_tools.cmp_coord !last_coord coord &&
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

      let width, height = Ow_size.get_document_size () in
      let current_x, current_y = Ow_event_tools.get_mouse_ev_coord ev in
      let start_x' = get_relative_position width start_x in
      let start_y' = get_relative_position height start_y in
      let end_x' = get_relative_position width end_x in
      let end_y' = get_relative_position height end_y in

      if (current_x >= start_x' && current_x <= end_x' &&
            current_y >= start_y' && current_y <= end_y')
      then func ()
      else Lwt.return () )

]
