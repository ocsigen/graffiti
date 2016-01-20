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

[%%client.start]

(* Enable / disable *)

val disable_mobile_zoom : unit -> Dom_html.event_listener_id

(** catch touchstarts on target
    and made peventDefault to avoid mouse propagation

    moreover if mousedown is fired
    at same coord and at less then 350ms after:
    a preventDefault and stopPropagation is made on it *)
val disable_ghost_mousedown : #Dom_html.eventTarget Js.t -> unit

val disable_ghost_mousemove : #Dom_html.eventTarget Js.t -> unit

val disable_ghost_mouseup : #Dom_html.eventTarget Js.t -> unit

(** Regroup the over fonctionnality in one *)
val disable_ghost_mouse_event : #Dom_html.eventTarget Js.t -> unit


(* click *)

(** local click position type **)
type lc_position = Value of int | Max_value of int

(** Detect click beetween (start_x, end_x, start_y, end_y) and launch func
    Use Max_value constructor to make value relative to document size
    and Value constructor to make static position **)
val detect_local_clicks :
  (lc_position * lc_position * lc_position * lc_position) ->
  (unit -> unit Lwt.t) ->
  unit Lwt.t
