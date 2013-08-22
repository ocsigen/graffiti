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

type orientation_t = Vertical | Horizontal
type callback = unit -> unit Lwt.t
type div = [ Html5_types.div ] Eliom_content.Html5.D.elt
type t

(**
   initial_value is at 0.5 by default
   It have to by between 0. and 1.

   return type t to future action
   and one div which is the slider to insert in html *)
val create :
  ?orientation: orientation_t ->
  ?start_slide: callback ->
  ?move_slide: callback ->
  ?end_slide: callback ->
  ?click: callback ->
  ?initial_value: float ->
  unit ->
  t * div

}}

{client{

val change_start_slide_callback : t -> callback -> unit
val remove_start_slide_callback : t -> unit

val change_move_slide_callback : t -> callback -> unit
val remove_move_slide_callback : t -> unit

val change_end_slide_callback : t -> callback -> unit
val remove_end_slide_callback : t -> unit

val change_click_callback : t -> callback -> unit
val remove_click_callback : t -> unit

(** return value between 0. and 1. *)
val get_value : t -> float

(** launch click callback at start with initialize value *)
val start : t -> unit

}}
