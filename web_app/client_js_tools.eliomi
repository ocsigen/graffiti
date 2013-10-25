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

  (* mobile tools *)

  (** Very usefull function to slide element

      elapsed_time is the time between each call
      step is the value between each call
      current is the start value
      target is the end value
      func is the function to apply at each call *)
  val progressive_apply :
    ?elapsed_time:float ->
    ?step:int ->
    int ->
    int ->
    (int -> 'a) ->
    unit Lwt.t

  val hide_navigation_bar : unit -> unit

  (* others *)

  val js_string_of_px : int -> Js.js_string Js.t

  (* css tools *)

  (** Allow to set transition and -webkit-transition property *)
  val set_transition : #Dom_html.element Js.t -> string -> unit

}}
