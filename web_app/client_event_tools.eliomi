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

  (* position / coordinated *)

  type position_type = Client | Screen | Page

  (** p_type is at Client by default *)
  val get_mouse_ev_coord :
    ?p_type:position_type ->
    Dom_html.mouseEvent Js.t ->
    int * int

  type touch_type = All_touches | Target_touches | Changed_touches

  (** p_type is at Client by default *)
  val get_touch_coord :
    ?p_type:position_type ->
    Dom_html.touch Js.t ->
    int * int

  (** Based on get_touch_coord

      Int arg is the id of touch

      t_type is at All_touches by default *)
  val get_touch_ev_coord :
    ?t_type:touch_type ->
    int ->
    ?p_type:position_type ->
    Dom_html.touchEvent Js.t -> int * int

  (** Based on get_mouse_ev_coord

      Element arg is the target *)
  val get_local_mouse_ev_coord :
    #Dom_html.element Js.t ->
    ?p_type:position_type ->
    Dom_html.mouseEvent Js.t ->
    int * int

  (** Based on get_touch_ev_coord

      Second arg is the target
      Third is the index of touch *)
  val get_local_touch_ev_coord :
    #Dom_html.element Js.t ->
    ?t_type:touch_type ->
    int ->
    ?p_type:position_type ->
    Dom_html.touchEvent Js.t ->
    int * int

  (** take two coordonne and return true if they are egal *)
  val cmp_coord : (int * int) -> (int * int) -> bool

  (* Enable / disable *)

  (** Disable Dom_html.Event with stopping propagation during capture phase **)
  val disable_event :  'a #Dom.event Js.t Dom_html.Event.typ ->
    #Dom_html.eventTarget Js.t ->
    Dom_html.event_listener_id

  (** Enable Dom_html.Event with id gived by disable_event **)
  val enable_event : Dom_html.event_listener_id -> unit

  val enable_events : Dom_html.event_listener_id list -> unit

  val disable_drag_and_drop : #Dom_html.eventTarget Js.t ->
    Dom_html.event_listener_id list

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

  (* orientation / resize *)

  val orientationchange : Dom_html.event Js.t Dom_html.Event.typ

  val onorientationchange : unit -> Dom_html.event Js.t Lwt.t
  val onorientationchange_or_onresize : unit -> Dom_html.event Js.t Lwt.t

  val onorientationchanges :
    (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
  val onorientationchanges_or_onresizes :
    (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

  (* limited *)

  (** [func_limited_loop e delay_fun target handler] will behave like
      [Lwt_js_events.async_loop e target handler] but it will run [delay_fun]
      first, and execut [handler] only when [delay_fun] is finished and
      no other event occurred in the meantime.

      This allows to limit the number of events catched.

      Be careful, it is an asynchrone loop, so if you give too little time,
      several instances of your handler could be run in same time **)
  val func_limited_loop :
    (?use_capture:bool -> 'a -> 'b Lwt.t) ->
    (unit -> 'a Lwt.t) ->
    ?use_capture:bool ->
    'a -> ('b -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

  (** Same as func_limited_loop but take time instead of function
      By default elapsed_time = 0.1s = 100ms **)
  val limited_loop:
    (?use_capture:bool -> 'a -> 'b Lwt.t) ->
    ?elapsed_time:float ->
    ?use_capture:bool ->
    'a -> ('b -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

  val limited_onresizes : ?elapsed_time:float ->
    (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

  val limited_onorientationchanges : ?elapsed_time:float ->
    (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

  val limited_onorientationchanges_or_onresizes : ?elapsed_time:float ->
    (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

  (* slide *)

  (** First is moves event
      Second is end event
      Third is move_func call at each move event
      Fourth is end event call at end event

      Theses events are catch on body *)
  val slide_without_start :
    (Dom_html.document Js.t ->
     ('a -> 'b Lwt.t -> 'b Lwt.t) -> 'b Lwt.t) ->
    (Dom_html.document Js.t -> 'c Lwt.t) ->
    ('a -> 'b Lwt.t -> 'b Lwt.t) ->
    ('c -> 'b Lwt.t) ->
    'b Lwt.t

  (** First is start event
      Second is function which take move_func and end_func
        (partial slide_without_start)
      Third is html element where catch start event
      Fourth is start_func call at start event
      Fifth is move_func call at each move event
      Sixth is end_func call at end event *)
  val slide_event :
    ((#Dom_html.eventTarget Js.t as 'a) -> 'b Lwt.t) ->
    (('c -> 'd Lwt.t -> 'd Lwt.t) -> ('e -> 'd Lwt.t) -> 'd Lwt.t) ->
    (#Dom_html.eventTarget Js.t as 'a) ->
    ('b -> 'd Lwt.t) ->
    ('c -> 'd Lwt.t -> 'd Lwt.t) ->
    ('e -> 'd Lwt.t) ->
    'd Lwt.t

  (** Same as slide_event but catch all start event instead of only one *)
  val slide_events :
    ((#Dom_html.eventTarget as 'a) Js.t ->
     ('b -> 'c Lwt.t -> 'c Lwt.t) -> 'c Lwt.t) ->
    (('d -> 'c Lwt.t -> 'c Lwt.t) -> ('e -> 'c Lwt.t) -> 'c Lwt.t) ->
    (#Dom_html.eventTarget as 'a) Js.t ->
    ('b -> 'c Lwt.t -> 'c Lwt.t) ->
    ('d -> 'c Lwt.t -> 'c Lwt.t) ->
    ('e -> 'c Lwt.t) ->
    'c Lwt.t

  (** First is html element where catch start event
      Second is start_func call at start event
      Third is move_func call at each move event
      Fourth is end event call at end event *)
  val mouseslide :
    #Dom_html.eventTarget Js.t ->
    (Dom_html.mouseEvent Js.t -> unit Lwt.t) ->
    (Dom_html.mouseEvent Js.t -> unit Lwt.t -> unit Lwt.t) ->
    (Dom_html.mouseEvent Js.t -> unit Lwt.t) ->
    unit Lwt.t

  (** Same as mouseslide but catch all start event instead of only one *)
  val mouseslides :
    #Dom_html.eventTarget Js.t ->
    (Dom_html.mouseEvent Js.t -> unit Lwt.t -> unit Lwt.t) ->
    (Dom_html.mouseEvent Js.t -> unit Lwt.t -> unit Lwt.t) ->
    (Dom_html.mouseEvent Js.t -> unit Lwt.t) ->
    unit Lwt.t

  (** Same as mouseslide but with touchevent *)
  val touchslide :
    #Dom_html.eventTarget Js.t ->
    (Dom_html.touchEvent Js.t -> unit Lwt.t) ->
    (Dom_html.touchEvent Js.t -> unit Lwt.t -> unit Lwt.t) ->
    (Dom_html.touchEvent Js.t -> unit Lwt.t) ->
    unit Lwt.t

  (** Same as mouseslides but with touchevent *)
  val touchslides :
    #Dom_html.eventTarget Js.t ->
    (Dom_html.touchEvent Js.t -> unit Lwt.t -> unit Lwt.t) ->
    (Dom_html.touchEvent Js.t -> unit Lwt.t -> unit Lwt.t) ->
    (Dom_html.touchEvent Js.t -> unit Lwt.t) ->
    unit Lwt.t

  type slide_event =
      Touch_event of Dom_html.touchEvent Js.t
    | Mouse_event of Dom_html.mouseEvent Js.t

  (** Based on get_mouse_ev_coord and get_touch_ev_coord

      First arg is the way to get touch with JS API
      Second arg is the id for touch event

      It get client positions *)
  val get_slide_coord :
    ?t_type:touch_type ->
    int ->
    ?p_type:position_type ->
    slide_event ->
    int * int

  (** Based on get_local_mouse_ev_coord and get_local_touch_ev_coord

      First arg is the way to get touch with JS API
      Second arg is the target
      Third arg is the id for touch event

      It get client positions *)
  val get_local_slide_coord :
    #Dom_html.element Js.t ->
    ?t_type:touch_type ->
    int ->
    ?p_type:position_type ->
    slide_event ->
    int * int

  (** Same as mouseslide or touchslide but handle the both *)
  val touch_or_mouse_slide:
    #Dom_html.eventTarget Js.t ->
    (slide_event -> unit Lwt.t) ->
    (slide_event -> unit Lwt.t -> unit Lwt.t) ->
    (slide_event -> unit Lwt.t) ->
    unit Lwt.t

  (** Same as touch_or_mouse_slide
      but catch all event instead of only the first *)
  val touch_or_mouse_slides:
    #Dom_html.eventTarget Js.t ->
    (slide_event -> unit Lwt.t -> unit Lwt.t) ->
    (slide_event -> unit Lwt.t -> unit Lwt.t) ->
    (slide_event -> unit Lwt.t) ->
    unit Lwt.t

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

}}
