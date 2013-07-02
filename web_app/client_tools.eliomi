
{client{

  (*** Js' tools ***)

  (* size and orientation *)

  type orientation = Portrait | Landscape

  val get_window_size : unit -> int * int

  val get_window_orientation : unit -> orientation

  val get_size :
    < clientHeight : < get : int; .. > Js.gen_prop;
      clientWidth : < get : int; .. > Js.gen_prop; .. > Js.t ->
      int * int

  val get_document_size : unit -> int * int

  (* time *)

  val get_timestamp : unit -> int

  (* position / coordinated *)

  val get_coord :
    < clientX : < get : int; .. > Js.gen_prop;
      clientY : < get : int; .. > Js.gen_prop; .. > Js.t ->
      int * int

  (** First arg is the id of touch *)
  val get_touch_coord : int -> Dom_html.touchEvent Js.t -> int * int

  (** First arg is the target *)
  val get_local_event_coord :
    #Dom_html.element Js.t ->
    < clientX : < get : int; .. > Js.gen_prop;
      clientY : < get : int; .. > Js.gen_prop; .. > Js.t ->
    int * int

  (** First arg is the target
      The second is the index of touch *)
  val get_local_touch_event_coord :
    #Dom_html.element Js.t ->
    int ->
    Dom_html.touchEvent Js.t ->
    int * int

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

  (* others *)

  val js_string_of_px : int -> Js.js_string Js.t

  (*** events' tools ***)

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

  val disable_mobile_scroll : unit -> Dom_html.event_listener_id

  (* orientation / resize *)

  val orientationchange : Dom_html.event Js.t Dom_html.Event.typ

  val onorientationchange : unit -> Dom_html.event Js.t Lwt.t
  val onorientationchange_or_onresize : unit -> Dom_html.event Js.t Lwt.t

  val onorientationchanges :
    (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
  val onorientationchanges_or_onresizes :
    (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

  (* limited *)

  (** Execute handler with last event from events' queue
      separate by the maximum time gived by execute time of limited_func

      Be careful, it is a asynchrone loop, so if you give too little time,
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
    unit Lwt.t

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

  (** The first arg is the id for touch event *)
  val get_slide_coord : int -> slide_event -> int * int

  (** The first arg is the target
      The second arg is the id for touch event *)
  val get_local_slide_coord :
    #Dom_html.element Js.t ->
    int ->
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

 (* languet tools *)

  type languet_orientation = Lg_left | Lg_right | Lg_up | Lg_down

 (** Handle left languet

     First arg is the target of event
     Second arg is the element to move
     First int is min value
     Second int is max value *)
  val languet :
    #Dom_html.eventTarget Js.t ->
    #Dom_html.element Js.t ->
    languet_orientation ->
    ?start_callback: (unit -> 'a Lwt.t) ->
    ?move_callback: (unit -> 'b Lwt.t) ->
    ?end_callback: (unit -> 'c Lwt.t) ->
    int ->
    int ->
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
