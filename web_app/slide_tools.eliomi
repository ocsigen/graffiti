
{client{

  type orientation = Left | Right | Up | Down
  type mode = Offset | Width_height

 (** [slide target ?elm orientation ?mode
     ?allow_click ?move_sensibility
      ?start ?move ?end min max]

     [target] is the target element of event
     [elm] is the element to transform, by default it is [target]
     [allow_click] if it is by defaut at true:
        allow to expand or contract when a simple click is fired
     [move_margin] allow to look a little move at a click, default at 0(px)
        If you already ignore click with [allow_click] at false
        and if you take [move_margin] at 1(px) (for example),
        move action which move less or egal at 1px will be ignored

     [move] take current move value in parameter
     [end] take final value in parameter

     [min] is min value
     [max] is max value

     Default mode is Lg_offset

     Lg_offset mode take information from offsetLeft/Top property
     (A calcul is made for right and down position)
     and set on style.left/right/top/button

     Lg_width_height mode take information from clientWidth/Height property
     and set on style.width/height
     Obviously with this mode, oritention left/right - up/down do the same thing

 *)
  val slide :
    (#Dom_html.element as 'a) Js.t ->
    ?elt: ('a Js.t option) ->
    orientation ->
    ?mode: mode ->
    ?allow_click: bool ->
    ?move_margin: int ->
    ?start_callback: (unit -> unit Lwt.t) ->
    ?move_callback: (int -> unit Lwt.t) ->
    ?end_callback: (int -> unit Lwt.t) ->
    int ->
    int ->
    unit Lwt.t

}}
