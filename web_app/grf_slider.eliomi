
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

val start : t -> unit

}}
