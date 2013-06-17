
{shared{

type t
type div = [ Html5_types.div ] Eliom_content.Html5.D.elt

(** The agrument is the divisor of 255
*** It have to  be greater than 1 **)
val genere_lll_color : int -> string list list list

(* Some pre-genereated lll_color in several precision *)
val lll_color_p2 : string list list list
val lll_color_p3 : string list list list
val lll_color_p4 : string list list list
val lll_color_p5 : string list list list
val lll_color_p6 : string list list list

(* Some hand-mained lll_color *)
val lll_color_6 : string list list list (* 1 table 2 columns 5 lines *)

(**
*** Take one list (tables) of list (columns) of list (lines) of color (string)
*** and build table list with it.
*** By default this list is initialised with lll_color_p5
***
*** It return
*** - t to future action,
*** - color_div, to display current select color,
***     it is not mandatory to include it in page
*** - and the block with all color square content in the genered table **)
val create :
  ?initial_color: string ->
  ?lll_color: string list list list ->
  unit ->
  (t * div * div)

(* css list to include in page *)
val css_list : string list list

}}

{client{

val start : t -> unit

val get_color: t -> string

}}
