
{shared{

open Eliom_content.Html5
open Eliom_content.Html5.D

type div = [ Html5_types.div ] Eliom_content.Html5.D.elt
type t = (string ref * div * div list)

let raise_exception str =
  failwith "Color_picker." ^ str

(** The agrument is the divisor of 255
** It have to  be greater than 1 **)
let genere_lll_color precision =

  let color_list = match precision with
    | p when p <= 1     ->
      ignore (raise_exception
                "genere_list_color, the argument have to be greater than 1");
      []
    | precision         ->
      let step = 255 / (precision - 1) in
      let rec aux_build nl v =
        if (v > 255)
        then nl
        else aux_build ((Printf.sprintf "%02X" v)::nl) (v + step)
      in aux_build [] 0
  in
  let max_iteration = List.length color_list in

  let rec aux_red nl = function
    | n when n >= max_iteration -> nl
    | n                         ->
      let red = List.nth color_list n in

      let rec aux_green nl = function
        | n when n >= max_iteration     -> nl
        | n                             ->
          let green = List.nth color_list n in

          let rec aux_blue nl = function
            | n when n >= max_iteration -> nl
            | n                         ->
              let blue = List.nth color_list n in
              aux_blue (("#" ^ red ^ green ^ blue)::nl) (n + 1)

          in aux_green ((aux_blue [] 0)::nl) (n + 1)

      in aux_red ((aux_green [] 0)::nl) (n + 1)

  in aux_red [] 0


(* Some pre-genereated ll_color in several precision *)
let lll_color_p2 = genere_lll_color 2
let lll_color_p2' = lll_color_p2@[[["#582900"; "#87591A"]]]
let lll_color_p3 = genere_lll_color 3
let lll_color_p4 = genere_lll_color 4
let lll_color_p5 = genere_lll_color 5
let lll_color_p6 = genere_lll_color 6


(**
** Take one list of list of list of color (string) and build table list with it.
** return also div_color_list to allow to launch start script detection
**)
let genere_color_table lll_color =

  let build_color_div color =
    div ~a:[a_class["color_picker_square"];
            a_title color;
            a_style ("background-color: " ^ color ^ ";")]
      []
  in
  let build_td_color color_div =
    F.td ~a:[a_class["color_picker_td"]] [color_div]
  in
  let build_tr_color tds =
    F.tr ~a:[a_class["color_picker_tr"]] tds
  in

  let rec build_table div_color_list tables = function
    | []                -> div_color_list, tables
    | head::tail        ->

      let rec build_column div_color_list trs = function
        | []            -> div_color_list, trs
        | head::tail    ->

          let rec build_line div_color_list tds = function
            | []                -> div_color_list, tds
            | color::tail       ->

              let color_div = build_color_div color in
              build_line
                (color_div::div_color_list)
                ((build_td_color color_div)::tds)
                tail
          in

          let div_color_list', tds = build_line div_color_list [] head in
          build_column
            div_color_list'
            ((build_tr_color tds)::trs)
            tail
      in

      let div_color_list', trs = build_column div_color_list [] head in
      let tbl = F.table ~a:[a_class["color_picker_table"]] (tr []) trs in
      build_table
        div_color_list'
        (tbl::tables)
        tail
  in

  let div_color_list, tables = build_table [] [] lll_color in
  div_color_list, tables

(**
*** Take one list (tables) of list (columns) of list (lines) of color (string)
*** and build table list with it.
***
*** It return t to future action,
*** color_div, to display current select color,
*** and the block with all color square
**)
let create ?(initial_color = "#0000FF") ?(lll_color = lll_color_p5) () =
  let color_ref = ref initial_color in
  let div_color_list, tables = genere_color_table lll_color in
  let color_div = div ~a:[a_class["color_picker_color_div"];
                          a_title !color_ref;
                          a_style ("background-color: " ^ !color_ref ^ ";")] []
  in
  let block = F.div ~a:[a_class["color_picker_block"]] tables in
  let type_t = (color_ref, color_div, div_color_list) in
  type_t, color_div, block

let css_list = [["css"; "color_picker.css"]]

}}

{client{

open Lwt

let start (color_ref, color_div, list) =
  let dom_color_div = Eliom_content.Html5.To_dom.of_div color_div in
  let rec aux = function
    | []                -> ()
    | div_elt::tail     ->
      let dom_div = Eliom_content.Html5.To_dom.of_div div_elt in
      Lwt.async (fun () ->
        Lwt_js_events.clicks dom_div (fun _ _ ->
          Lwt.return
            (let color = dom_div##title in
             dom_color_div##style##backgroundColor <- color;
             dom_color_div##title <- color;
             color_ref := (Js.to_string color))));
      aux tail
  in aux list

let get_color (color_ref, _ , _) = !color_ref

}}
