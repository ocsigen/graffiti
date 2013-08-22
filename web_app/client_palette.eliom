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

  open Lwt

  (** Handle client palette action **)
  let start body_elt canvas_elt palette_div palette_button
      slider color_picker color_div =

    (*** Elements ***)

    let dom_palette = Eliom_content.Html5.To_dom.of_div palette_div in
    let dom_button_palette = Eliom_content.Html5.To_dom.of_table palette_button
    in
    let dom_canvas = Eliom_content.Html5.To_dom.of_canvas canvas_elt in
    let dom_color = Eliom_content.Html5.To_dom.of_div color_div in
    let base_size = ref (float_of_int dom_canvas##clientHeight) in

    (* Elarge color picker on computer *)
    let color_picker' = if (not (Client_mobile.has_small_screen ()))
      then Grf_color_picker.add_square_color color_picker
        Grf_color_picker.lll_color_6
      else color_picker
    in

    let color_square_list =
      Grf_color_picker.get_square_color_div_list color_picker'
    in
    let dom_color_list = List.map
      (fun elt -> Eliom_content.Html5.To_dom.of_div elt) color_square_list
    in
    let nb_square_row = (List.length color_square_list) / 2 in

    (* Add touch slide listenner on small screen *)
    let touch_slide_button () =
      let move = ref false in
      Lwt.async (fun () ->
        Lwt_js_events.clicks dom_button_palette (fun _ _ ->
          Lwt.return
            (if not !move then
                match dom_palette##offsetLeft with
                  | 0   -> dom_palette##style##left <-
                    Client_js_tools.js_string_of_px (-196)
                  | _   -> dom_palette##style##left <- Js.string "0px"
             else () )));
      Lwt.async (fun () ->
        Slide_tools.slide dom_palette
          Slide_tools.Left ~allow_click:false ~move_margin:4
          ~start_callback:(fun () -> Lwt.return (move := false))
          ~move_callback:(fun v -> Lwt.return
	    (if not (v = 0) then move := true))
          (-196) 0)
    in Client_mobile.launch_on_small_medium touch_slide_button;

    (* Add listenner of resize event *)

    (* on color square *)
    (* calcul and resize square color to take the maximum of space *)
    let handle_color_square_resize () =
      let margin = 8 in
      let doc_height = Dom_html.document##documentElement##clientHeight in
      let new_height = (doc_height - (margin * 2)) / nb_square_row in
      let rec aux = function
        | []            -> ()
        | dom_div::tail ->
          dom_div##style##height <- Js.string
            (string_of_int (new_height) ^ "px");
          aux tail
      in aux dom_color_list
    in handle_color_square_resize (); (* To initialize view *)
    Lwt.async (fun () -> Client_event_tools.limited_onorientationchanges_or_onresizes
      (fun _ _ -> Lwt.return (handle_color_square_resize ())));

    (* catch slider move and click *)
    let handler () =
      let brush_size = Js.string (string_of_int
        (int_of_float (
          ((Client_tools.get_slider_value slider) *.
            !base_size))) ^ "px")
      in
      dom_color##style##width <- brush_size;
      dom_color##style##height <- brush_size;
      Lwt.return ()
    in
    Grf_slider.change_move_slide_callback slider handler;
    Grf_slider.change_click_callback slider handler;

    (* Handle recalcul base canvas size *)
    Lwt.async (fun () -> Client_event_tools.limited_onorientationchanges_or_onresizes
      (fun _ _ -> Lwt.return
        (base_size := (float_of_int dom_canvas##clientHeight))));

    (* start slider script *)
    Grf_slider.start slider;

    (* Start color picker stript *)
    Grf_color_picker.start color_picker'

}}
