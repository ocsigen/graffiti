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

  (*** Tools **)
  let hide_element dom_html = dom_html##style##display <- Js.string "none"
  let show_element dom_html = dom_html##style##display <- Js.string "inline"

  let show_if_hide dom_html =
    match (Js.to_string dom_html##style##display) with
      | "inline"      -> ()
      | _             -> show_element dom_html

  let hide_if_show dom_html =
    match (Js.to_string dom_html##style##display) with
      | "inline"      -> hide_element dom_html
      | _             -> ()

  let set_position body_elt header_elt dom_html margin =
    let width, height = Ow_size.get_document_size () in
    let header_height = Client_header.get_height body_elt header_elt in
    dom_html##style##height <- Js.string
      ((string_of_int (height - header_height - (margin * 2))) ^ "px");
    dom_html##style##top <- Js.string ((string_of_int header_height) ^ "px")

  (** switch simple element display **)
  let switch_display dom_html =
    match (Js.to_string dom_html##style##display) with
      | "inline"      -> hide_element dom_html
      | _             -> show_element dom_html

  (** switch display on fullscreen element with gray layer **)
  let rec switch_fullscreen_display dom_gray_layer dom_html =
    match (Js.to_string dom_html##style##display) with
      | "inline"     ->
        (hide_element dom_gray_layer;
         hide_element dom_html)
      | _            ->
        ((* Catch click / touch event to hide again elements *)
          let catch_hide_event elt = Lwt_js_events.click elt >>= (fun _ ->
            Lwt.return (switch_fullscreen_display dom_gray_layer dom_html))
          in
          ignore (catch_hide_event dom_html);
          ignore (catch_hide_event dom_gray_layer);
          show_element dom_gray_layer;
          show_element dom_html)

}}
