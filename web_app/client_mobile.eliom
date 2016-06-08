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

[%%client

  open Lwt

  let already_removed = ref false

  (* Handheld screen *)
  let has_small_screen () =
    let width, height = Ot_size.get_screen_size () in
    width <= 768

  (* Tablet screen *)
  let has_medium_screen_or_less () =
    let width, height = Ot_size.get_screen_size () in
    width <= 980

  let launch_on_small func =
    if has_small_screen ()
    then func ()
    else ()

  let not_launch_on_small func =
    if has_small_screen ()
    then ()
    else func ()

  let launch_on_small_medium func =
    if has_medium_screen_or_less ()
    then func ()
    else ()

  let not_launch_on_small_medium func =
    if has_medium_screen_or_less ()
    then ()
    else func ()

  (** remove header on small screen
  *** and return true if it removed **)
  let remove_header body_elt header_elt =
    if !already_removed
    then true
    else (

      (* Remove header *)
      let medium_screen () =
        Eliom_content.Html.Manip.removeChild
          body_elt header_elt;
        already_removed := true;
        !already_removed
      in

      (* Check to let or not header *)
      if has_medium_screen_or_less ()
      then medium_screen ()
      else false
    )

  (** Handle image 'touch to start' apparition **)
  let handle_touch_to_start body_elt starting_logo_elt =

    (*** Tools  ***)
    let dom_statring_logo =
      Eliom_content.Html.To_dom.of_table starting_logo_elt
    in

    let remove_touch_to_start_logo () =
      Eliom_content.Html.Manip.removeChild body_elt starting_logo_elt
    in

    (* Set touch action *)
    let medium_screen () =
      Lwt.async (fun () ->
        Lwt_js_events.click dom_statring_logo >>= (fun _ ->
          Lwt.return (remove_touch_to_start_logo ())))
    in

    let normal_screen () =
      remove_touch_to_start_logo ()
    in

    (* Check to let or not 'touch to start' image *)
    if has_medium_screen_or_less ()
    then medium_screen ()
    else normal_screen ()

]
