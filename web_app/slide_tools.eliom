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

  type orientation = Left | Right | Up | Down
  type mode = Offset | Width_height

  let slide (target: (#Dom_html.element as 'a) Js.t)
      ?(elt:('a Js.t option)=None)
      orientation
      ?(mode=Offset)
      ?(allow_click=true)
      ?(move_margin=0)
      ?start_callback ?move_callback ?end_callback
      min max =

    let move = ref 0 in
    let dom_elt = match elt with
      | None    -> target
      | Some e  -> e
    in
    let last_diff = ref 0 in
    let old_coord = ref (0, 0) in
    let save_coord ev =
      old_coord := Ojw_slide_event.get_local_slide_coord dom_elt 0 ev
    in
    let launch_callback = function
      | Some func       -> func ()
      | _               -> Lwt.return ()
    in
    let launch_callback_with arg = function
      | Some func       -> func arg
      | _               -> Lwt.return ()
    in

    let get_offset () = match mode with
      | Offset          -> (match orientation with
          | Left        -> dom_elt##offsetLeft
          | Right       -> Dom_html.document##documentElement##clientWidth -
            dom_elt##offsetLeft - dom_elt##offsetWidth
          | Down        -> Dom_html.document##documentElement##clientHeight -
            dom_elt##offsetTop - dom_elt##offsetHeight
          | Up          -> dom_elt##offsetTop)

      | Width_height    -> (match orientation with
          | Left        -> dom_elt##clientWidth
          | Right       -> dom_elt##clientWidth
          | Down        -> dom_elt##clientHeight
          | Up          -> dom_elt##clientHeight)
    in

    (** get inverse of current position (min or max) if click is allow
        else give current position (min or max) *)
    let get_inverse_of_current () =
      let margin = min + ((max - min) / 2) in
      if allow_click then (if (get_offset () < margin) then max else min)
      else (if (get_offset () > margin) then max else min)
    in

    let set_last_diff diff = if not (diff = 0) then last_diff := diff in

    let set_v value =
      let v = Client_js_tools.js_string_of_px value in
      match mode with
      | Offset          -> (match orientation with
          | Left        -> dom_elt##style##left <- v
          | Right       -> dom_elt##style##right <- v
          | Down        -> dom_elt##style##bottom <- v
          | Up          -> dom_elt##style##top <- v)
      | Width_height    -> (match orientation with
          | Left        -> dom_elt##style##width <- v
          | Right       -> dom_elt##style##width <- v
          | Down        -> dom_elt##style##height <- v
          | Up          -> dom_elt##style##height <- v)
    in

     Ojw_slide_event.touch_or_mouse_slides target
        (fun ev _ ->
          save_coord ev;
          move := 0;
          last_diff := 0;
	  Client_js_tools.set_transition dom_elt "0s";
          launch_callback start_callback)
        (fun ev _ ->
          let new_coord =
	    Ojw_slide_event.get_local_slide_coord dom_elt 0 ev
	  in
          let diff_x, diff_y =
            (fst new_coord) - (fst !old_coord),
            (snd new_coord) - (snd !old_coord)
          in
          let diff = match orientation with
            | Left   -> diff_x
            | Right  -> -diff_x
            | Down   -> -diff_y
            | Up     -> diff_y
          in
          let old_v = get_offset () in
          let new_v =
            let tmp = old_v + diff in
            if tmp < min then min
            else if tmp > max then max
            else tmp
          in
          set_v new_v;
          set_last_diff diff;
          save_coord ev;
          move := (!move + abs diff);
          launch_callback_with diff move_callback)
        (fun ev ->
          let v =
            if !move <= move_margin then get_inverse_of_current ()
            else if !last_diff > 0 then max else min
          in
	  Client_js_tools.set_transition dom_elt "1s";
          set_v v;
          launch_callback_with v end_callback)

}}
