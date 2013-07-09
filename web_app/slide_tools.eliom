
{client{

  type languet_orientation = Lg_left | Lg_right | Lg_up | Lg_down
  type languet_mode = Lg_offset | Lg_width_height

  let languet (target: (#Dom_html.element as 'a) Js.t)
      ?(elt:('a Js.t option)=None)
      orientation
      ?(mode=Lg_offset)
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
      old_coord := Client_event_tools.get_local_slide_coord dom_elt 0 ev
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
      | Lg_offset       -> (match orientation with
          | Lg_left         -> dom_elt##offsetLeft
          | Lg_right        -> Dom_html.document##documentElement##clientWidth -
            dom_elt##offsetLeft - dom_elt##offsetWidth
          | Lg_down         -> Dom_html.document##documentElement##clientHeight -
            dom_elt##offsetTop - dom_elt##offsetHeight
          | Lg_up           -> dom_elt##offsetTop)

      | Lg_width_height -> (match orientation with
          | Lg_left         -> dom_elt##clientWidth
          | Lg_right        -> dom_elt##clientWidth
          | Lg_down         -> dom_elt##clientHeight
          | Lg_up           -> dom_elt##clientHeight)
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
      | Lg_offset       -> (match orientation with
          | Lg_left         -> dom_elt##style##left <- v
          | Lg_right        -> dom_elt##style##right <- v
          | Lg_down         -> dom_elt##style##bottom <- v
          | Lg_up           -> dom_elt##style##top <- v)
      | Lg_width_height -> (match orientation with
          | Lg_left         -> dom_elt##style##width <- v
          | Lg_right        -> dom_elt##style##width <- v
          | Lg_down         -> dom_elt##style##height <- v
          | Lg_up           -> dom_elt##style##height <- v)
    in

     Client_event_tools.touch_or_mouse_slides target
        (fun ev _ ->
          save_coord ev;
          move := 0;
          last_diff := 0;
	  Client_js_tools.set_transition dom_elt "0s";
          launch_callback start_callback)
        (fun ev _ ->
          let new_coord =
	    Client_event_tools.get_local_slide_coord dom_elt 0 ev
	  in
          let diff_x, diff_y =
            (fst new_coord) - (fst !old_coord),
            (snd new_coord) - (snd !old_coord)
          in
          let diff = match orientation with
            | Lg_left   -> diff_x
            | Lg_right  -> -diff_x
            | Lg_down   -> -diff_y
            | Lg_up     -> diff_y
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
