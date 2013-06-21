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
    let width, height = Client_tools.get_document_size () in
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

  (** local click position type **)
  type lc_position =
    | Value of int      (** Simple value **)
    | Max_value of int  (** Max value + your value **)

  (** Detect click beetween start_x, end_x, start_y and end_y and launch func
  *** Use Max_value constructor to make value relative to screen size **)
  let detect_local_clicks (start_x, end_x, start_y, end_y) func =

    let get_mouse_coord ev = (ev##clientX, ev##clientY) in
    let get_relative_position max = function
      | Value v         -> v
      | Max_value v     -> max + v
    in

    Lwt_js_events.clicks Dom_html.document (fun ev _ ->

      (*** Initi data ***)
      let width, height = Client_tools.get_document_size () in

      let current_x, current_y = get_mouse_coord ev in
      let start_x' = get_relative_position width start_x in
      let start_y' = get_relative_position height start_y in
      let end_x' = get_relative_position width end_x in
      let end_y' = get_relative_position height end_y in

      if (current_x >= start_x' && current_x <= end_x' &&
            current_y >= start_y' && current_y <= end_y')
      then func ()
      else Lwt.return () )

}}
