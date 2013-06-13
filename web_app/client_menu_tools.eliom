{client{

  open Lwt

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

  (** switch simple element display **)
  let switch_display dom_html =
    match (Js.to_string dom_html##style##display) with
      | "inline"      -> hide_element dom_html
      | _             -> show_element dom_html

  (** switch display on fullscreen element with gray layer **)
  let rec switch_fullscreen_display dom_html =
    let dom_gray_layer =
      Eliom_content.Html5.To_dom.of_div %Server_html.gray_layer_elt
    in
    match (Js.to_string dom_html##style##display) with
      | "inline"     ->
        (hide_element dom_gray_layer;
         hide_element dom_html)
      | _            ->
        ((* Catch click / touch event to hide again elements *)
          let catch_hide_event elt = Lwt_js_events.click elt >>= (fun _ ->
            Lwt.return (switch_fullscreen_display dom_html))
          in
          ignore (catch_hide_event dom_html);
          ignore (catch_hide_event dom_gray_layer);
          show_element dom_gray_layer;
          show_element dom_html)

  (** Detect click beetween start_x, end_x, start_y and end_y and launch func **)
  let detect_local_clicks (start_x, end_x, start_y, end_y) func =

    let get_mouse_coord ev = (ev##clientX, ev##clientY) in

    Lwt_js_events.async (fun () ->
      Lwt_js_events.clicks Dom_html.document (fun ev _ ->

        let current_x, current_y = get_mouse_coord ev in

        Lwt.return (if (current_x >= start_x && current_x <= end_x &&
                        current_y >= start_y && current_y <= end_y)
          then func ()
          else () )))


}}
