
{client{

  open Lwt

  (** Handle client menu action **)
  let start ()  =

    (*** Menu's tools ***)
    let dom_gray_layer =
      Eliom_content.Html5.To_dom.of_div %Server_html.gray_layer_elt
    in
    let dom_header = Eliom_content.Html5.To_dom.of_table %Server_html.header_elt
    in
    let dom_menu = Eliom_content.Html5.To_dom.of_div %Server_html.menu_elt in

    let hide_element dom_html =
      dom_html##style##display <- Js.string "none"
    in
    let show_element dom_html =
      dom_html##style##display <- Js.string "inline"
    in

    (** switch simple element display **)
    let switch_display dom_html =
      match (Js.to_string dom_html##style##display) with
        | "inline"      -> hide_element dom_html
        | _             -> show_element dom_html
    in

    (** switch display on fullscreen element with gray layer **)
    let rec switch_fullscreen_display dom_html =
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
    in

    let menu_position () =
      let margin = 3 in
      dom_menu##style##top <- Js.string
        ((string_of_int dom_header##clientHeight) ^ "px");
      dom_menu##style##height <- Js.string
        ((string_of_int (Dom_html.document##documentElement##clientHeight -
                           dom_header##clientHeight - margin * 2)) ^ "px")
    in

    (*** Elements ***)

    let dom_about_option =
      Eliom_content.Html5.To_dom.of_td %Server_html.about_option_elt
    in
    let dom_button_menu =
      Eliom_content.Html5.To_dom.of_td %Server_html.menu_button_elt
    in
    let dom_about = Eliom_content.Html5.To_dom.of_div %Server_html.about_elt in

    (* Add listenner of resize window events *)
    (* on menu button *)
    Client_tools.add_window_resize_function (2, menu_position);

    (* Add listenner of touch click events *)
    (* on menu button *)
    Lwt.async (fun () -> Lwt_js_events.clicks dom_button_menu
      (fun _ _ -> Lwt.return
        (menu_position ();
         switch_display dom_menu)));

    (* on about  *)
    Lwt.async (fun () -> Lwt_js_events.clicks dom_about_option
      (fun _ _ -> Lwt.return (switch_fullscreen_display dom_about)))

}}
