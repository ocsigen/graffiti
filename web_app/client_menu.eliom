
{client{

  (** Handle client menu action **)
  let start ()  =

    (*** Menu's tools ***)
    let dom_gray_layer =
      Eliom_content.Html5.To_dom.of_div %Server_html.gray_layer_elt
    in
    let dom_header = Eliom_content.Html5.To_dom.of_table %Server_html.header_elt
    in

    let hide_element dom_html =
      dom_html##style##visibility <- Js.string "hidden"
    in
    let show_element dom_html =
      dom_html##style##visibility <- Js.string "visible"
    in

    (** switch simple element visibility **)
    let switch_visibility dom_html =
      match (Js.to_string dom_html##style##visibility) with
        | "visible"     -> hide_element dom_html
        | _             -> show_element dom_html
    in

    (** switch visibility on fullscreen element with gray layer **)
    let rec switch_fullscreen_visibility dom_html =
      match (Js.to_string dom_html##style##visibility) with
        | "visible"    ->
          (hide_element dom_gray_layer;
           hide_element dom_html)
        | _            ->
          ((* Catch click / touch event to hide again elements *)
            let catch_hide_event elt = Client_tools.touch_click elt
              (fun _ -> Lwt.return
                (switch_fullscreen_visibility dom_html))
            in
            (* catch_hide_event dom_html; *)
            catch_hide_event dom_gray_layer;
            show_element dom_gray_layer;
            show_element dom_html)
    in

    let dom_about_option =
      Eliom_content.Html5.To_dom.of_td %Server_html.about_option_elt
    in
    let dom_save_option =
      Eliom_content.Html5.To_dom.of_td %Server_html.save_option_elt
    in
    let dom_button_menu =
      Eliom_content.Html5.To_dom.of_td %Server_html.menu_button_elt
    in
    let dom_menu = Eliom_content.Html5.To_dom.of_div %Server_html.menu_elt in
    let dom_about = Eliom_content.Html5.To_dom.of_div %Server_html.about_elt in
    let dom_canvas =
      Eliom_content.Html5.To_dom.of_canvas %Server_html.canvas_elt
    in

    (* menu tools *)
    let menu_position () =
      dom_menu##style##top <- Js.string
        ((string_of_int dom_header##clientHeight) ^ "px")
    in

    let save_canvas_img () =
      let img = dom_canvas##toDataURL () in
      let rgx = jsnew Js.regExp (Js.string "image/png") in
      let img' = img##replace
        (rgx, (* give a name to download image do not work *)
         (Js.string "image/force-download;filename='graffiti.png'"))
      in

      (* (\* allow to avoid cach image *\) *)
      (* let attr = get_rand () in *)
      (* Eliom_client.exit_to ~service:%medium_imageservice attr () *)

      ignore (Dom_html.window##open_
                (img', Js.string "_self", Js.null))
    in

    (* Add listenner of resize window events *)
    (* on menu button *)
    Client_tools.add_window_resize_function (2, menu_position);

    (* Add listenner of touch click events *)
    (* on menu button *)
    Client_tools.touchs_clicks dom_button_menu
      (fun _ -> Lwt.return
        (menu_position ();
         switch_visibility dom_menu));

    (* on save  *)

    (* save image on mobile not work *)
    (* try js way to do it, but currently desable *)

    (* touchs_clicks dom_save_option *)
    (*   (fun _ -> Lwt.return (save_canvas_img ())); *)

    (* on about  *)
    Client_tools.touchs_clicks dom_about_option
      (fun _ -> Lwt.return (switch_fullscreen_visibility dom_about))

}}
