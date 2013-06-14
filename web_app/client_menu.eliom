
{client{

  open Lwt

  (** Handle client menu action **)
  let start ()  =

    (*** Elements ***)
    let dom_menu = Eliom_content.Html5.To_dom.of_div %Server_html.menu_div in
    let dom_about_option =
      Eliom_content.Html5.To_dom.of_td %Server_html.about_option_elt
    in
    let dom_button_menu =
      Eliom_content.Html5.To_dom.of_td %Server_html.menu_button_elt
    in
    let dom_about = Eliom_content.Html5.To_dom.of_div %Server_html.about_elt in

    (* Add listenner of local click events *)
    (* for menu *)
    let detect_local_click () =
      Client_menu_tools.detect_local_clicks
        (Client_menu_tools.Max_value (-100),    (* start_x *)
         Client_menu_tools.Max_value 0,         (* end_x *)
         Client_menu_tools.Value 0,             (* start_y *)
         Client_menu_tools.Value 100)           (* end_y *)
        (fun () -> Client_menu_tools.show_if_hide dom_menu)
    in

    (* Launch it only on mobile to avoid conflit with button menu *)
    Client_mobile.launch_func_only_on_mobile detect_local_click;

    (* on body *)
    Client_menu_tools.detect_local_clicks
      (Client_menu_tools.Value 0,               (* start_x *)
       Client_menu_tools.Max_value (-100),      (* end_x *)
       Client_menu_tools.Value 0,               (* start_y *)
       Client_menu_tools.Max_value 0)           (* end_y *)
      (fun () -> Client_menu_tools.hide_if_show dom_menu);

    (* Add listenner of touch click events *)
    (* on menu button *)
    Lwt.async (fun () -> Lwt_js_events.clicks dom_button_menu
      (fun _ _ -> Lwt.return
        (Client_menu_tools.switch_display dom_menu)));

    (* on about  *)
    Lwt.async (fun () -> Lwt_js_events.clicks dom_about_option
      (fun _ _ -> Lwt.return
        (Client_menu_tools.switch_fullscreen_display dom_about)))

}}
