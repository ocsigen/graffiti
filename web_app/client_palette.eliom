
{client{

  open Lwt

  (** Handle client palette action **)
  let start ()  =

    (*** Initi data ***)

    let size = Client_tools.get_size () in
    let width = fst size and height = snd size in

    (*** Elements ***)

    let dom_palette = Eliom_content.Html5.To_dom.of_div %Server_html.palette_div
    in

    let dom_button_palette =
      Eliom_content.Html5.To_dom.of_td %Server_html.palette_button_elt
    in

    (* Add listenner of touch slide events *)
    (* for palette menu *)
    let detect_local_click () =
      Client_menu_tools.detect_local_clicks (0, 100, 0, 100)
        (fun () -> Client_menu_tools.show_if_hide dom_palette)
    in

    (* Launch it only on mobile to avoid conflit with button menu *)
    Client_mobile.launch_func_only_on_mobile detect_local_click;

    Client_menu_tools.detect_local_clicks (100, width, 0, height)
      (fun () -> Client_menu_tools.hide_if_show dom_palette);

    (* Add listenner of touch click events *)
    (* on palette button *)
    Lwt.async (fun () -> Lwt_js_events.clicks dom_button_palette
      (fun _ _ -> Lwt.return
        (Client_menu_tools.switch_display dom_palette)));

    (* Start color picker stript *)
    Color_picker.start %Server_html.color_picker

}}
