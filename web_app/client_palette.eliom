
{client{

  open Lwt

  (** Handle client palette action **)
  let start palette_button_elt palette_div color_picker =

    (*** Elements ***)

    let dom_button_palette =
      Eliom_content.Html5.To_dom.of_div palette_button_elt
    in
    let dom_palette = Eliom_content.Html5.To_dom.of_div palette_div in

    (* Add listenner of touch slide events *)
    (* for palette menu *)
    let detect_local_click () =
      Client_menu_tools.detect_local_clicks
        (Client_menu_tools.Value 0,     (* start_x *)
         Client_menu_tools.Value 100,   (* end_x *)
         Client_menu_tools.Value 0,     (* start_y *)
         Client_menu_tools.Value 100)   (* end_y *)
        (fun () -> Client_menu_tools.show_if_hide dom_palette)
    in

    (* Launch it only on mobile to avoid conflit with button menu *)
    Client_mobile.launch_func_only_on_mobile detect_local_click;

    Client_menu_tools.detect_local_clicks
      (Client_menu_tools.Value 100,     (* start_x *)
       Client_menu_tools.Max_value 0,   (* end_x *)
       Client_menu_tools.Value 0,       (* start_y *)
       Client_menu_tools.Max_value 0)   (* end_y *)
      (fun () -> Client_menu_tools.hide_if_show dom_palette);

    (* Add listenner of touch click events *)
    (* on palette button *)
    Lwt.async (fun () -> Lwt_js_events.clicks dom_button_palette
      (fun _ _ -> Lwt.return
        (Client_menu_tools.switch_display dom_palette)));

    (* Start color picker stript *)
    Color_picker.start color_picker

}}
