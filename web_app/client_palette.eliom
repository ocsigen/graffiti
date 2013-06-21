
{client{

  open Lwt

  (** Handle client palette action **)
  let start body_elt header_elt canvas_elt palette_div color_picker =

    (*** Elements ***)

    let dom_palette = Eliom_content.Html5.To_dom.of_div palette_div in

    (* Add listenner of touch events on small screen *)
    (* for palette menu *)
    let detect_local_click () =
      (* to show *)
      Lwt.async (fun () ->
        Client_menu_tools.detect_local_clicks
          (Client_menu_tools.Value 0,     (* start_x *)
           Client_menu_tools.Value 100,   (* end_x *)
           Client_menu_tools.Value 0,     (* start_y *)
           Client_menu_tools.Value 100)   (* end_y *)
          (fun () -> Client_menu_tools.show_if_hide dom_palette;
            Lwt.return ()));

      (* to hide *)
      Lwt.async (fun () ->
        Client_menu_tools.detect_local_clicks
          (Client_menu_tools.Value 100,     (* start_x *)
           Client_menu_tools.Max_value 0,   (* end_x *)
           Client_menu_tools.Value 0,       (* start_y *)
           Client_menu_tools.Max_value 0)   (* end_y *)
          (fun () -> Client_menu_tools.hide_if_show dom_palette;
          Lwt.return ()));
    in Client_mobile.launch_func_only_on_small_screen detect_local_click;

    (* Add listenner of resize event on small screen (menu mode) *)
    (* on palette menu *)
    let handle_orientationchange_and_resize () =
      Lwt.async (fun () ->
        Client_tools.limited_onorientationchanges_or_onresizes
          (fun _ _ -> Lwt.return
            (Client_menu_tools.set_position
               body_elt header_elt dom_palette 14)))
    in
    Client_mobile.launch_func_only_on_small_screen
    handle_orientationchange_and_resize;

    (* Start color picker stript *)
    Color_picker.start color_picker

}}
