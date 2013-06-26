
{client{

  open Lwt

  (** Handle client palette action **)
  let start body_elt header_elt canvas_elt palette_div
      slider color_picker color_div =

    (*** Elements ***)

    let dom_palette = Eliom_content.Html5.To_dom.of_div palette_div in
    let dom_canvas = Eliom_content.Html5.To_dom.of_canvas canvas_elt in
    let dom_color = Eliom_content.Html5.To_dom.of_div color_div in
    let width, height = Client_tools.get_size dom_canvas in
    let base_size = min width height in

    (* Add listenner of touch events on small screen *)
    (* for palette menu *)
    let detect_local_click () =
      (* to show *)
      Lwt.async (fun () ->
        Client_tools.detect_local_clicks
          (Client_tools.Value 0,     (* start_x *)
           Client_tools.Value 100,   (* end_x *)
           Client_tools.Value 0,     (* start_y *)
           Client_tools.Value 100)   (* end_y *)
          (fun () -> Client_menu_tools.show_if_hide dom_palette;
            Lwt.return ()));

      (* to hide *)
      Lwt.async (fun () ->
        Client_tools.detect_local_clicks
          (Client_tools.Value 100,     (* start_x *)
           Client_tools.Max_value 0,   (* end_x *)
           Client_tools.Value 0,       (* start_y *)
           Client_tools.Max_value 0)   (* end_y *)
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

    let color_picker' = if (not (Client_mobile.has_small_screen ()))
      then Color_picker.add_square_color color_picker Color_picker.lll_color_6
      else color_picker
    in

    (* catch slider move and click *)
    let handler () =
      let brush_size = Js.string (string_of_int
        (int_of_float (
	  ((Client_tools.get_slider_value slider) *.
	    (float_of_int base_size)))) ^ "px")
      in
      dom_color##style##width <- brush_size;
      dom_color##style##height <- brush_size;
      Lwt.return ()
    in
    Grf_slider.change_move_slide_callback slider handler;
    Grf_slider.change_click_callback slider handler;

    (* start slider script *)
    Grf_slider.start slider;

    (* Start color picker stript *)
    Color_picker.start color_picker'

}}
