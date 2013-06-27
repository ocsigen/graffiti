
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

    (* Elarge color picker on computer *)
    let color_picker' = if (not (Client_mobile.has_small_screen ()))
      then Grf_color_picker.add_square_color color_picker
        Grf_color_picker.lll_color_6
      else color_picker
    in

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
    (* on color square *)
    let handle_color_square_resize () =
      let margin = 8 in
      let body_height = Dom_html.document##documentElement##clientHeight in
      let color_square_list =
        Grf_color_picker.get_square_color_div_list color_picker'
      in
      let n_row = (List.length color_square_list) / 2 in
      let new_height = (body_height - (margin * 2)) / n_row in
      let rec aux = function
        | []            -> ()
        | div::tail     ->
          let dom_div = Eliom_content.Html5.To_dom.of_div div in
          dom_div##style##height <- Js.string
            (string_of_int (new_height) ^ "px");
          aux tail
      in aux color_square_list
    in handle_color_square_resize (); (* To initialize view *)
    Lwt.async (fun () -> Client_tools.limited_onorientationchanges_or_onresizes
      (fun _ _ -> Lwt.return (handle_color_square_resize ())));

    (* catch slider move and click *)
    let handler () =
      let brush_size = Js.string (string_of_int
        (int_of_float (
          ((Client_ext_mod_tools.get_slider_value slider) *.
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
    Grf_color_picker.start color_picker'

}}
