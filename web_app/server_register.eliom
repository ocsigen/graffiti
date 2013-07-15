
open Eliom_content.Html5.F

let _ =
  Server_service.My_app.register
    ~service:Server_service.main_service
    (fun () () ->

      (* html generation *)
      let html_page =
        (html
           (Eliom_tools.F.head ~title:"Graffiti"
              ~css:[["css"; "grf_color_picker.css"];
                    ["css"; "grf_slider.css"];
                    ["css"; "graffiti.css"];
                    ["css"; "graffiti_small_screen.css"];
                    ["css"; "graffiti_medium_screen.css"];
                    ["css"; "graffiti_large_screen.css"]]
              ~other:[meta ~a:[a_http_equiv "X-UA-Compatible";
                               a_content "IE=edge,chrome=1"]
                         ();
                      meta ~a:[a_name "viewport";
                               a_content "user-scalable=no, initial-scale=1, maximum-scale=1, minimum-scale=1, width=device-width, height=device-height, target-densitydpi=device-dpi"]
                     () ] ())
           Server_html.body_elt)
      in

      (*** init client ***)
      ignore {unit{

	(* Remove navigation bar *)
	ignore (Client_js_tools.hide_navigation_bar ());

        (* Random logo image *)
        Client_header.rand_logo
          %Server_html.body_elt
          %Server_html.header_elt;

        (* start canvs script *)
        ignore (Client_core.start
                  %Server_html.body_elt
                  %Server_html.header_elt
                  %Server_html.canvas_elt
                  %Server_html.canvas2_elt
                  %Server_html.grf_slider
                  %Server_html.color_picker);

        (* Start menu script *)
        Client_menu.start
          %Server_html.body_elt
          %Server_html.header_elt
          %Server_html.save_button_elt
          %Server_html.save_link_elt
          %Server_html.about_point
          %Server_html.gray_layer_elt
          %Server_html.about_elt;

        (* Start palette menu script *)
        Client_palette.start
          %Server_html.body_elt
          %Server_html.canvas_elt
          %Server_html.palette_wrapper
          %Server_html.palette_button
          %Server_html.grf_slider
          %Server_html.color_picker
          %Server_html.color_div;

        (* Check if 'touch to start' have to be removed (on pc) *)
        Client_mobile.handle_touch_to_start
          %Server_html.body_elt
          %Server_html.starting_logo_elt;

      }};

      Lwt.return (html_page))
