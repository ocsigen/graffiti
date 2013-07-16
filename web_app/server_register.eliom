
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
      ignore {unit Lwt.t{ Client_core.initialize () }};

      Lwt.return (html_page))
