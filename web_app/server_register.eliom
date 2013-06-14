
open Eliom_content.Html5.F

let _ =
  Server_tools.My_app.register
    ~service:Server_tools.main_service
    (fun () () ->

      (* html generation *)
      let html_page =
        (html
           (Eliom_tools.F.head ~title:"Graffiti"
              ~css:(Color_picker.css_list@
                      [["css"; "graffiti.css"];
                       ["css"; "graffiti_small_screen.css"];
                       ["css"; "graffiti_medium_screen.css"];
                       ["css"; "graffiti_large_screen.css"]])
              ~other:[meta ~a:[a_http_equiv "X-UA-Compatible";
                               a_content "IE=edge,chrome=1"]
                         ();
                      meta ~a:[a_name "viewport";
                               a_content "width=device-width; user-scalable=0; initial-scale=1.0; maximum-scale=1.0;"]
                     () ] ())
           Server_html.body_elt)
      in

      (*** init client ***)
      ignore {unit Lwt.t{
        Client_core.start (Client_canvas.init ())
      }};
      Lwt.return (html_page))
