
open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.D

let _ =
  Server_tools.My_app.register
    ~service:Server_tools.main_service
    (fun () () ->

      (* html generation *)
      let html_page =
        (html
           (head (title (pcdata "Graffiti"))
              [meta ~a:[a_http_equiv "X-UA-Compatible";
                        a_content "IE=edge,chrome=1"]
                  ();
               meta ~a:[a_name "viewport";
                        a_content "width=device-width; user-scalable=0; initial-scale=1.0; maximum-scale=1.0;"]
                 ();
               css_link
                  ~uri:(make_uri (Eliom_service.static_dir ())
                          ["css"; "graffiti.css"]) ();
               css_link
                 ~uri:(make_uri (Eliom_service.static_dir ())
                         ["css"; "graffiti_small_screen.css"]) ();
               css_link
                 ~uri:(make_uri (Eliom_service.static_dir ())
                         ["css"; "graffiti_medium_screen.css"]) ();
               css_link
                 ~uri:(make_uri (Eliom_service.static_dir ())
                         ["css"; "graffiti_large_screen.css"]) ()])
           Server_html.body_elt)
      in

      (*** init client ***)
      ignore {unit Lwt.t{
        Client_core.start (Client_canvas.init ())
      }};
      Lwt.return (html_page))
