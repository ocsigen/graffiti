
{client{

  open Lwt

  (** Handle client menu action **)
  let start body_elt header_elt save_elt about_point gray_layer_elt about_elt =

    (*** Elements ***)
    let dom_about_point = Eliom_content.Html5.To_dom.of_span about_point in
    let dom_gray_layer = Eliom_content.Html5.To_dom.of_div gray_layer_elt in
    let dom_save = Eliom_content.Html5.To_dom.of_div save_elt in
    let dom_about = Eliom_content.Html5.To_dom.of_div about_elt in

    (* on about  *)
    Lwt.async (fun () -> Lwt_js_events.clicks dom_about_point
      (fun _ _ -> Lwt.return (Client_menu_tools.switch_fullscreen_display
                                dom_gray_layer dom_about)));

    (* on save button *)
    Lwt.async (fun () -> Lwt_js_events.clicks dom_save
      (fun _ _ -> Lwt.return (
        Dom_html.window##location##href <- Js.string "/image/graffiti.png"
       )))

}}
