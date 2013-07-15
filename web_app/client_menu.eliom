
{client{

  open Lwt

  (** Handle client menu action **)
  let start body_elt header_elt save_elt save_link_elt
      about_point gray_layer_elt about_elt =

    (*** Elements ***)
    let dom_about_point = Eliom_content.Html5.To_dom.of_span about_point in
    let dom_gray_layer = Eliom_content.Html5.To_dom.of_div gray_layer_elt in
    let dom_save = Eliom_content.Html5.To_dom.of_div save_elt in
    let dom_save_link = Eliom_content.Html5.To_dom.of_a save_link_elt in
    let dom_about = Eliom_content.Html5.To_dom.of_div about_elt in

    (* on about  *)
    Lwt.async (fun () -> Lwt_js_events.clicks dom_about_point
      (fun _ _ -> Lwt.return (Client_menu_tools.switch_fullscreen_display
                                dom_gray_layer dom_about)));

    (* on save button *)
    (* active contract/expand only on small screen *)
    let save_click () =

      let disable_contract = ref false in
      let one_time_disable = ref false in

      let id = ref
        (Client_event_tools.disable_event Dom_html.Event.click dom_save_link)
      in
      let current_id_disable = ref true in
      let disable_id () = if (not !current_id_disable) then
          begin
            id := Client_event_tools.disable_event
	      Dom_html.Event.click dom_save_link;
            current_id_disable := true
          end
      in
      let enable_id () = if (!current_id_disable) then
          begin
            Client_event_tools.enable_event !id;
            current_id_disable := false
          end
      in

      let contract () =
        dom_save##style##width <- Js.string "30px";
        enable_id ()
      and expand () =
        one_time_disable := true;
        dom_save##style##width <- Js.string "60px";
        disable_id ()
      in

      (* avoid to let expand after return by browser arrow *)
      Lwt.async (fun () ->
      let rec aux () =
        lwt () = Lwt_js.sleep 3. in
        if (not !disable_contract) then
          (if (not !one_time_disable)
           then begin contract (); disable_id () end
           else one_time_disable := false);
        aux ()
      in aux ());

      (* action *)

      (* Handle touch slide *)
      Lwt.async (fun () -> Slide_tools.slide dom_save
        Slide_tools.Right
        ~mode:Slide_tools.Width_height
        ~start_callback:(fun () -> disable_id ();
          Lwt.return (disable_contract := true))
        ~end_callback:(fun width ->
          if width > 45 then expand () else contract ();
          Lwt.return (disable_contract := false))
        30 60)

    in Client_mobile.launch_on_small_medium save_click

}}
