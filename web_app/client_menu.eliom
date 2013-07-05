
{client{

  open Lwt

  (** Handle client menu action **)
  let start body_elt header_elt save_elt save_link_elt save_div_elt
      about_point gray_layer_elt about_elt =

    (*** Elements ***)
    let dom_about_point = Eliom_content.Html5.To_dom.of_span about_point in
    let dom_gray_layer = Eliom_content.Html5.To_dom.of_div gray_layer_elt in
    let dom_save = Eliom_content.Html5.To_dom.of_div save_elt in
    let dom_save_link = Eliom_content.Html5.To_dom.of_a save_link_elt in
    let dom_save_div = Eliom_content.Html5.To_dom.of_div save_div_elt in
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
        (Client_tools.disable_event Dom_html.Event.click dom_save_link)
      in
      let current_id_disable = ref true in
      let disable_id () = if (not !current_id_disable) then
	  begin
	    id := Client_tools.disable_event Dom_html.Event.click dom_save_link;
	    current_id_disable := true
	  end
      in
      let enable_id () = if (!current_id_disable) then
	  begin
	    Client_tools.enable_event !id;
	    current_id_disable := false
	  end
      in

      let contract () =
        dom_save##style##width <- Js.string "30px";
        dom_save_div##style##width <- Js.string "13px";
	disable_id ()
      and expand () =
	one_time_disable := true;
	dom_save##style##width <- Js.string "60px";
        dom_save_div##style##width <- Js.string "26px";
	enable_id ()
      in

      (* avoid to let expand after return by browser arrow *)
      Lwt.async (fun () ->
      let rec aux () =
        lwt _ = Lwt_js.sleep 2. in
        if (not !disable_contract) then
          (if (not !one_time_disable)
           then contract ()
           else one_time_disable := false);
        aux ()
      in aux ());

      (* action *)

      (* Handle touch slide *)
      Lwt.async (fun () -> Client_tools.languet dom_save dom_save
        Client_tools.Lg_right
        ~start_callback:(fun () ->
          let right = if dom_save##clientWidth > 45 then "0px" else "-30px" in
          dom_save##style##right <- Js.string right;
          dom_save##style##width <- Js.string "60px";
          dom_save_div##style##width <- Js.string "26px";
	  disable_id ();
          Lwt.return (disable_contract := true))
        ~end_callback:(fun () ->
          let right =
	    let str = dom_save##style##right in
	    let reg = jsnew Js.regExp (Js.string "px") in
	    int_of_string (Js.to_string (str##replace(reg, Js.string "")))
	  in
          dom_save##style##right <- Js.string "0px";
          if right > -15 then expand () else contract ();
          Lwt.return (disable_contract := false))
        (-30) 0)

    in Client_mobile.launch_only_on_small_screen save_click

}}
