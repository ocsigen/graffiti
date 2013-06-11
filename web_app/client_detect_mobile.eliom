
{client{

  open Lwt

  (** Handle image 'touch to start' apparition on smartphone  **)
  let detect () =

    (*** Init data ***)
    (*** Get device size to avoid appearance it on computer ***)
    let size = Client_tools.get_device_size () in
    let width = fst size and height = snd size in

    let dom_statring_logo =
      Eliom_content.Html5.To_dom.of_table %Server_html.starting_logo_elt
    in

    let remove_starting_logo () =
      Eliom_content.Html5.Manip.removeChild
       %Server_html.body_elt %Server_html.starting_logo_elt
    in

    (* Set touch action *)
    let mobile_screen () =
      Lwt.async (fun () ->
        Lwt_js_events.click dom_statring_logo >>= (fun _ ->
          Lwt.return (remove_starting_logo ())))
    in

    let normal_screen () =
      remove_starting_logo ()
    in

    (* Check to let or not 'touch to start' image *)
    if (width <= 480 || height <= 800)
    then ignore (mobile_screen ())
    else ignore (normal_screen ())


}}
