
{client{

  open Lwt

  let mobile_width = 480
  let mobile_height = 800
  let already_removed = ref false

  let is_on_mobile () =
    let size = Client_tools.get_device_size () in
    let width = fst size and height = snd size in
    (width <= mobile_width || height <= mobile_height)

  let launch_func_only_on_mobile func =
    if is_on_mobile ()
    then func ()
    else ()

  let not_launch_func_on_mobile func =
    if is_on_mobile ()
    then ()
    else func ()

  (** remove header on mobile
  *** and return true if it removed **)
  let remove_header_mobile () = if !already_removed
      then true
      else (

        (* Remove header *)
        let mobile_screen () =
          Eliom_content.Html5.Manip.removeChild
           %Server_html.body_elt %Server_html.header_elt;
          already_removed := true;
          !already_removed
        in

        (* Check to let or not header *)
        if is_on_mobile ()
        then mobile_screen ()
        else false
      )


  (** Handle image 'touch to start' apparition **)
  let handle_touch_to_start_mobile () =

    (*** Tools  ***)
    let dom_statring_logo =
      Eliom_content.Html5.To_dom.of_table %Server_html.starting_logo_elt
    in

    let remove_touch_to_start_logo () =
      Eliom_content.Html5.Manip.removeChild
       %Server_html.body_elt %Server_html.starting_logo_elt
    in

    (* Set touch action *)
    let mobile_screen () =
      Lwt.async (fun () ->
        Lwt_js_events.click dom_statring_logo >>= (fun _ ->
          Lwt.return (remove_touch_to_start_logo ())))
    in

    let normal_screen () =
      remove_touch_to_start_logo ()
    in

    (* Check to let or not 'touch to start' image *)
    if is_on_mobile ()
    then mobile_screen ()
    else normal_screen ()

}}
