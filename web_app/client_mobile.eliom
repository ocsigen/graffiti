
{client{

  open Lwt

  let mobile_width = 480
  let mobile_height = 800
  let already_removed = ref false

  let has_small_screen () =
    let width, height = Client_tools.get_device_size () in
    (width <= mobile_width || height <= mobile_height)

  let launch_func_only_on_mobile func =
    if has_small_screen ()
    then func ()
    else ()

  let not_launch_func_on_mobile func =
    if has_small_screen ()
    then ()
    else func ()

  (** remove header on mobile
  *** and return true if it removed **)
  let remove_header_mobile body_elt header_elt = if !already_removed
      then true
      else (

        (* Remove header *)
        let mobile_screen () =
          Eliom_content.Html5.Manip.removeChild
            body_elt header_elt;
          already_removed := true;
          !already_removed
        in

        (* Check to let or not header *)
        if has_small_screen ()
        then mobile_screen ()
        else false
      )


  (** Handle image 'touch to start' apparition **)
  let handle_touch_to_start_mobile body_elt starting_logo_elt =

    (*** Tools  ***)
    let dom_statring_logo =
      Eliom_content.Html5.To_dom.of_table starting_logo_elt
    in

    let remove_touch_to_start_logo () =
      Eliom_content.Html5.Manip.removeChild body_elt starting_logo_elt
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
    if has_small_screen ()
    then mobile_screen ()
    else normal_screen ()

}}
