
{client{

  open Lwt

  let already_removed = ref false

  let has_small_screen () =
    let width, height = Client_js_tools.get_screen_size () in
    width <= 480

  let has_medium_screen_or_less () =
    let width, height = Client_js_tools.get_screen_size () in
    width <= 768

  let launch_on_small func =
    if has_small_screen ()
    then func ()
    else ()

  let not_launch_on_small func =
    if has_small_screen ()
    then ()
    else func ()

  let launch_on_small_medium func =
    if has_medium_screen_or_less ()
    then func ()
    else ()

  let not_launch_on_small_medium func =
    if has_medium_screen_or_less ()
    then ()
    else func ()

  (** remove header on small screen
  *** and return true if it removed **)
  let remove_header body_elt header_elt =
    if !already_removed
    then true
    else (

      (* Remove header *)
      let medium_screen () =
        Eliom_content.Html5.Manip.removeChild
          body_elt header_elt;
        already_removed := true;
        !already_removed
      in

      (* Check to let or not header *)
      if has_medium_screen_or_less ()
      then medium_screen ()
      else false
    )

  (** Handle image 'touch to start' apparition **)
  let handle_touch_to_start body_elt starting_logo_elt =

    (*** Tools  ***)
    let dom_statring_logo =
      Eliom_content.Html5.To_dom.of_table starting_logo_elt
    in

    let remove_touch_to_start_logo () =
      Eliom_content.Html5.Manip.removeChild body_elt starting_logo_elt
    in

    (* Set touch action *)
    let medium_screen () =
      Lwt.async (fun () ->
        Lwt_js_events.click dom_statring_logo >>= (fun _ ->
          Lwt.return (remove_touch_to_start_logo ())))
    in

    let normal_screen () =
      remove_touch_to_start_logo ()
    in

    (* Check to let or not 'touch to start' image *)
    if has_medium_screen_or_less ()
    then medium_screen ()
    else normal_screen ()

}}
