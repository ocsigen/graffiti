{shared{
  open Eliom_pervasives
  open Common
}}
{client{
  open Client
}}
open Server
open Feed

let start_drawing name image canvas =
  let bus = get_bus name in
  Eliom_services.onload
    {{
      let canceller = launch_client_canvas %bus %image %canvas in
      Eliom_client.on_unload (fun () -> stop_drawing canceller)
    }}

let counter = ref 0

let () = Connected.register ~service:multigraffiti_service
  !% ( fun name () username ->
    (* Some browsers won't reload the image, so we force
       them by changing the url each time. *)
    incr counter;
    let image =
      HTML5.img ~alt:name ~src:(Eliom_output.Html5.make_uri
				  ~service:imageservice (name,!counter)) () in
    let canvas =
      HTML5.canvas
        ~a:[HTML5.a_width width; HTML5.a_height height ]
        [ HTML5.pcdata "your browser doesn't support canvas";  HTML5.br (); image] in
    lwt save_box = if name = username
      then save_image_box name
      else Lwt.return (HTML5.pcdata "no saving")
    in
    start_drawing name image canvas;
    make_page
      [ HTML5.h1 [ HTML5.pcdata name];
        disconnect_box ();
        choose_drawing_form ();
        Eliom_output.Html5.a feed_service [HTML5.pcdata "atom feed"] name;
        HTML5.div [save_box];
        canvas;])

