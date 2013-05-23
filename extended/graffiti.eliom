{shared{
  open Eliom_content
  open Common
}}
{client{
  open Client
}}
open Server
open Feed

let start_drawing name image canvas =
  let bus = get_bus name in
  ignore {unit{
    let canceller = launch_client_canvas %bus %image %canvas in
    Eliom_client.onunload (fun () -> stop_drawing canceller)
  }}

let counter = ref 0

let () = Connected.register ~service:multigraffiti_service
  !% ( fun name () username ->
    (* Some browsers won't reload the image, so we force
       them by changing the url each time. *)
    incr counter;
    let image =
      Html5.D.img ~alt:name ~src:(Html5.D.make_uri
				  ~service:imageservice (name, !counter)) ()
    in
    let canvas =
      Html5.D.canvas
        ~a:[Html5.D.a_width width; Html5.D.a_height height ]
        [ Html5.D.pcdata "your browser doesn't support canvas";
          Html5.D.br (); image] in
    lwt save_box = if name = username
      then save_image_box name
      else Lwt.return (Html5.D.pcdata "no saving")
    in
    start_drawing name image canvas;
    make_page
      [ Html5.D.h1 [ Html5.D.pcdata name];
        disconnect_box ();
        choose_drawing_form ();
        Html5.D.a feed_service [Html5.D.pcdata "atom feed"] name;
        Html5.D.div [save_box];
        canvas;])
