{shared{
  open Eliom_pervasives
  open HTML5.M
  open Common
}}
{client{
  open Client
}}
open Server
open Feed

let start_drawing name canvas =

  let bus,image_string = get_bus_image name in

  let imageservice =
    Eliom_output.Text.register_coservice'
      ~timeout:10.
      (* the service is available fo 10 seconds only, but it is long
	 enouth for the browser to do its request. *)
      ~get_params:Eliom_parameters.unit
      (fun () () -> Lwt.return (image_string (), "image/png"))
  in

  Eliom_services.onload
    {{
      let canceller = launch_client_canvas %bus %imageservice %canvas in
      Eliom_client.on_unload (fun () -> stop_drawing canceller)
    }}

let () = Connected.register ~service:multigraffiti_service
  !% ( fun name () username ->
    (* the page element in wich we will include the canvas *)
    let canvas = unique (canvas ~a:[ a_width width; a_height height ]
			   [pcdata "your browser doesn't support canvas"]) in
    start_drawing name canvas;
    make_page
      [h1 [pcdata name];
       disconnect_box ();
       choose_drawing_form ();
       Eliom_output.Html5.a feed_service [pcdata "atom feed"] name;
       div ( if name = username
	 then [save_image_box name]
	 else [pcdata "no saving"] );
       canvas;])

