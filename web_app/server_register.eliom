
open Eliom_content.Html5.F

(* Wait end of initialization of server to return html page of app *)
let get_main_html_service () =
  match (Lwt.state Server_image.replay_no_save_drawing) with
    | Lwt.Return ()     -> (* In normal case *)
      ignore {unit Lwt.t{ Client_core.initialize () }}; (* init client *)
      Server_html.main_service_html
    | _                 -> (* During initialize process *)
      Server_html.tmp_service_html

let _ =
  Server_service.My_app.register
    ~service:Server_service.main_service
    (fun () () -> Lwt.return (get_main_html_service ()))

let _ =
  Server_service.My_app.register
    ~service:Server_service.setting_replay_service
    (fun () () -> Lwt.return (Server_html.setting_replay_service_html))

let _ =
  Server_service.My_app.register
    ~service:Server_service.start_replay_service
    (fun () (d1, (t1, (d2, (t2, coef_to_replay)))) ->
      try
	let dt1 = Server_tools.datetime_of_jsdatetime d1 t1 in
	let dt2 = Server_tools.datetime_of_jsdatetime d2 t2 in
	let dv1 = Server_tools.get_date_value dt1 in
	let dv2 = Server_tools.get_date_value dt2 in
	let s1 = Server_tools.sec_of_date dv1 in
	let s2 = Server_tools.sec_of_date dv2 in
	if (s1 >= s2)
	then failwith "Starting datetime have to be smaller than ending datetime";
	let bus = Eliom_bus.create ~scope:`Site
	  ~size:500 Json.t<Shared_tools.messages>
	in
	let write data =
	  Lwt.return (Eliom_bus.write bus data)
	in
	Lwt.async (fun () ->
	  Server_image.replay_drawing ~coef_to_replay dt1 dt2 write);

	let html_elt, body_elt, header_elt, canvas_elt, canvas2_elt,
	  angle_elt, gray_layer_elt, about_elt, starting_logo_elt =
	  Server_html.starting_replay_service_html ()
	in
	ignore {unit Lwt.t{
	  Client_core.initialize_replay %bus %body_elt
            %header_elt %canvas_elt %canvas2_elt
            %angle_elt %gray_layer_elt %about_elt
            %starting_logo_elt
	}};

	Lwt.return (html_elt)
      with e	->
	Lwt.return (Server_html.starting_replay_service_error_html ()))
