
open Eliom_content.Html5.F
open Server_html

(* Wait end of initialization of server to return html page of app *)
let get_main_html_service () =
  match (Lwt.state Server_image.replay_no_save_drawing) with
    | Lwt.Return ()     -> (* In normal case *)
      let main_record = Server_html.main_service_html () in
      (* init client *)
      ignore {unit Lwt.t{ Client_core.initialize %main_record }};
      main_record.ms_main.html
    | _                 -> (* During initialize process *)
      Server_html.tmp_service_html ()

let _ =
  Server_service.My_app.register
    ~service:Server_service.main_service
    (fun () () -> Lwt.return (get_main_html_service ()))

let _ =
  Server_service.My_app.register
    ~service:Server_service.information_service
    (fun () () -> Lwt.return (Server_html.info_service_html ()))

let _ =
  Server_service.My_app.register
    ~service:Server_service.setting_replay_service
    (fun () () -> Lwt.return (Server_html.setting_replay_service_html ()))

let _ =
  Server_service.My_app.register
    ~service:Server_service.start_replay_service
    (fun () (d1, (t1, (d2, (t2, (coef_to_replay, hts))))) ->
      try
        let dt1 = Server_tools.datetime_of_jsdatetime d1 t1 in
        let dt2 = Server_tools.datetime_of_jsdatetime d2 t2 in
        let dv1 = Server_tools.get_date_value dt1 in
        let dv2 = Server_tools.get_date_value dt2 in
        let s1 = Server_tools.sec_of_date dv1 in
        let s2 = Server_tools.sec_of_date dv2 in
        if (s1 >= s2)
        then failwith "Starting datetime have to be smaller than ending datetime";

        (* Have to change by channel *)
        let bus = Eliom_bus.create ~scope:`Site
          ~size:500 Json.t<Shared_tools.messages>
        in

        let write data =
          Lwt.return (Eliom_bus.write bus data)
        in
        let coef =
          if not (coef_to_replay = 0)
          then 1. /. (float_of_int coef_to_replay)
          else float_of_int coef_to_replay
        in
        let bool_hts = match hts with
          | Some _      -> true
          | _           -> false
        in
        Lwt.async (fun () ->
          Server_image.replay_drawing
            ~coef_to_replay:coef
            ~skip_hts:bool_hts
            dt1 dt2 write);

        let sr_record = Server_html.starting_replay_service_html () in
        ignore {unit Lwt.t{ Client_replay.initialize %bus %sr_record }};
        Lwt.return (sr_record.sr_main.html)
      with e    ->
        Lwt.return (Server_html.starting_replay_service_error_html ()))
