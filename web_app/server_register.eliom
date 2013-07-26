
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
    (fun () (d1, (t1, (d2, t2))) -> Lwt.return
      (Server_html.starting_replay_service_html d1 t1 d2 t2))
