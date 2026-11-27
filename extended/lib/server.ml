open Eliom.Content
open Common
open Lwt.Syntax

module My_app =
  Eliom.Registration.App (struct
    let application_name = "graffiti"
    let global_data_path = None
  end)

let launch_server_canvas () =
  let bus = Eliom.Bus.create [%json: messages] in

  let draw_server, image_string =
    let rgb_ints_to_floats (r, g, b) =
      float r /. 255., float g /. 255., float b /. 255. in
    let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w:width ~h:height in
    let ctx = Cairo.create surface in
    ((fun (rgb, size, (x1, y1), (x2, y2)) ->

      (* Set thickness of brush *)
      let r, g, b = rgb_ints_to_floats rgb in
      Cairo.set_line_width ctx (float size) ;
      Cairo.set_line_join ctx Cairo.JOIN_ROUND ;
      Cairo.set_line_cap ctx Cairo.ROUND ;
      Cairo.set_source_rgb ctx r g b ;

      Cairo.move_to ctx (float x1) (float y1) ;
      Cairo.line_to ctx (float x2) (float y2) ;
      Cairo.Path.close ctx ;

      (* Apply the ink *)
      Cairo.stroke ctx ;
     ),
     (fun () ->
       let b = Buffer.create 10000 in
       (* Output a PNG in a string *)
       Cairo.PNG.write_to_stream surface (Buffer.add_string b);
       Buffer.contents b
     ))
  in
  Lwt.async (fun () -> Lwt_stream.iter draw_server (Eliom.Bus.stream bus));
  bus, image_string

let graffiti_info = Hashtbl.create 0

let imageservice =
  Eliom.Registration.String.create
    ~path:(Eliom.Service.Path ["image"])
    ~headers:
       (Cohttp.Header.add_list (Cohttp.Header.init ())
          [(Ocsigen_http.Header.Name.(to_string cache_control), "no-cache") ;
           (Ocsigen_http.Header.Name.(to_string expires), string_of_int 0)])
    ~meth:
      (Eliom.Service.Get
         (let open Eliom.Parameter in string "name" ** int "q"))
    (* we add another parameter for the browser not to cache: at least
       for chrome, there is no way to force the browser to reload the
       image without leaving the application *)
    (fun (name, _) () ->
      match Hashtbl.find_opt graffiti_info name with
      | Some (_, image_string) -> Lwt.return (image_string (), "image/png")
      | None -> Lwt.fail Eliom.Common.Eliom_404)

let get_bus (name : string) =
  (* create a new bus and image_string function only if it did not exist *)
  match Hashtbl.find_opt graffiti_info name with
  | Some (bus, _) -> bus
  | None ->
      let bus, image_string = launch_server_canvas () in
      Hashtbl.add graffiti_info name (bus, image_string);
      bus

let main_service =
  Eliom.Service.create
    ~path:(Eliom.Service.Path [""])
    ~meth:(Eliom.Service.Get (Eliom.Parameter.unit))
    ()

let multigraffiti_service =
  Eliom.Service.create
    ~path:(Eliom.Service.Path [""])
    ~meth:(Eliom.Service.Get (Eliom.Parameter.(suffix (string "name"))))
    ()

let choose_drawing_form () =
  Html.D.Form.get_form ~service:multigraffiti_service
    (fun (name) ->
       [Html.D.p [
           Html.D.txt "drawing name: ";
           Html.D.Form.input ~input_type:`Text ~name
             Html.D.Form.string;
           Html.D.br ();
           Html.D.Form.input ~input_type:`Submit ~value:"Go"
             Html.D.Form.string
         ]])

let make_page body =
  let css file =
    Html.D.css_link
      ~uri:(Html.D.make_uri ~service:(Eliom.Service.static_dir ()) ["css"; file])
      ()
  in
  Lwt.return
    (Html.D.html
       (Html.D.head
          (Html.D.title (Html.D.txt "Graffiti"))
          [css "graffiti.css"; css "ot_color_picker.css"])
       (Html.D.body body))

let connection_service = Eliom.Service.create
    ~path:Eliom.Service.No_path
    ~meth:(Eliom.Service.Post (
      Eliom.Parameter.unit,
      Eliom.Parameter.(string "name" ** string "password")
    ))
    ()

let disconnection_service = Eliom.Service.create
    ~path:Eliom.Service.No_path
    ~meth:(Eliom.Service.Post (Eliom.Parameter.unit, Eliom.Parameter.unit))
    ()

let create_account_service =
  Eliom.Service.create
    ~path:Eliom.Service.No_path
    ~meth:(Eliom.Service.Post
             (Eliom.Parameter.unit,
              Eliom.Parameter.(string "name" ** string "password")))
    ()

let user_table = Ocsipersist.Polymorphic.open_table "user_table"

let check_pwd name pwd =
  let* user_table = user_table in
  Lwt.catch
    (fun () ->
      let* saved_password = Ocsipersist.Polymorphic.find user_table name in
      Lwt.return (pwd = saved_password))
    (function Not_found -> Lwt.return false | e -> Lwt.fail e)

let () = Eliom.Registration.Action.register
  ~service:create_account_service
  (fun () (name, pwd) ->
    let* user_table = user_table in
    Ocsipersist.Polymorphic.add user_table name pwd)

let () = Eliom.Registration.Action.register
  ~service:connection_service
  (fun () (name, password) ->
    let* ok = check_pwd name password in
    if ok
    then Eliom.State.set_volatile_data_session_group
        ~scope:Eliom.Common.default_session_scope name;
    Lwt.return ())

let () =
  Eliom.Registration.Action.register
    ~service:disconnection_service
    (fun () () ->
      Eliom.State.discard ~scope:Eliom.Common.default_session_scope ())

let disconnect_box () =
  Html.D.Form.post_form ~service:disconnection_service
    (fun _ ->
       [Html.D.p [
           Html.D.Form.input
             ~input_type:`Submit ~value:"Log out"
             Html.D.Form.string
         ]
       ]) ()

let login_name_form service button_text =
  Html.D.Form.post_form ~service
    (fun (name1, name2) ->
       [Html.D.p [
           Html.D.txt "login: ";
           Html.D.Form.input ~input_type:`Text ~name:name1
             Html.D.Form.string;
           Html.D.br ();
           Html.D.txt "password: ";
           Html.D.Form.input ~input_type:`Password ~name:name2
             Html.D.Form.string;
           Html.D.br ();
           Html.D.Form.input ~input_type:`Submit ~value:button_text
             Html.D.Form.string
         ]]) ()

let default_content () =
  make_page
    [Html.D.h1 [Html.D.txt "Welcome to Multigraffiti"];
     Html.D.h2 [Html.D.txt "log in"];
     login_name_form connection_service "Connect";
     Html.D.h2 [Html.D.txt "create account"];
     login_name_form create_account_service "Create account";]

module Connected_translate =
struct
  type page = string -> My_app.page Lwt.t
  let translate page =
    match Eliom.State.get_volatile_data_session_group
      ~scope:Eliom.Common.default_session_scope () with
    | None -> default_content ()
    | Some username -> page username
end

module Connected =
  Eliom.Registration.Customize (My_app) (Connected_translate)

let ( !% ) f = fun a b -> Lwt.return (fun c -> f a b c)

let () = Connected.register
  ~service:main_service
  !% (fun () () username ->
    make_page
      [Html.D.h1 [Html.D.txt ("Welcome to Multigraffiti " ^ username)];
       choose_drawing_form ()])
