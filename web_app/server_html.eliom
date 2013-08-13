
open Eliom_content.Html5
open Eliom_content.Html5.F

let start_width = 400
let start_height = 200

(* about elements *)

let about_point = D.div ~a:[a_class["about_point"]] []

let about_link_elt =
  Raw.a
    ~a:[a_href (Xml.uri_of_string "http://ocsigen.org");
        a_target "_blank"]
    [pcdata "Ocsigen"]

let about_elt = D.div ~a:[a_class["about_wrapper"]]
  [table ~a:[a_class["inline_table"]]
      (tr [td [div ~a:[a_class["about_block"]]
                  [img ~a:[a_class["about_logo"]]
                      ~alt:("Graffiti Logo")
                      ~src:(make_uri
                              ~service:(Eliom_service.static_dir ())
                              ["img"; "Logo_GRAFFITIbyOcsigen_blue.png"])
                      ();
                   br ();
                   pcdata "Web application developed by Vincent Balat";
                   br ();
                   pcdata "Extended and exported on Mobile by Arnaud Parant";
                   br ();
                   pcdata "Design by Bruno Loton";
                   br ();
                   div [pcdata "Created with "; about_link_elt]]]]) []]

let gray_layer_elt = D.div ~a:[a_class["gray_layer"]] []

(* canvas element *)

let canvas_elt =
  D.canvas ~a:[a_width start_width; a_height start_height;
             a_class["unselectable"]]
    [pcdata "your browser doesn't support canvas"]

let canvas2_elt =
  D.canvas ~a:[a_width start_width; a_height start_height;
               D.Unsafe.string_attrib "draggable" "false";
               a_class["canvas2"; "unselectable"]] []


let angle_elt = D.div ~a:[a_class["angle_div"]] [about_point]

(* save elements *)

let save_div_elt = D.div ~a:[a_class["save_div"]] []

(** change image in css with class save_button *)
let save_link_elt = D.a
  ~service:Server_image.download_imageservice
  ~a:[a_class["save_link"]; D.Unsafe.string_attrib "download" "graffiti.png"]
   [save_div_elt] ()

let save_button_elt = D.div ~a:[a_class["save_button"]] [save_link_elt]

(* palette *)

let color_picker, color_div, block = Grf_color_picker.create
  ~initial_color:(0, 3, 0) ~lll_color:Grf_color_picker.lll_color_10 ()

let grf_slider, slider_elt = Grf_slider.create
  ~orientation:Grf_slider.Vertical ?initial_value:(Some 0.8) ()

let palette_button = D.table ~a:[a_class["palette_button"]]
    (tr [td ~a:[a_class["max_height"]] [color_div]]) []

let palette_wrapper = D.div ~a:[a_class["palette_wrap"]]
  [table (tr ~a:[a_class["max_height"]] [td [slider_elt]; td [block]]) [];
   palette_button]

(* starting logo *)

let starting_logo_elt =
  D.table ~a:[a_class["logo"]]
    (tr [td
            [img ~a:[a_class["logo_img"]]
                ~alt:("Graffiti Logo")
                ~src:(make_uri
                        ~service:(Eliom_service.static_dir ())
                        ["img"; "Logo_GRAFFITIbyOcsigenTouch.png"])
                ()]])
    []

(* General element *)

let manifest_uri = Xml.uri_of_string "graffiti.appcache"

let header =
  Eliom_tools.F.head ~title:"Graffiti"
    ~css:[["css"; "grf_color_picker.css"];
          ["css"; "grf_slider.css"];
          ["css"; "graffiti.css"];
          ["css"; "graffiti_large_screen.css"];
          ["css"; "graffiti_medium_screen.css"];
          ["css"; "graffiti_handheld_screen.css"];
          ["css"; "graffiti_small_handheld_screen.css"]]
    ~other:[meta ~a:[a_http_equiv "X-UA-Compatible";
                     a_content "IE=edge,chrome=1"]
               ();
            meta ~a:[a_name "viewport";
                     a_content "user-scalable=no, initial-scale=1, maximum-scale=1, minimum-scale=1, width=device-width, height=device-height, target-densitydpi=device-dpi"]
              () ] ()

(* main service *)

let header_elt =
  D.div ~a:[a_class["header_div"; "unselectable"]]
    [a ~a:[a_class["tool_button"; "right_button"];
	   a_target "_blank";
	   a_title "Replay"]
       ~service:Server_service.setting_replay_service
       [img ~alt:("Replay Logo")
           ~src:(make_uri
                   ~service:(Eliom_service.static_dir ())
                   ["img"; "replay.png"])
           ()] ();
     a ~a:[a_class["tool_button"];
	   a_target "_blank";
	   a_title "Information"]
       ~service:Server_service.information_service
       [pcdata "I"] ()]

(* These bodys are sementicaly strange
   Because html element are created at top level and give to several services
   So it is the same element ?
   If yes, it is very strange.
   Else, why create it at top level ?

   So, it need to be recode to create element in service calling. *)
let body_elt = D.body ~a:[a_class["unselectable"]]
  [header_elt; div ~a:[a_id "canvas"] [canvas_elt; canvas2_elt; angle_elt];
   save_button_elt; palette_wrapper; gray_layer_elt; about_elt;
   starting_logo_elt]

let main_service_html =
  html ~a:[a_manifest manifest_uri] header body_elt

(* Intialize process Elements *)
let tmp_body = body [div [pcdata "Graffiti is in initialize process."];
                     div [pcdata "Try again in a few moment."];
		     br ();
		     a ~service:Server_service.main_service
		       [pcdata "Try again"] ()]

let tmp_service_html =
  html header tmp_body

(* Information service *)
let info_body = body ~a:[a_class["information_body"]]
  [a ~service:Server_service.main_service
      [pcdata "Come back to main service"] ();
   br ();
   div [pcdata "You could easily send data in bus."];
                      (* Have to change this hard link by getting function link *)
   div [pcdata "It is at: http://ocsigen.org/graffiti2/drawing"];
   div [pcdata "Information have to format as:"];
   div [pcdata "(color, size, (x1, y1), (x2, y2))"];
   br ();
   div [pcdata "color: string as #0198DD"];
   div [pcdata "size: float, relative to canvas' height, less or equal than 1, more or equal than 0"];
   div [pcdata "x1: float, x origin, relative to width, less or equal than 1, more or equal than 0"];
   div [pcdata "y1: float, y origin, relative to height, less or equal than 1, more or equal than 0"];
   div [pcdata "x2: float, x destination, relative to width, less or equal than 1, more or equal than 0"];
   div [pcdata "y2: float, y destination, relative to height, less or equal than 1, more or equal than 0"];
  ]

let info_service_html =
  html header info_body


(* Setting replay *)

let setting_form () =
  let to_str = Printf.sprintf "%02d" in
  let get_time (_, _, _, hour, min, _, _) =
    (to_str hour) ^ ":" ^ (to_str min)
  in
  let get_date (mday, mon, year, _, _, _, _) =
    (to_str year) ^ "-" ^ (to_str mon) ^ "-" ^ (to_str mday)
  in
  let get_date_less_one_day (mday, mon, year, hour, min, sec, mls) =
    let _, new_tm =
      Server_tools.check_and_fix_date (mday - 1, mon, year, hour, min, sec, mls)
    in
    get_date (Server_tools.datevalue_of_tm new_tm mls)
  in
  let default_time = "00:00" in
  let current_datetime =
    Server_tools.get_date_value (Server_tools.get_str_localdate ())
  in
  let date_less_one_day = get_date_less_one_day current_datetime in
  let current_date = get_date current_datetime in
  let current_time = get_time current_datetime in
  let default_coef = 5 in
  let hts_value = 1 in
  post_form ~service:Server_service.start_replay_service
    (fun (start_d, (start_t, (end_d, (end_t, (coef_to_replay, hts))))) ->
        [fieldset
            [label ~a:[a_for start_d]
                [pcdata "Date and time to starting replay"];
             string_input ~input_type:`Date ~name:start_d ()
               ~value:date_less_one_day;
             string_input ~input_type:`Time ~name:start_t ()
               ~value:default_time;
             br ();
             label ~a:[a_for end_d]
               [pcdata "Date and time to finishing"];
             string_input ~input_type:`Date ~name:end_d ()
               ~value:current_date;
             string_input ~input_type:`Time ~name:end_t ()
               ~value:current_time;
             br ();
             label ~a:[a_for coef_to_replay]
               [pcdata "Coeficient of accelerating"];
             int_input ~input_type:`Number ~name:coef_to_replay ()
               ~value:default_coef;
             br ();
             label ~a:[a_for coef_to_replay]
               [pcdata "Skip huge time space"];
             int_input ~a:[a_checked `Checked]
               ~input_type:`Checkbox ~name:hts ()
               ~value:hts_value;
             br ();
             string_input ~input_type:`Submit ~value:"Send" ();
        ]]) ()

let setting_replay_service_html () =
  let form_div = div ~a:[a_class ["form_div"]] [setting_form ()] in
  html header
    (body
       [a ~service:Server_service.main_service
	   [pcdata "Come back to main service"] ();
	br ();
	form_div])

(* Starting replay *)

let replay_header_elt =
  D.div ~a:[a_class["header_div"; "unselectable"]]
    [a ~a:[a_class["tool_button"; "right_button"];
	   a_target "_blank";
	   a_title "Replay again"]
       ~service:Server_service.setting_replay_service
       [img ~alt:("Replay Logo")
           ~src:(make_uri
                   ~service:(Eliom_service.static_dir ())
                   ["img"; "replay.png"])
           ()] ();
     a ~a:[a_class["tool_button"];
	   a_target "_blank";
	   a_title "Information"]
       ~service:Server_service.information_service
       [pcdata "I"] ();
     a ~a:[a_class["tool_button"];
	   a_target "_blank";
	   a_title "Come back to Graffiti"]
       ~service:Server_service.main_service
       [pcdata "G"] ()]

let starting_replay_service_html () =
  let body_elt = D.body
    [replay_header_elt;
     div ~a:[a_id "canvas"] [canvas_elt; canvas2_elt; angle_elt];
     gray_layer_elt; about_elt; starting_logo_elt]
  in
  (html header body_elt), body_elt, header_elt, canvas_elt, canvas2_elt,
  angle_elt, gray_layer_elt, about_elt, starting_logo_elt

let starting_replay_service_error_html () =
  (html header
     (body [h3 [pcdata "Invalide format or data."];
            br ();
            div [pcdata "Please make sure to format date like this '01/01/1997',"];
            div [pcdata "and to format time like this '13:42'"];
            br ();
            div [pcdata "Becareful also to put the smallest date as starting date,"];
            div [pcdata "and to put the biggest date as ending date,"];
            br ();
            a ~service:Server_service.setting_replay_service
              [pcdata "Try again"] ()
           ]))
