(* Graffiti
 * http://www.ocsigen.org/graffiti
 * Copyright (C) 2013 Arnaud Parant
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

{shared{

type 'a canvas_type =
    {canvas1 : [`Canvas of 'a] Eliom_content.Html5.elt;
     canvas2 : [`Canvas of 'a] Eliom_content.Html5.elt;
     angle: [Html5_types.div] Eliom_content.Html5.elt;
     about_point: [Html5_types.div] Eliom_content.Html5.elt}

type 'a save_type =
    {save_button : [Html5_types.div] Eliom_content.Html5.elt;
     save_link: ['a Html5_types.a] Eliom_content.Html5.elt}

type palette_type =
    {palette_wrapper: [Html5_types.div] Eliom_content.Html5.elt;
     palette_button: [`Table] Eliom_content.Html5.elt;
     ew_slider: Ew_slider.t;
     color_picker: Ew_table_color_picker.t;
     color_div: [Html5_types.div] Eliom_content.Html5.elt}

type main_type =
    {body: [`Body] Eliom_content.Html5.elt;
     header: [Html5_types.div] Eliom_content.Html5.elt}

type 'a ms_type =
    {ms_main: main_type;
     ms_canvas: 'a canvas_type;
     ms_save: 'a save_type;
     ms_palette: palette_type;
     ms_gray_layer: [Html5_types.div] Eliom_content.Html5.elt;
     ms_about: [Html5_types.div] Eliom_content.Html5.elt;
     ms_starting_logo: [`Table] Eliom_content.Html5.elt}

type 'a sr_type =
    {sr_main: main_type;
     sr_canvas: 'a canvas_type;
     sr_gray_layer: [Html5_types.div] Eliom_content.Html5.elt;
     sr_about: [Html5_types.div] Eliom_content.Html5.elt;
     sr_starting_logo: [`Table] Eliom_content.Html5.elt}

}}

open Eliom_content.Html5
open Eliom_content.Html5.F

let start_width = 400
let start_height = 200

(* about elements *)

let about_point () =
  D.div ~a:[a_class["about_point"]] []

let about_link () =
  Raw.a
    ~a:[a_href (Xml.uri_of_string "http://ocsigen.org");
        a_target "_blank"]
    [pcdata "Ocsigen"]

let about () =
  D.div ~a:[a_class["about_wrapper"]]
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
                   div [pcdata "Created with "; about_link ()]]]]) []]

let gray_layer () =
  D.div ~a:[a_class["gray_layer"]] []

(* canvas element *)

let canvas () =
  D.canvas ~a:[a_width start_width; a_height start_height;
             a_class["unselectable"]]
    [pcdata "your browser doesn't support canvas"]

let canvas2 () =
  D.canvas ~a:[a_width start_width; a_height start_height;
               D.Unsafe.string_attrib "draggable" "false";
               a_class["canvas2"; "unselectable"]] []


let angle () =
  let about_point_elt = about_point () in
  D.div ~a:[a_class["angle_div"]] [about_point_elt],
  about_point_elt

let canvas_wrap () =
  let canvas_elt = canvas () in
  let canvas2_elt = canvas2 () in
  let angle_elt, about_point_elt = angle () in
  div ~a:[a_id "canvas"] [canvas_elt; canvas2_elt; angle_elt],
  canvas_elt, canvas2_elt, angle_elt, about_point_elt

(* save elements *)

(** change image in css with class save_button *)
let save_link () =
  D.a
    ~service:Server_image.download_imageservice
    ~a:[a_class["save_link"]; D.Unsafe.string_attrib "download" "graffiti.png"]
    [div ~a:[a_class["save_div"]] []] ()

let save_button () =
  let save_link_elt = save_link () in
  D.div ~a:[a_class["save_button"]] [save_link_elt],
  save_link_elt

(* palette *)

let palette () =
  let color_picker, color_div, block_elt = Ew_table_color_picker.create
    ~initial_color:(0, 3, 0) ~lll_color:Ew_table_color_picker.lll_color_10 ()
  in
  let ew_slider, slider_elt = Ew_slider.create
    ~orientation:Ew_slider.Vertical ?initial_value:(Some 0.8) ()
  in
  let palette_button_elt =
    D.table ~a:[a_class["palette_button"]]
      (tr [td ~a:[a_class["max_height"]] [color_div]]) []
  in
  let palette_wrapper_elt =
    D.div ~a:[a_class["palette_wrap"]]
      [table (tr ~a:[a_class["max_height"]] [td [slider_elt]; td [block_elt]]) [];
       palette_button_elt]
  in
  palette_wrapper_elt,
  palette_button_elt,
  ew_slider,
  color_picker,
  color_div

(* starting logo *)

let starting_logo () =
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

let manifest_uri =
  Xml.uri_of_string "graffiti.appcache"

let header =
  Eliom_tools.F.head ~title:"Graffiti"
    ~css:[["css"; "ew_table_color_picker.css"];
          ["css"; "ew_slider.css"];
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

let main_header () =
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

let main_service_html () =
  let header_elt = main_header () in
  let canvas_wrap_elt, canvas_elt, canvas2_elt, angle_elt, about_point_elt =
    canvas_wrap ()
  in
  let save_button_elt, save_link_elt = save_button () in
  let palette_wrapper_elt, palette_button_elt,
    ew_slider, color_picker, color_div =
    palette ()
  in
  let gray_layer_elt = gray_layer () in
  let about_elt = about () in
  let starting_logo_elt = starting_logo () in
  let body_elt =
    D.body ~a:[a_class["unselectable"]]
      [header_elt;
       canvas_wrap_elt;
       save_button_elt;
       palette_wrapper_elt;
       gray_layer_elt;
       about_elt;
       starting_logo_elt]
  in
  (html ~a:[a_manifest manifest_uri] header body_elt),
  {ms_main = {body = body_elt;
              header = header_elt};
   ms_canvas = {canvas1 = canvas_elt;
		canvas2 = canvas2_elt;
		angle = angle_elt;
		about_point = about_point_elt};
   ms_save = {save_button = save_button_elt;
              save_link = save_link_elt};
   ms_palette = {palette_wrapper = palette_wrapper_elt;
		 palette_button = palette_button_elt;
		 ew_slider = ew_slider;
		 color_picker = color_picker;
		 color_div = color_div};
   ms_gray_layer = gray_layer_elt;
   ms_about = about_elt;
   ms_starting_logo = starting_logo_elt}


(* Intialize process Elements *)
let tmp_body () =
  body [div [pcdata "Graffiti is in initialize process."];
        div [pcdata "Try again in a few moment."];
        br ();
        a ~service:Server_service.main_service
          [pcdata "Try again"] ()]

let tmp_service_html () =
  html header (tmp_body ())

(* Information service *)
let info_body () =
  body ~a:[a_class["information_body"]]
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
     div [pcdata "y2: float, y destination, relative to height, less or equal than 1, more or equal than 0"]]

let info_service_html () =
  html header (info_body ())


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

let replay_header () =
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
  let header_elt = replay_header () in
  let canvas_wrap_elt, canvas_elt, canvas2_elt, angle_elt, about_point_elt =
    canvas_wrap ()
  in
  let gray_layer_elt = gray_layer () in
  let about_elt = about () in
  let starting_logo_elt = starting_logo () in
  let body_elt = D.body [header_elt; canvas_wrap_elt; gray_layer_elt;
                         about_elt; starting_logo_elt]
  in
  (html header body_elt),
  {sr_main = {body = body_elt;
              header = header_elt};
   sr_canvas = {canvas1 = canvas_elt;
		canvas2 = canvas2_elt;
		angle = angle_elt;
		about_point = about_point_elt};
   sr_gray_layer = gray_layer_elt;
   sr_about = about_elt;
   sr_starting_logo = starting_logo_elt}

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
