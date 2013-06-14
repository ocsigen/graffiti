open Eliom_content.Html5
open Eliom_content.Html5.F

let start_width = 400
let start_height = 200

let canvas_elt =
  D.canvas ~a:[a_width start_width; a_height start_height;
             a_class["unselectable"]]
    [pcdata "your browser doesn't support canvas"]


(* menu elements *)

(* links not work *)

let about_link_elt =
  Raw.a
    ~a:[a_href (Xml.uri_of_string "http://ocsigen.org");
        a_target "_blank"]
    [pcdata "Ocsigen"]

let about_elt =
  D.div ~a:[a_class["about_block"]]
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
     div [pcdata "Created with "; about_link_elt]
    ]

let gray_layer_elt =
  D.div ~a:[a_class["gray_layer"]] []

let save_option_elt =
  tr [td ~a:[a_class["menu_option"]]
         [a ~a:[Unsafe.string_attrib "download" "graffiti.png"]
             ~service:Server_image.download_imageservice
             [pcdata "Save"] ()]]

let about_option_elt = D.td ~a:[a_class["menu_option"]] [pcdata "About"]

let menu_table_elt = table ~a:[a_class["menu_table"]]
  (tr []) [save_option_elt; tr [about_option_elt] ]

let menu_div = D.div ~a:[a_class["menu_div"; "unselectable"]] [menu_table_elt]

(** change image in css with class menu_button **)
let menu_button_elt =
  D.td ~a:[a_class["menu_button"]] []

let menu_elements = [menu_div; gray_layer_elt; about_elt]

(* palette *)

let color_picker, color_div, block = Color_picker.create
  ~lll_color:Color_picker.lll_color_p2' ()

let td_block = td ~a:[a_class["td_block"]] [block]

let slider_elt = D.raw_input ~input_type:`Range ~value:"10"
  ~a:[a_class["brush_slider"](* ; a_input_min 1; a_input_max 100 *)] ()

let td_slider = td ~a:[a_class["td_slider"]] [slider_elt]

let palette_table_elt = table ~a:[a_class["palette_table"]]
  (tr []) [tr [td_block; td_slider]]

let palette_div = D.div ~a:[a_class["palette_div"; "unselectable"]]
  [palette_table_elt]

let palette_button_elt =
  D.td ~a:[a_class["palette_button"]] [color_div]

let palette_elements = [palette_div]

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

(* header / body *)

let header_elt =
  D.table ~a:[a_class["header_table"; "unselectable"]]
    (tr
       [palette_button_elt;
        td ~a:[a_class["header_center_td"]] [];
        menu_button_elt])
    []

let body_elt = D.body ~a:[a_class["unselectable"]]
  ([header_elt; canvas_elt; starting_logo_elt]@
      palette_elements@menu_elements)
