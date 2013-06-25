
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

let gray_layer_elt = D.div ~a:[a_class["gray_layer"]] []

(* canvas element *)

let canvas_elt =
  D.canvas ~a:[a_width start_width; a_height start_height;
             a_class["unselectable"]]
    [pcdata "your browser doesn't support canvas"]

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

let color_picker, color_div, block = Color_picker.create
  ~initial_color:(0, 3, 0)
  ~lll_color:Color_picker.lll_color_10 ()

let slider_elt = D.raw_input ~input_type:`Range ~value:"10"
  ~a:[a_class["brush_slider"](* ; a_input_min 1; a_input_max 100 *)] ()

let palette_div = D.div ~a:[a_class["palette_div"; "unselectable"]]
  [div ~a:[a_class["color_div"]] [color_div; block];
   div ~a:[a_class["brush_div"]] [slider_elt]]

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
  D.div ~a:[a_class["header_div"; "unselectable"]] []

let body_elt = D.body ~a:[a_class["unselectable"]]
  ([header_elt; palette_div; canvas_elt; angle_elt;
    save_button_elt; gray_layer_elt; about_elt;
    starting_logo_elt])
