open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.D

let start_width = 400
let start_height = 200

let canvas_elt =
  canvas ~a:[a_width start_width; a_height start_height;
             a_class["unselectable"]]
    [pcdata "your browser doesn't support canvas"]


(* menu elements *)

(* links not work *)

let about_link_elt =
    Html5.F.Raw.a
        ~a:[a_href (Xml.uri_of_string "http://ocsigen.org")]
        [pcdata "Ocsigen"]

let about_elt =
  div ~a:[a_class["about_block"]]
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
  div ~a:[a_class["gray_layer"]] []

let save_option_elt =
  td ~a:[a_class["menu_option"]]
    [a ~service:Server_image.download_service
        ~a:[D.Unsafe.string_attrib "download" "graffiti.png"]
        [pcdata "Save"] ("/img/medium?time=42", ("graffiti.png", "image/png"))]

let about_option_elt =
  td ~a:[a_class["menu_option"]] [pcdata "About"]

let menu_elt =
  div ~a:[a_class["menu_div"; "unselectable"]]
  [table ~a:[a_class["menu_table"]]
      (tr [save_option_elt])
      [tr [about_option_elt]] ]

(** change image in css with class menu_button **)
let menu_button_elt =
  td ~a:[a_class["menu_button"]] []

let menu_elements = [menu_elt; gray_layer_elt; about_elt]

(* starting logo *)

let starting_logo_elt =
  table ~a:[a_class["logo"]]
    (tr [td
            [img ~a:[a_class["logo_img"]]
                ~alt:("Graffiti Logo")
                ~src:(make_uri
                        ~service:(Eliom_service.static_dir ())
                        ["img"; "Logo_GRAFFITIbyOcsigenTouch.png"])
                ()]])
    []

(* header / body *)

let header_center_elt = div []

let header_elt =
  table ~a:[a_class["header_table"; "unselectable"]]
    (tr
       [td ~a:[a_class["header_left_td"]] [];
        menu_button_elt])
    []

let body_elt = body ([header_elt; canvas_elt; starting_logo_elt]@menu_elements)
