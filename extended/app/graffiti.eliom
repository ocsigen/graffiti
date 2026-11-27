open%server Eliom.Content.Html.D
open%server Common
open%client Client
open%server Server

let%server counter = ref 0

let%server () =
  Connected.register ~service:multigraffiti_service !% (fun name () _username ->
    (* Some browsers won't reload the image, so we force
       them by changing the url each time. *)
    incr counter;
    let image =
      img ~alt:name
        ~src:(make_uri ~service:imageservice (name, !counter)) ()
    in
    let slider =
      Form.input
        ~a:[
          a_id "slider";
          a_input_min (`Number 1);
          a_input_max (`Number 80)
        ]
        ~input_type:`Range
        Form.int
    in
    let canvas =
      canvas ~a:[a_width width; a_height height]
        [txt "your browser doesn't support canvas"; br (); image]
    in
    let bus = get_bus name in
    ignore [%client
      (let canceller =
         launch_client_canvas ~%bus ~%image ~%canvas ~%slider
       in
       Eliom.Client.onunload (fun () -> stop_drawing canceller)
       : unit)
    ];
    make_page
      [h1 [txt name];
       disconnect_box ();
       choose_drawing_form ();
       canvas;
       div [slider]])
