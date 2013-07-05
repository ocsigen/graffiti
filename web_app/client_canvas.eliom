
{client{

  let draw ctx base_size (width, height) (color, size, (x1, y1), (x2, y2)) =
    ctx##strokeStyle <- (Js.string color);
    ctx##lineWidth <- (size *. base_size);
    ctx##beginPath();
    ctx##moveTo(x1 *. width, y1 *. height);
    ctx##lineTo(x2 *. width, y2 *. height +. 0.1);
    ctx##stroke()

  (** Calcul and set size of canvas **)
  let init_size body_elt header_elt canvas_elt canvas2_elt =

    (*** Init data ***)
    let size = Client_tools.get_document_size () in
    let dom_canvas = Eliom_content.Html5.To_dom.of_canvas canvas_elt in
    let dom_canvas2 = Eliom_content.Html5.To_dom.of_canvas canvas2_elt in
    let width_canvas_margin = if Client_mobile.has_small_screen ()
        then 35
        else 230
    in
    let margin = 6 in
    let min_width = 100 in
    let min_height = Shared_tools.get_min_resolution min_width in
    let max_width = (fst size) - (margin * 2) - width_canvas_margin in
    let max_height = (snd size) - (margin * 2) -
      (Client_header.get_height body_elt header_elt)
    in

    (* limit size *)
    let max_width', max_height' =
      (if (max_width < min_width) then min_width else max_width),
      (if (max_height < min_height) then min_height else max_height)
    in

    (* calcul canvas' size *)
    let width = max_width' in
    let height = Shared_tools.get_min_resolution width in

    let height', width' =
      if height > max_height'
      then max_height', Shared_tools.get_max_resolution max_height'
      else height, width
    in

    (* Init canvas *)
    let init_canvas dom_canvas =
      dom_canvas##width <- width';
      dom_canvas##height <- height'
    in
    init_canvas dom_canvas;
    init_canvas dom_canvas2;

    (* vertical center calcul *)
    let lineHeight = snd size - Client_header.get_height body_elt header_elt in

    (* set vertical center *)
    Dom_html.document##body##style##lineHeight <-
      Client_tools.js_string_of_px lineHeight;

    (* return result *)
    (width', height')

  (** Handle set image in canvas**)
  let init_image ctx bus_mutex (width, height) =

      lwt () = Lwt_mutex.lock bus_mutex in

      let copy_image dom_img =
        ctx##drawImage_withSize(dom_img, 0., 0., width, height);
      in

      let dom_img =
	(* create js image object to avoid long time loading in webkit *)
	let dom_img = Dom_html.createImg Dom_html.document in
        (* allow to avoid cach image *)
        let attr = Client_tools.get_timestamp () in
	dom_img##src <- Js.string (Eliom_content.Html5.F.make_string_uri
				     ~service:%Server_image.imageservice
				     (int_of_float width, attr));
	dom_img
      in

      (* We wait for the image to be loaded before drawing it on canvas *)
      if (Js.to_bool (dom_img##complete))
      then begin
        copy_image dom_img;
        Lwt_mutex.unlock bus_mutex
      end
      else
        Lwt_js_events.(async (fun () ->
          lwt _ = load dom_img in
          copy_image dom_img;
          Lwt_mutex.unlock bus_mutex;
          Lwt.return ()));

      Lwt.return ()

}}
