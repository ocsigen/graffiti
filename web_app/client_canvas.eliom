
{client{

  let draw ctx base_size (width, height) (color, size, (x1, y1), (x2, y2)) =
    ctx##strokeStyle <- (Js.string color);
    ctx##lineWidth <- (size *. base_size);
    ctx##beginPath();
    ctx##moveTo(x1 *. width, y1 *. height);
    ctx##lineTo(x2 *. width, y2 *. height +. 0.1);
    ctx##stroke()

  (** Calcul and set size of canvas **)
  let init body_elt header_elt canvas_elt angle_elt =

    (*** Init data ***)
    let size = Client_tools.get_document_size () in
    let dom_canvas = Eliom_content.Html5.To_dom.of_canvas canvas_elt in
    let dom_angle = Eliom_content.Html5.To_dom.of_div angle_elt in
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
    dom_canvas##width <- width';
    dom_canvas##height <- height';

    (* vertical center calcul *)
    let lineHeight = (snd size) -
      if Client_mobile.has_small_screen () then 0
      else (Client_header.get_height body_elt header_elt)
    in

    (* set vertical center *)
    Dom_html.document##body##style##lineHeight <-
      Client_tools.js_string_of_px lineHeight;

    (* set angle position *)
    let angle_width = dom_angle##clientWidth - 1 in
    let _ =
      let css_margin = 2 in
      let ox, oy = Dom_html.elementClientPosition dom_canvas in
      dom_angle##style##top <- Client_tools.js_string_of_px oy;
      dom_angle##style##left <- Client_tools.js_string_of_px
        (ox + width' + (css_margin * 2) - angle_width)
    in

    (* return result *)
    (width', height')

}}
