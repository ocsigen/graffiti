
{client{

  let draw ctx base_size (width, height) (color, size, (x1, y1), (x2, y2)) =
    ctx##strokeStyle <- (Js.string color);
    ctx##lineWidth <- (size *. base_size);
    ctx##beginPath();
    ctx##moveTo(x1 *. width, y1 *. height);
    ctx##lineTo(x2 *. width, y2 *. height);
    ctx##stroke()

  (** Calcul and set size of canvas **)
  let init body_elt header_elt canvas_elt angle_elt =

    (*** Init data ***)
    let size = Client_tools.get_document_size () in
    let dom_canvas = Eliom_content.Html5.To_dom.of_canvas canvas_elt in
    let dom_angle = Eliom_content.Html5.To_dom.of_div angle_elt in
    let width_canvas_margin = if Client_mobile.has_small_screen ()
        then 0
        else 106
    in
    let margin = 2 in
    let width = (fst size) - (margin * 2) - width_canvas_margin in
    let height = (snd size) - (margin * 4) -
      (Client_header.get_height body_elt header_elt)
    in

    (*** Tool ***)
    (** if max = true, set max size, else, set min size **)
    let set_size max value =
      if ((width <= height && max) ||
             width > height && not max)
      then width, value
      else value, height
    in

    (*** First calcul ***)
    let window_orientation = Client_tools.get_window_orientation () in
    let min = if (width <= height) then width else height in
    let max = Shared_tools.get_max_resolution min in

    (*** Check result ***)
    (* If max value is out of window, it is wrong *)
    let good_result = if (width <= height)
      then (height >= max)
      else (width >= max)
    in

    (* Try the other way if result is not a good_result *)
    let width', height' = if (not good_result)
      then (
        let max = if (width > height) then width else height in
        let min = Shared_tools.get_min_resolution max in

        (* Second way set *)
        set_size false min)

      (* First way set *)
      else (set_size true max)
    in

    (* Init canvas *)
    dom_canvas##width <- width';
    dom_canvas##height <- height';

    (* Set position angle *)
    let width_angle' =
      let ox, _ = Dom_html.elementClientPosition dom_canvas in
      let width_angle = dom_angle##clientWidth in
      Js.string (string_of_int (ox + width' - width_angle + 1) ^ "px")
    in dom_angle##style##left <- width_angle';

    (* return result *)
    (window_orientation, (width', height'))

}}
