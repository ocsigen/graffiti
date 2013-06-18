{client{

  (** launch check to remove header and get it's height **)
  let get_height body_elt header_elt =
    if (Client_mobile.remove_header body_elt header_elt)
    then 0
    else let dom_header = Eliom_content.Html5.To_dom.of_div header_elt
         in dom_header##clientHeight

  (** Handle switching logo on each reload of page **)
  let rand_logo body_elt header_elt =

    Random.init (Client_tools.get_timestamp ());

    let logo_list = ["blue"; "green"; "purple"; "yellow"] in
    let rand_img =
      "url('/img/Logo_GRAFFITIbyOcsigen_" ^
        (List.nth logo_list
           (Random.int
              (List.length logo_list)))
      ^ ".png')"
    in

    if Client_mobile.remove_header body_elt header_elt
    then ()     (* Not rand on small screen because of header's removed *)
    else
      (let dom_header =
         Eliom_content.Html5.To_dom.of_div header_elt
       in dom_header##style##backgroundImage <- (Js.string rand_img))

}}
