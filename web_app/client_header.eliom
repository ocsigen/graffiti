{client{

  (** Handle switching logo on each reload of page **)
  let rand_logo () =

    Random.init (Client_tools.get_timestamp ());

    let logo_list = ["blue"; "green"; "purple"; "yellow"] in
    let rand_img =
      "url('/img/Logo_GRAFFITIbyOcsigen_" ^
        (List.nth logo_list
           (Random.int
              (List.length logo_list)))
      ^ ".png')"
    in

    if Client_mobile.remove_header_mobile ()
    then ()     (* Not rand on mobile because of header's removed *)
    else
      (let dom_header =
         Eliom_content.Html5.To_dom.of_div %Server_html.header_elt
       in dom_header##style##backgroundImage <- (Js.string rand_img))

}}
