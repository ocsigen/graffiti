
{client{

  (** Handle switching logo on each reload of page **)
  let rand_logo () =

    Random.init (Client_tools.get_timestamp ());
    let logo_list = ["blue"; "green"; "purple"; "yellow"] in
    let size = Client_tools.get_device_size () in
    let width = fst size and height = snd size in
    let str_logo = "/img/Logo_GRAFFITIbyOcsigen_" ^
      (if (width <= 480 || height <= 800)
       then "small_"    (* mobile screen *)
       else ""          (* normal screen *))
    in

    let rand_img =
      "url('" ^ str_logo ^
        (List.nth logo_list
           (Random.int
              (List.length logo_list)))
      ^ ".png')"
    in

    let dom_header = Eliom_content.Html5.To_dom.of_table %Server_html.header_elt
    in

    dom_header##style##backgroundImage <- (Js.string rand_img)

}}
