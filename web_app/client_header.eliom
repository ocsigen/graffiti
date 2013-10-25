(* Graffiti
 * http://www.ocsigen.org/graffiti
 * Copyright (C) 2013 Arnaud Parant
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

{client{

  (** launch check to remove header and get it's height **)
  let get_height body_elt header_elt =
    if (Client_mobile.remove_header body_elt header_elt)
    then 0
    else let dom_header = Eliom_content.Html5.To_dom.of_div header_elt
         in dom_header##clientHeight

  (** Handle switching logo on each reload of page **)
  let rand_logo body_elt header_elt =

    Random.init (int_of_float (Ojw_tools.get_timestamp ()));

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
