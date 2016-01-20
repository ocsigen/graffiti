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
[%%client

  open Server_html

  (* It is a simple copy of code...
     It need to be recorded, 'modulariser' and 'generiser'

     Some issue list:
       - Canvas are place as main service to let space for color palette.
         But there is not color palette.
       - About button it is not working (?)
         It is realy needed ?
         Because of code which handle that which it not launch (menu)
         But it is handle also save button, not needed here ?
         Not from sever, but from canvas
  *)

  let start body_elt header_elt canvas_elt canvas2_elt server_channel =

    (*** Init data***)
    let size =
      Client_canvas.init_size body_elt header_elt canvas_elt canvas2_elt
    in
    let width = ref (float_of_int (fst size)) in
    let height = ref (float_of_int (snd size)) in
    let float_size = ref (!width, !height) in
    let base_size = ref !height in

    let dom_canvas = Eliom_content.Html5.To_dom.of_canvas canvas_elt in
    let ctx = dom_canvas##(getContext (Dom_html._2d_)) in
    ctx##.lineCap := Js.string "round";

    let bus_draw (color, brush_size, (x1, y1), (x2, y2)) =
      Client_canvas.draw ctx !base_size !float_size
        (color, brush_size, (x1, y1), (x2, y2))
    in

    (* get channel message *)
    Lwt.async (fun () -> Lwt_stream.iter bus_draw server_channel)


  (*** init client to replay ***)
  let initialize server_channel sr_record =
    begin

      (* Remove navigation bar *)
      Ow_mobile_tools.hide_navigation_bar ();

      (* Random logo image *)
      Client_header.rand_logo
         sr_record.sr_main.body
         sr_record.sr_main.header;

      (* start canvas script *)
      ignore (start
         sr_record.sr_main.body
         sr_record.sr_main.header
         sr_record.sr_canvas.canvas1
         sr_record.sr_canvas.canvas2
         server_channel);

      (* Check if 'touch to start' have to be removed (on pc) *)
      Client_mobile.handle_touch_to_start
         sr_record.sr_main.body
         sr_record.sr_starting_logo;

      Lwt.return ()

    end

]
