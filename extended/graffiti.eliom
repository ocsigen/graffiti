(* Graffiti
 * http://www.ocsigen.org/graffiti
 * Copyright (C) 2013 Vincent Balat
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

[%%shared
  open Eliom_content
  open Common
]

[%%client
  open Client
]

open Server
open Feed

let start_drawing name image canvas slider =
  let bus = get_bus name in
  ignore [%client
    (let canceller =
       launch_client_canvas ~%bus ~%image ~%canvas ~%slider
     in
     Eliom_client.onunload (fun () -> stop_drawing canceller)
     : unit)
  ]

let counter = ref 0

let () =
  Connected.register ~service:multigraffiti_service
    !% ( fun name () username ->
        (* Some browsers won't reload the image, so we force
           them by changing the url each time. *)
        incr counter;
        let image =
          Html.D.img ~alt:name
            ~src:(Html.D.make_uri ~service:imageservice (name, !counter)) ()
        in
        let slider = Html.D.Form.input
            ~a:[
              Html.D.a_id "slider";
              Html.D.a_input_min (`Number 1);
              Html.D.a_input_max (`Number 80)
            ]
            ~input_type:`Range Html.D.Form.int in
        let canvas =
          Html.D.canvas
            ~a:[Html.D.a_width width; Html.D.a_height height ]
            [ Html.D.pcdata "your browser doesn't support canvas";
              Html.D.br (); image]
        in
        let%lwt save_box = if name = username
          then save_image_box name
          else Lwt.return (Html.D.pcdata "no saving")
        in
        start_drawing name image canvas slider;
        make_page
          [ Html.D.h1 [ Html.D.pcdata name];
            disconnect_box ();
            choose_drawing_form ();
            Html.D.a feed_service [Html.D.pcdata "atom feed"] name;
            Html.D.div [save_box];
            canvas;
            Html.D.div [slider]])
