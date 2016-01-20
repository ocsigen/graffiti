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
     Eliom_client.onunload (fun () -> stop_drawing canceller; None)
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
          Html5.D.img ~alt:name
            ~src:(Html5.D.make_uri ~service:imageservice (name, !counter)) ()
        in
        let slider = Html5.D.Form.input
            ~a:[
              Html5.D.a_id "slider";
              Html5.D.a_input_min 1.;
              Html5.D.a_input_max 80.
            ]
            ~input_type:`Range Html5.D.Form.int in
        let canvas =
          Html5.D.canvas
            ~a:[Html5.D.a_width width; Html5.D.a_height height ]
            [ Html5.D.pcdata "your browser doesn't support canvas";
              Html5.D.br (); image]
        in
        let%lwt save_box = if name = username
          then save_image_box name
          else Lwt.return (Html5.D.pcdata "no saving")
        in
        start_drawing name image canvas slider;
        make_page
          [ Html5.D.h1 [ Html5.D.pcdata name];
            disconnect_box ();
            choose_drawing_form ();
            Html5.D.a feed_service [Html5.D.pcdata "atom feed"] name;
            Html5.D.div [save_box];
            canvas;
            Html5.D.div [slider]])
