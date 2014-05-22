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

open Common
open Eliom_content

let draw ctx ((r, g, b), size, (x1, y1), (x2, y2)) =
  let color = CSS.Color.string_of_t (CSS.Color.rgb r g b) in
  ctx##strokeStyle <- (Js.string color);
  ctx##lineWidth <- float size;
  ctx##beginPath();
  ctx##moveTo(float x1, float y1);
  ctx##lineTo(float x2, float y2);
  ctx##stroke()

(* type containing all informations we need to stop interaction
   inside the page *)
type drawing_canceller =
    { message_thread : unit Lwt.t;
      (* the thread reading messages from the bus *)
      drawing_thread : unit Lwt.t;
      (* the arrow handling mouse events *)
    }

let stop_drawing { message_thread; drawing_thread } =
  Lwt.cancel message_thread;
  (* cancelling this thread also close the bus *)
  Lwt.cancel drawing_thread

let launch_client_canvas bus image_elt canvas_elt slider =
  let canvas = Html5.To_dom.of_canvas canvas_elt in
  let ctx = canvas##getContext (Dom_html._2d_) in
  ctx##lineCap <- Js.string "round";

  let img = Html5.To_dom.of_img image_elt in
  let copy_image () = ctx##drawImage(img, 0., 0.) in
  if Js.to_bool (img##complete)
  then copy_image ()
  else img##onload <- Dom_html.handler
    (fun ev -> copy_image (); Js._false);


  (* The color palette: *)
  let colorpicker = Ojw_color_picker.create ~width:150 () in
  Ojw_color_picker.append_at (Dom_html.document##body) colorpicker;
  Ojw_color_picker.init_handler colorpicker;

  let x = ref 0 and y = ref 0 in
  let set_coord ev =
    let x0, y0 = Dom_html.elementClientPosition canvas in
    x := ev##clientX - x0; y := ev##clientY - y0 in
  let compute_line ev =
    let oldx = !x and oldy = !y in
    set_coord ev;
    let rgb = Ojw_color_picker.get_rgb colorpicker in
    let size_slider = Html5.To_dom.of_input slider in
    let size = int_of_string (Js.to_string size_slider##value) in
    (rgb, size, (oldx, oldy), (!x, !y))
  in
  let line ev =
    let v = compute_line ev in
    let _ = Eliom_bus.write bus v in
    draw ctx v;
    Lwt.return ()
  in
  let t = Lwt_stream.iter (draw ctx) (Eliom_bus.stream bus) in
  let drawing_thread =
    Lwt_js_events.(
      mousedowns canvas (fun ev elt ->
        Dom.preventDefault ev;
        set_coord ev;
        lwt () = line ev in
        Lwt.pick [mousemoves Dom_html.document (fun a _ -> line a);
	          lwt ev = mouseup Dom_html.document in line ev]))
  in
  { message_thread = t;
    drawing_thread = drawing_thread }
