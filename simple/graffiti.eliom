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

open%shared Eliom_content
open%client Js_of_ocaml
open%client Js_of_ocaml_lwt

module%server Graffiti_app =
  Eliom_registration.App (
  struct
    let application_name = "graffiti"
    let global_data_path = None
  end)

let%server width  = 700
let%server height = 400

type%shared messages = ((int * int * int) * int * (int * int) * (int * int))
[@@deriving json]

let%server bus = Eliom_bus.create [%json: messages]

let%server draw_server, image_string =
  let rgb_ints_to_floats (r, g, b) =
    float r /. 255., float g /. 255., float b /. 255.
  in
  (* needed by cairo *)
  let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w:width ~h:height in
  let ctx = Cairo.create surface in
  ( (fun (rgb, size, (x1, y1), (x2, y2)) ->
        (* Set thickness of brush *)
        let r, g, b = rgb_ints_to_floats rgb in
        Cairo.set_line_width ctx (float size);
        Cairo.set_line_join ctx Cairo.JOIN_ROUND;
        Cairo.set_line_cap ctx Cairo.ROUND;
        Cairo.set_source_rgb ctx r g b;
        Cairo.move_to ctx (float x1) (float y1);
        Cairo.line_to ctx (float x2) (float y2);
        Cairo.Path.close ctx;
        (* Apply the ink *)
        Cairo.stroke ctx)
  , fun () ->
    let b = Buffer.create 10000 in
    (* Output a PNG in a string *)
    Cairo.PNG.write_to_stream surface (Buffer.add_string b);
    Buffer.contents b )

let%server _ = Lwt_stream.iter draw_server (Eliom_bus.stream bus)

let%server imageservice =
  Eliom_registration.String.create
    ~path:(Eliom_service.Path ["image"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    (fun () () -> Lwt.return (image_string (), "image/png"))

let%client draw ctx ((r, g, b), size, (x1, y1), (x2, y2)) =
  let color = CSS.Color.string_of_t (CSS.Color.rgb r g b) in
  ctx##.strokeStyle := (Js.string color);
  ctx##.lineWidth := float size;
  ctx##beginPath;
  ctx##(moveTo (float x1) (float y1));
  ctx##(lineTo (float x2) (float y2));
  ctx##stroke

let%server canvas_elt =
  Html.D.canvas ~a:[Html.D.a_width width; Html.D.a_height height]
    [Html.D.txt "your browser doesn't support canvas"]

let%server slider =
  Eliom_content.Html.D.Form.input
    ~a:
      [ Html.D.a_id "slider"
      ; Html.D.a_class ["slider"]
      ; Html.D.a_input_min (`Number 1)
      ; Html.D.a_input_max (`Number 80)
      ; Html.D.a_value "22" ]
    ~input_type:`Range Html.D.Form.int

let%server page () =
  let colorpicker, cp_sig =
    Ot_color_picker.make ~a:[Html.D.a_class ["colorpicker"]] ()
  in
  ( Html.D.html
      (Html.D.head
         (Html.D.title (Html.D.txt "Graffiti"))
         [ Html.D.css_link
             ~uri:
               (Html.D.make_uri
                  ~service:(Eliom_service.static_dir ())
                  ["css"; "graffiti.css"])
             ()
         ; Html.D.css_link
             ~uri:
               (Html.D.make_uri
                  ~service:(Eliom_service.static_dir ())
                  ["css"; "ot_color_picker.css"])
             () ])
      (Html.D.body [canvas_elt; slider; colorpicker])
  , cp_sig )

let%client init_client ~cp_sig () =

  let canvas = Eliom_content.Html.To_dom.of_canvas ~%canvas_elt in
  let ctx = canvas##(getContext (Dom_html._2d_)) in
  ctx##.lineCap := Js.string "round";

  (* The initial image: *)
  let img = Eliom_content.Html.To_dom.of_img
      (Html.D.img ~alt:"canvas"
         ~src:(Html.D.make_uri ~service:~%imageservice ())
         ())
  in
  img##.onload := Dom_html.handler (fun _ev ->
      ctx##drawImage img 0. 0.; Js._false);

  let x = ref 0 and y = ref 0 in

  let set_coord ev =
    let x0, y0 = Dom_html.elementClientPosition canvas in
    x := ev##.clientX - x0; y := ev##.clientY - y0
  in

  let compute_line ev =
    let oldx = !x and oldy = !y in
    set_coord ev;
    let size_slider = Eliom_content.Html.To_dom.of_input ~%slider in
    let size = int_of_string (Js.to_string size_slider##.value) in
    let h, s, v = Eliom_shared.React.S.value cp_sig in
    let r, g, b = Ot_color_picker.hsv_to_rgb h s v in
    let rgb = int_of_float r, int_of_float g, int_of_float b in
    (rgb, size, (oldx, oldy), (!x, !y))
  in

  let line ev =
    let v = compute_line ev in
    let _ = Eliom_bus.write ~%bus v in
    draw ctx v;
    Lwt.return ()
  in

  Lwt.async (fun () ->
      let open Lwt_js_events in
      mousedowns canvas
        (fun ev _ ->
           set_coord ev;
           let%lwt () = line ev in
           Lwt.pick
             [mousemoves Dom_html.document (fun x _ -> line x);
              let%lwt ev = mouseup Dom_html.document in line ev]));

  Lwt.async (fun () -> Lwt_stream.iter (draw ctx) (Eliom_bus.stream ~%(bus : (messages, messages) Eliom_bus.t)))

let%server _main_service =
  Graffiti_app.create
    ~path:(Eliom_service.Path [""])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    (fun () () ->
       (* Cf. section "Client side side-effects on the server" *)
       let page, cp_sig = page () in
       let _ = [%client (init_client ~cp_sig:~%cp_sig () : unit) ] in
       Lwt.return page)
