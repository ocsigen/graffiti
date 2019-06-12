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
open Eliom_lib.Lwt_ops
open Eliom_content

let width = 700
let height = 300

module CSS = Js_of_ocaml.CSS
module Js = Js_of_ocaml.Js
module Dom = Js_of_ocaml.Dom
module Dom_html = Js_of_ocaml.Dom_html]

[%%client
module Lwt_js = Js_of_ocaml_lwt.Lwt_js
module Lwt_js_events = Js_of_ocaml_lwt.Lwt_js_events]

let%server _ =
  Eliom_state.set_global_volatile_data_state_timeout
    ~cookie_scope:Eliom_common.comet_client_process_scope (Some 20.)

module My_appl = Eliom_registration.App (struct
  let application_name = "graffiti"
  let global_data_path = None
end)

let%client draw ?(alpha = 1.) ctx ((r, g, b), size, (x1, y1), (x2, y2)) =
  let color = CSS.Color.string_of_t (CSS.Color.rgb r g b ~a:alpha) in
  ctx##.strokeStyle := Js.string color;
  ctx##.lineWidth := float size;
  ctx##beginPath;
  ctx ## (moveTo (float x1) (float y1));
  ctx ## (lineTo (float x2) (float y2 +. 0.1));
  (* The 0.1 is a fix for Chrome (does not draw lines if the first
     and last points are equal) *)
  ctx##stroke

let%client () =
  let c = Eliom_comet.Configuration.new_configuration () in
  Eliom_comet.Configuration.set_active_until_timeout c true

[%%shared
type messages = (int * int * int) * int * (int * int) * (int * int)
[@@deriving json]

type canvas = Html_types.flow5 Html_types.canvas Eliom_content.Html.elt]

let%server bus : (messages, messages) Eliom_bus.t =
  Eliom_bus.create ~scope:`Site ~name:"grib" ~size:500 [%derive.json: messages]

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

let%server image_elt =
  Html.D.img ~alt:"canvas" ~src:(Html.D.make_uri ~service:imageservice ()) ()

let%server canvas_elt : canvas =
  let open Html.D in
  canvas
    ~a:[a_width width; a_height height; a_class ["undercanvas"]]
    [Html.D.txt "your browser doesn't support canvas"; Html.D.br (); image_elt]

let%server canvas2_elt : canvas =
  Html.D.(canvas ~a:[a_width width; a_height height; a_class ["overcanvas"]] [])

let%server slider_create () =
  let slider_sig, slider_f = Eliom_shared.React.S.create 0 in
  let elt =
    let open Html.F in
    input
      ~a:
        [ a_input_type `Range
        ; a_class ["slider"]
        ; a_input_min (`Number 1)
        ; a_input_max (`Number 80)
        ; a_value "1"
        ; a_oninput
            [%client
              fun ev ->
                match Dom_html.opt_tagged ev##.currentTarget with
                | Some (Dom_html.Input elem) ->
                    ~%slider_f (int_of_string (Js.to_string elem##.value))
                | _ -> ()] ]
      ()
  in
  elt, slider_sig

let%server page () =
  let colorpicker, cp_sig =
    Ot_color_picker.make ~a:[Html.D.a_class ["colorpicker"]]
  in
  let slider, slider_sig = slider_create () in
  ( Html.D.html
      (Html.D.head
         (Html.D.title (Html.D.txt "Graffiti"))
         [ Html.D.css_link
             ~uri:
               (Html.D.make_uri
                  (Eliom_service.static_dir ())
                  ["css"; "graffiti.css"])
             ()
         ; Html.D.css_link
             ~uri:
               (Html.D.make_uri
                  (Eliom_service.static_dir ())
                  ["css"; "ot_color_picker.css"])
             () ])
      (Html.D.body [Html.D.div [canvas_elt; canvas2_elt]; slider; colorpicker])
  , cp_sig
  , slider_sig )

let%client init_client ~cp_sig ~slider_sig =
  let canvas = Html.To_dom.of_canvas ~%canvas_elt in
  let ctx = canvas ## (getContext Dom_html._2d_) in
  ctx##.lineCap := Js.string "round";
  (* Another canvas, for second layer *)
  let canvas2 = Html.To_dom.of_canvas ~%canvas2_elt in
  let ctx2 = canvas2 ## (getContext Dom_html._2d_) in
  ctx2##.lineCap := Js.string "round";
  (* The initial image: *)
  let img = Html.To_dom.of_img ~%image_elt in
  let copy_image () = ctx ## (drawImage img 0. 0.) in
  if Js.to_bool img##.complete
  then copy_image ()
  else img##.onload := Dom_html.handler (fun ev -> copy_image (); Js._false);
  let x = ref 0 and y = ref 0 in
  let set_coord ev =
    let x0, y0 = Dom_html.elementClientPosition canvas in
    x := ev##.clientX - x0;
    y := ev##.clientY - y0
  in
  let compute_line set_coord x y ev =
    let oldx = !x and oldy = !y in
    set_coord ev;
    let size = Eliom_shared.React.S.value slider_sig in
    let h, s, v = Eliom_shared.React.S.value cp_sig in
    let r, g, b = Ot_color_picker.hsv_to_rgb h s v in
    let rgb = int_of_float r, int_of_float g, int_of_float b in
    rgb, size, (oldx, oldy), (!x, !y)
  in
  let bus = ~%bus in
  let line ev =
    let v = compute_line set_coord x y ev in
    let _ = Eliom_bus.write bus v in
    draw ctx v; Lwt.return ()
  in
  ignore
    ( Lwt_js.sleep 0.1 >>= fun () ->
      (* avoid chromium looping cursor *)
      Lwt.catch
        (fun () -> Lwt_stream.iter (draw ctx) (Eliom_bus.stream bus))
        (function
          | e (* Eliom_comet.Channel_full *) ->
              Js_of_ocaml.Firebug.console ## (log e);
              Eliom_client.exit_to ~service:Eliom_service.reload_action () ();
              Lwt.return ()) );
  (let open Lwt_js_events in
  async (fun () ->
      mousedowns canvas2 (fun ev elt ->
          Dom.preventDefault ev;
          set_coord ev;
          let%lwt () = line ev in
          Lwt.pick
            [ mousemoves Dom_html.document (fun a _ -> line a)
            ; (let%lwt ev = mouseup Dom_html.document in
               line ev) ])));
  (* The brush *)
  ctx2##.globalCompositeOperation := Js.string "copy";
  let x, y, size = ref 0, ref 0, ref 0 in
  let set_coord ev =
    let x0, y0 = Dom_html.elementClientPosition canvas2 in
    x := ev##.clientX - x0;
    y := ev##.clientY - y0
  in
  let brush ev _ =
    let color, newsize, oldv, v = compute_line set_coord x y ev in
    draw ~alpha:0. ctx2 ((0, 0, 0), !size + 3, oldv, oldv);
    size := newsize;
    draw ctx2 (color, newsize, v, v);
    Lwt.return ()
  in
  ignore Lwt_js_events.(async (fun () -> mousemoves Dom_html.document brush))

let%server main_service =
  My_appl.create
    ~path:(Eliom_service.Path [""])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    (fun () () ->
      let page, cp_sig, slider_sig = page () in
      ignore
        [%client
          (init_client ~cp_sig:~%cp_sig ~slider_sig:~%slider_sig : unit)];
      Lwt.return page)
