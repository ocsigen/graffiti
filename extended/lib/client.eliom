[%%client.start]

open Js_of_ocaml
open Js_of_ocaml_lwt
open Eliom.Content
open Lwt.Syntax

let draw ctx ((r, g, b), size, (x1, y1), (x2, y2)) =
  let color = CSS.Color.string_of_t (CSS.Color.rgb r g b) in
  ctx##.strokeStyle := (Js.string color);
  ctx##.lineWidth := Js.float (float size);
  ctx##beginPath;
  ctx##(moveTo (Js.float (float x1)) (Js.float (float y1)));
  ctx##(lineTo (Js.float (float x2)) (Js.float (float y2)));
  ctx##stroke

(* type containing all informations we need to stop interaction
   inside the page *)
type drawing_canceller =
    { message_thread : unit Lwt.t;
      (* the thread reading messages from the bus *)
      drawing_thread : unit Lwt.t;
      (* the thread handling mouse events *)
    }

let stop_drawing { message_thread; drawing_thread } =
  Lwt.cancel message_thread;
  Lwt.cancel drawing_thread

let launch_client_canvas bus image_elt canvas_elt slider =
  let canvas = Html.To_dom.of_canvas canvas_elt in
  let ctx = canvas##(getContext (Dom_html._2d_)) in
  ctx##.lineCap := Js.string "round";

  let img = Html.To_dom.of_img image_elt in
  let copy_image () = ctx##(drawImage img (Js.float 0.) (Js.float 0.)) in
  if Js.to_bool (img##.complete)
  then copy_image ()
  else img##.onload := Dom_html.handler
    (fun _ -> copy_image (); Js._false);

  (* The color palette: *)
  let colorpicker, cp_sig = Ot.Color_picker.make () in
  Html.Manip.appendChild (Html.Manip.Elt.body ()) colorpicker;

  let x = ref 0 and y = ref 0 in
  let set_coord ev =
    let x0, y0 = Dom_html.elementClientPosition canvas in
    x := truncate (Js.to_float ev##.clientX) - x0;
    y := truncate (Js.to_float ev##.clientY) - y0
  in
  let compute_line ev =
    let oldx = !x and oldy = !y in
    set_coord ev;
    let h, s, v = Eliom.Shared.React.S.value cp_sig in
    let r, g, b = Ot.Color_picker.hsv_to_rgb h s v in
    let rgb = int_of_float r, int_of_float g, int_of_float b in
    let size_slider = Html.To_dom.of_input slider in
    let size = int_of_string (Js.to_string size_slider##.value) in
    (rgb, size, (oldx, oldy), (!x, !y))
  in
  let line ev =
    let v = compute_line ev in
    let _ = Eliom.Bus.write bus v in
    draw ctx v;
    Lwt.return ()
  in
  let t = Lwt_stream.iter (draw ctx) (Eliom.Bus.stream bus) in
  let drawing_thread =
    Lwt_js_events.(
      mousedowns canvas (fun ev _ ->
        Dom.preventDefault ev;
        set_coord ev;
        let* () = line ev in
        Lwt.pick [mousemoves Dom_html.document (fun a _ -> line a);
                  let* ev = mouseup Dom_html.document in line ev]))
  in
  { message_thread = t;
    drawing_thread = drawing_thread }
