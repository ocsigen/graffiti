
{client{

  open Lwt

  (*** General tools ***)

  type orientation = Portrait | Landscape

  let get_device_size () =
    let scr = Dom_html.window##screen in
    scr##width, scr##height

  let get_size () =
    let doc = Dom_html.document##documentElement in
    doc##clientWidth, doc##clientHeight

  let get_timestamp () =
    (let date = jsnew Js.date_now () in
     int_of_float (Js.to_float (date##getTime ())))


  (*** Smartphone's tool events ***)

  (** Disable Js event with stopping propagation during capture phase **)
  let disable_event event html_elt =
    (Lwt.async (fun () ->
      Lwt_js_events.seq_loop
        (Lwt_js_events.make_event event) ~use_capture:true html_elt
        (fun ev _ -> Lwt.return (Lwt_js_events.preventDefault ev)) ))

  (* drag events *)
  let disable_drag_and_drop html_elt =
    (disable_event Dom_html.Event.drag html_elt;
     disable_event Dom_html.Event.dragstart html_elt;
     disable_event Dom_html.Event.dragenter html_elt;
     disable_event Dom_html.Event.drop html_elt)

  (* mobile scroll events *)
  let disable_mobile_scroll body_elt =
    (disable_event Dom_html.Event.touchmove body_elt)

  (*** window resize ***)
  let window_resize_function = ref []

  (** Add func to launch at window resize event **)
  (** The id is essential to could remove it **)
  let add_window_resize_function ((id : int), (func : unit -> unit)) =
    window_resize_function := (id, func)::!window_resize_function

  (** Remove func assossiate to id, to launch at window resize event **)
  let delete_window_resize_function id_to_rm =
    let rec aux new_list = function
      | (id, func)::q when id = id_to_rm -> aux new_list q
      | h::q                             -> aux (h::new_list) q
      | []                               -> new_list
    in window_resize_function := aux [] !window_resize_function

  (** catch window_resize and launch associate function **)
  let _ =
    let launch_window_resize_function () =
      let rec aux = function
        | (_, func)::q        -> func (); aux q
        | []                  -> ()
      in aux !window_resize_function
    in
    let handler =
      (fun _ _ -> Lwt.return (launch_window_resize_function ()))
    in
    (Lwt.async (fun () -> Lwt_js_events.onresizes handler));
    (Lwt.async (fun () ->
      Lwt_js_events.seq_loop
        (Lwt_js_events.make_event (Dom.Event.make "orientationchange"))
        Dom_html.document handler))

}}
