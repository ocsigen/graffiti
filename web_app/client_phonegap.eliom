{client{

  open Lwt

  let on_phonegap () =
    (Js.Unsafe.eval_string "'device' in window") &&
    (Js.Unsafe.eval_string "'cordova' in window.device")

  let launch_on_phonegap func =
    if on_phonegap ()
    then func ()
    else ()

  let not_launch_on_phonegap func =
    if on_phonegap ()
    then ()
    else func ()

  let download_file uri =
    let js_null_func = Js.Unsafe.eval_string "function (p) {}" in
    let transfer = Js.Unsafe.eval_string "new FileTransfer()" in
    (Js.Unsafe.coerce transfer)##download(uri, "Graffiti/graffiti.png",
                                          js_null_func, js_null_func, true)

  (*** Events ***)

  let deviceready:(#Dom_html.event as 'a) Js.t Dom_html.Event.typ =
    Dom_html.Event.make "deviceready"

  let ondeviceready () =
    Lwt_js_events.make_event deviceready Dom_html.document

  (** ifpg series mind if on phonegap do it action else directly return *)
  let ondeviceready_ifpg () =
    if on_phonegap ()
    then (ondeviceready () >>= fun ev -> Lwt.return (Some ev))
    else Lwt.return (None)

  let onload_ondeviceready_ifpg () =
    lwt _ = Lwt_js_events.onload () in
    ondeviceready_ifpg ()

}}
