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

  open Lwt

  let on_phonegap () =
    (Js.to_bool (Js.Unsafe.eval_string "'device' in window"))

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
    (Js.Unsafe.coerce transfer)##(download
                                    uri ("Graffiti/graffiti.png")
                                    js_null_func js_null_func true)

  (*** Events ***)

  let deviceready:(#Dom_html.event as 'a) Js.t Dom_html.Event.typ =
    Dom_html.Event.make "deviceready"

  let ondeviceready () =
    Lwt_js_events.make_event deviceready Dom_html.document

  (** ifpg series mind if on phonegap do it action else directly return *)
  let ondeviceready_ifpg () =
    if (Js.to_bool (Js.Unsafe.eval_string "'deviceready' in window"))
    then (ondeviceready () >>= fun ev -> Lwt.return (Some ev))
    else Lwt.return (None)

  let onload_ondeviceready_ifpg () =
    let%lwt _ = Lwt_js_events.onload () in
    ondeviceready_ifpg ()

]
