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

open Eliom_content.Html5.F
open Server_html

(* Wait end of initialization of server to return html page of app *)
let get_main_html_service () =
  match (Lwt.state Server_image.replay_no_save_drawing) with
    | Lwt.Return ()     -> (* In normal case *)
      let html_elt, main_record = Server_html.main_service_html () in
      (* init client *)
      ignore {unit Lwt.t{ Client_core.initialize %main_record }};
      html_elt
    | _                 -> (* During initialize process *)
      Server_html.tmp_service_html ()

let _ =
  Server_service.My_app.register
    ~service:Server_service.main_service
    (fun () () -> Lwt.return (get_main_html_service ()))

let _ =
  Server_service.My_app.register
    ~service:Server_service.information_service
    (fun () () -> Lwt.return (Server_html.info_service_html ()))

let _ =
  Server_service.My_app.register
    ~service:Server_service.setting_replay_service
    (fun () () -> Lwt.return (Server_html.setting_replay_service_html ()))

let _ =
  Server_service.My_app.register
    ~service:Server_service.start_replay_service
    (fun () (d1, (t1, (d2, (t2, (coef_to_replay, hts))))) ->
      try
        let dt1 = Server_tools.datetime_of_jsdatetime d1 t1 in
        let dt2 = Server_tools.datetime_of_jsdatetime d2 t2 in
        let dv1 = Server_tools.get_date_value dt1 in
        let dv2 = Server_tools.get_date_value dt2 in
        let s1 = Server_tools.sec_of_date dv1 in
        let s2 = Server_tools.sec_of_date dv2 in
        if (s1 >= s2)
        then failwith "Starting datetime have to be smaller than ending datetime";

        (* Have to change by channel *)
	let s, s_push_function = Lwt_stream.create () in
        let channel = Eliom_comet.Channel.create s in

        let write data =
          Lwt.return (s_push_function (Some data))
        in
        let coef =
          if not (coef_to_replay = 0)
          then 1. /. (float_of_int coef_to_replay)
          else float_of_int coef_to_replay
        in
        let bool_hts = match hts with
          | Some _      -> true
          | _           -> false
        in
        Lwt.async (fun () ->
          Server_image.replay_drawing
            ~coef_to_replay:coef
            ~skip_hts:bool_hts
            dt1 dt2 write);

        let html_elt, sr_record = Server_html.starting_replay_service_html () in
        ignore {unit Lwt.t{ Client_replay.initialize %channel %sr_record }};
        Lwt.return (html_elt)
      with e    ->
        Lwt.return (Server_html.starting_replay_service_error_html ()))
