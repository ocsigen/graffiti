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

open Eliom_content
open Eliom_lib
open Html.F
open Server

let static_dir =
  match Eliom_config.get_config () with
  | [Simplexmlparser.Element ("staticdir", [], [Simplexmlparser.PCData dir])] ->
    dir
  | [] ->
    raise (Ocsigen_extensions.Error_in_config_file
             ("<staticdir> option required for <graffiti>"))
  | _ ->
    raise (Ocsigen_extensions.Error_in_config_file
             ("Unexpected content inside graffiti config"))

let image_dir name =
  let dir = static_dir ^ "/graffiti_saved/" ^ (Url.encode name) in
  (try%lwt Lwt_unix.mkdir dir 0o777 with
   | _ -> debug "could not create the directory %s" dir; Lwt.return ()) >|=
  (fun () -> dir)

let make_filename name number =
  image_dir name >|= ( fun dir -> (dir ^ "/" ^ (string_of_int number) ^ ".png") )

let save image name number =
  let%lwt file_name = make_filename name number in
  let%lwt out_chan = Lwt_io.open_file ~mode:Lwt_io.output file_name in
  Lwt_io.write out_chan image

let image_info_table = Ocsipersist.open_table "image_info_table"

let save_image username =
  let now = CalendarLib.Calendar.now () in
  let%lwt image_info_table = image_info_table in
  let%lwt number,_,list =
    try%lwt Ocsipersist.find image_info_table username with
    | Not_found -> Lwt.return (0,now,[])
    | e -> Lwt.fail e
  in
  let%lwt () = Ocsipersist.add image_info_table
      username (number+1,now,(number,now)::list) in
  let (_,image_string) = Hashtbl.find graffiti_info username in
  save (image_string ()) username number

let save_image_box =
  let save_service_reference =
    Eliom_reference.eref ~scope:Eliom_common.default_group_scope None in
  fun name ->
    let%lwt save_image_service =
      match%lwt Eliom_reference.get save_service_reference with
      | None ->
        let service = Eliom_registration.Action.create
            ~scope:Eliom_common.default_group_scope
            ~meth:(Eliom_service.Post
                     (Eliom_parameter.unit,
                      Eliom_parameter.unit))
            ~path:Eliom_service.No_path
            (fun () () -> save_image name) in
        let%lwt () = Eliom_reference.set save_service_reference (Some service) in
        Lwt.return service
      | Some service -> Lwt.return service
    in
    Lwt.return (
      Html.D.Form.post_form save_image_service
        (fun _ ->
           [p [
               Html.D.Form.input ~input_type:`Submit ~value:"save"
                 Html.D.Form.string]]) ())

let feed_service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["feed"])
    ~meth:(Eliom_service.Get (Eliom_parameter.string "name"))
    ()

let local_filename name number =
  ["graffiti_saved"; Url.encode name ; (string_of_int number) ^ ".png"]

let rec entries name list = function
  | 0 -> []
  | len ->
    match list with
    | [] -> []
    | (n,saved)::q ->
      let title = Atom_feed.plain
          ("graffiti " ^ name ^ " " ^ (string_of_int n)) in
      let uri =
        Html.D.make_uri ~absolute:true
          ~service:(Eliom_service.static_dir ())
          (local_filename name n)
      in
      let entry =
        Atom_feed.entry ~title ~id:(Xml.string_of_uri uri) ~updated:saved
          [Atom_feed.html5C [ Html.F.img ~src:uri ~alt:"image" ()]] in
      entry::(entries name q (len - 1))

let feed name () =
  let id = Xml.string_of_uri (Html.D.make_uri ~absolute:true
                                ~service:feed_service name) in
  let title = Atom_feed.plain ("nice drawings of " ^ name) in
  try%lwt
    let%lwt image_info_table = image_info_table in
    Ocsipersist.find image_info_table name >|=
    (fun (number,updated,list) ->
       Atom_feed.feed ~id ~updated ~title (entries name list 10))
  with
  | Not_found ->
    let now = CalendarLib.Calendar.now () in
    Lwt.return (Atom_feed.feed ~id ~updated:now ~title [])
  | e -> Lwt.fail e

let feed name () =
  let id = Xml.string_of_uri (Html.D.make_uri ~absolute:true ~service:feed_service name) in
  let title = Atom_feed.plain ("nice drawings of " ^ name) in
  Lwt.catch
    (fun () ->
       let%lwt image_info_table = image_info_table in
       Ocsipersist.find image_info_table name >|=
      (fun (number,updated,list) -> Atom_feed.feed ~id ~updated ~title (entries name list 10)))
    ( function Not_found ->
      let now = CalendarLib.Calendar.now () in
      Lwt.return (Atom_feed.feed ~id ~updated:now ~title [])
             | e -> Lwt.fail e )

let () = Eliom_atom.Reg.register
    ~service:feed_service feed
