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
open Lwt.Infix
open Html.F
open Server

let static_dir =
  match Eliom_config.get_config () with
  | [Element ("staticdir", [], [PCData dir])] ->
    dir
  | [] ->
    raise (Ocsigen_extensions.Error_in_config_file
             ("<staticdir> option required for <graffiti>"))
  | _ ->
    raise (Ocsigen_extensions.Error_in_config_file
             ("Unexpected content inside graffiti config"))

let create_dir dir =
  try%lwt Lwt_unix.mkdir dir 0o777 with
  | Unix.Unix_error (Unix.EEXIST, "mkdir", _) -> Lwt.return_unit
  | _ ->
      Eliom_lib.debug "could not create the directory %s" dir;
      Lwt.return_unit

let image_dir name =
  let dir = static_dir ^ "/graffiti_saved/" in
  let%lwt () = create_dir dir in
  let dir = dir ^ Eliom_lib.Url.encode name in
  let%lwt () = create_dir dir in
  Lwt.return dir

let make_filename name number =
  image_dir name >|= ( fun dir -> (dir ^ "/" ^ (string_of_int number) ^ ".png") )

let save image name number =
  let%lwt file_name = make_filename name number in
  let%lwt out_chan = Lwt_io.open_file ~mode:Lwt_io.output file_name in
  Lwt_io.write out_chan image

let image_info_table = Ocsipersist.Polymorphic.open_table "image_info_table"

let save_image username =
  let now = Option.get (Ptime.of_float_s (Unix.gettimeofday ())) in
  let%lwt image_info_table = image_info_table in
  let%lwt number,_,list =
    try%lwt Ocsipersist.Polymorphic.find image_info_table username with
    | Not_found -> Lwt.return (0,now,[])
    | e -> Lwt.fail e
  in
  let%lwt () = Ocsipersist.Polymorphic.add image_info_table
      username (number+1,now,(number,now)::list) in
  let (_,image_string) = Hashtbl.find graffiti_info username in
  save (image_string ()) username number

let save_image_service =
  Eliom_service.create
    ~meth:(Eliom_service.Post
             (Eliom_parameter.unit, Eliom_parameter.string "name"))
    ~path:Eliom_service.No_path ()

let () =
  Eliom_registration.Action.register
    ~service:save_image_service (fun () name -> save_image name)

let save_image_box name =
  Lwt.return
    (Html.D.Form.post_form ~service:save_image_service
       (fun param_name ->
         [p [Html.D.Form.input ~input_type:`Hidden ~name:param_name
              ~value:name Html.D.Form.string;
             Html.D.Form.button_no_value ~button_type:`Submit [txt "save"]]])
       ())

let feed_service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["feed"])
    ~meth:(Eliom_service.Get (Eliom_parameter.string "name"))
    ()

let local_filename name number =
  ["graffiti_saved"; Eliom_lib.Url.encode name ; (string_of_int number) ^ ".png"]

let rec entries name list = function
  | 0 -> []
  | len ->
    match list with
    | [] -> []
    | (n,saved)::q ->
      let uri =
        Html.D.make_uri ~absolute:true
          ~service:(Eliom_service.static_dir ())
          (local_filename name n)
        |> Xml.string_of_uri
        |> Uri.of_string in
      let content = Syndic.Atom.Src (None, uri) in
      let authors = (Syndic.Atom.author name), [] in
      let title : Syndic.Atom.text_construct =
        Syndic.Atom.Text ("graffiti " ^ name ^ " " ^ (string_of_int n)) in
      let entry =
        Syndic.Atom.entry ~content ~id:uri ~authors ~title ~updated:saved () in
      entry::(entries name q (len - 1))

let string_page_of_feed feed =
  feed
  |> Syndic.Atom.to_xml
  |> Syndic.XML.to_string ~ns_prefix:(fun x -> Some x)
  |> fun string -> string, ""

let feed name () =
  let id =
    Xml.string_of_uri
      (Html.D.make_uri ~absolute:true ~service:feed_service name)
    |> Uri.of_string in
  let title : Syndic.Atom.text_construct =
    Syndic.Atom.Text ("nice drawings of " ^ name) in
  Lwt.catch
    (fun () ->
       let%lwt image_info_table = image_info_table in
       Ocsipersist.Polymorphic.find image_info_table name >|=
      (fun (number,updated,list) ->
         Syndic.Atom.feed ~id ~updated ~title (entries name list 10)
       |> string_page_of_feed))
    ( function Not_found ->
      let now = Option.get (Ptime.of_float_s (Unix.gettimeofday ())) in
      Lwt.return (Syndic.Atom.feed ~id ~updated:now ~title []
                  |> string_page_of_feed)
             | e -> Lwt.fail e )

let () = Eliom_registration.String.register ~service:feed_service feed
