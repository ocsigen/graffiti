
{client{

  val on_phonegap : unit -> bool

  val launch_on_phonegap : (unit -> unit) -> unit

  val not_launch_on_phonegap : (unit -> unit) -> unit

  val download_file : Js.js_string Js.t -> unit

  (*** Events ***)

  val deviceready: Dom_html.event Js.t Dom_html.Event.typ

  val ondeviceready : unit -> Dom_html.event Js.t Lwt.t

  (** ifpg series mind if on phonegap do it action else directly return *)
  val ondeviceready_ifpg : unit -> Dom_html.event Js.t option Lwt.t

  (** if not on phonegap, only what onload *)
  val onload_ondeviceready_ifpg : unit -> Dom_html.event Js.t option Lwt.t

}}
