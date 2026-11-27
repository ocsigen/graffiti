(** This is the main file of your own executable, which links Ocsigen Server as
    a library and needs no configuration file. README.md describes the other way
    to run the application, with the ocsigenserver command and
    graffiti.conf. *)

let logdir = "local/var/log/graffiti"
let dbfile = "local/var/data/graffiti/ocsidb"
let rundir = "local/var/run"
let staticdir = "local/var/www/graffiti"

(* The absolute links take the host name of the request. In production, behind a
   reverse proxy or when the application sends links by email, add
   [~usedefaulthostname:true] and give the public host name, port and protocol
   to [Ocsigen.Server.host] ([~default_hostname], [~default_httpport],
   [~default_protocol_is_https]): the links then use them, rather than the Host
   header of the request, which a client can forge. *)
let () =
  Ocsipersist_settings.set_db_file dbfile;
  Ocsigen.Server.start
    ~ports:[`All, 8080]
    ~veryverbose:() ~debugmode:true ~logdir ~uploaddir:(Some "/tmp")
    ~command_pipe:(Filename.concat rundir "graffiti-cmd")
    ~default_charset:(Some "utf-8")
    [ Ocsigen.Server.host
        [ Staticmod.run ~dir:staticdir ()
        ; (* As <wasm> in the configuration file: the page loads the WASM
             version of the client program in the browsers that support it, and
             the JavaScript version in the others. *)
          Eliom.App.run ~enable_wasm:true () ] ]
