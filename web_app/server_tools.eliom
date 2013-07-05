
let make_element name v =
  Ocsigen_extensions.Configuration.element
    ~name ?pcdata:(Some (fun s -> v := s)) ()

let logdir_ref = ref ""
let logdir_elt = make_element "logdir" logdir_ref

let datadir_ref = ref ""
let datadir_elt = make_element "datadir" datadir_ref

let config = Eliom_config.parse_config [logdir_elt; datadir_elt]

let logdir = !logdir_ref ^ "/"
let datadir = !datadir_ref ^ "/"
