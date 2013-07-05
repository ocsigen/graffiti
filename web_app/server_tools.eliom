
let get_element name =
  let v = ref "" in
  let elm = Ocsigen_extensions.Configuration.element
    ~name ?pcdata:(Some (fun s -> v := s)) ()
  in
  Eliom_config.parse_config [elm];
  !v

let logdir = get_element "logdir"
