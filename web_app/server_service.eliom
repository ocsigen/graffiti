
module My_app =
  Eliom_registration.App (struct
    let application_name = "graffiti"
  end)

let main_service =
  Eliom_service.service
    ~path:[""]
    ~get_params:Eliom_parameter.unit
    ()

let setting_replay_service =
  Eliom_service.service
    ~path:["replay"]
    ~get_params:Eliom_parameter.unit
    ()

let start_replay_service =
  Eliom_service.post_coservice
    ~name:"start_replay"
    ~fallback:setting_replay_service
    ~post_params:Eliom_parameter.(string "start_d" ** string "start_t" **
				  string "end_d" ** string "end_t")
    ()
