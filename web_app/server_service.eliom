
module My_app =
  Eliom_registration.App (struct
    let application_name = "graffiti"
  end)

let main_service =
  Eliom_service.service
    ~path:[""]
    ~get_params:Eliom_parameter.unit
    ()
