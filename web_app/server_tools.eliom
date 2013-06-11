
{shared{

  type messages = (string * float * (float * float) * (float * float))
      deriving (Json)
}}

(* application and services *)

module My_app =
  Eliom_registration.App (struct
    let application_name = "graffiti"
  end)

let main_service =
  (Eliom_service.service
     ~path:[""]
     ~get_params:Eliom_parameter.unit
     ())

(* tools function *)

let get_smaller width height =
  if (width < height)
  then width
  else height

let round value = ceil (value -. 0.5)
