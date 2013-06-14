
{shared{

  type messages = (string * float * (float * float) * (float * float))
      deriving (Json)

  let get_smaller x y =
    if (x < y)
    then x
    else y

  let round value = ceil (value -. 0.5)

  (*** images tools ***)

  let max_resolution = 16
  let min_resolution = 8

  let get_min_resolution max =
    int_of_float (round
                    (( float_of_int (max * min_resolution) ) /.
			(float_of_int max_resolution) ))

  let get_max_resolution min =
    int_of_float (round
                    (( float_of_int (min * max_resolution) ) /.
                      (float_of_int min_resolution) ))


}}
