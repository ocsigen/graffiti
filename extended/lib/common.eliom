[%%shared.start]

type messages =
  ((int * int * int) * int * (int * int) * (int * int))
  [@@deriving json]

let width = 700
let height = 400
