// NOTE:
//
//  VSCode imports should not be allowed in this module, otherwise it would contaminate the view
//

module Offset = {
  type t = int

  let decode = Json.Decode.int
  let encode = Json.Encode.int
}

module Interval = {
  type t = (int, int)

  let contains = (interval, offset) => {
    let (start, end_) = interval
    start <= offset && offset <= end_
  }

  let decode = Json.Decode.pair(Json.Decode.int, Json.Decode.int)
  let encode = Json.Encode.pair(Json.Encode.int, Json.Encode.int)
}
