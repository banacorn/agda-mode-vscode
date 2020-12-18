module Offset = {
  type t = int

  let toPosition = (document, offset) => document->VSCode.TextDocument.positionAt(offset)
  let fromPosition = (document, position) => document->VSCode.TextDocument.offsetAt(position)

  let decode = Json.Decode.int
  let encode = Json.Encode.int
}

module Interval = {
  type t = (int, int)

  let toRange = (document, interval) =>
    VSCode.Range.make(
      Offset.toPosition(document, fst(interval)),
      Offset.toPosition(document, snd(interval)),
    )
  let fromRange = (document, range) => (
    Offset.fromPosition(document, VSCode.Range.start(range)),
    Offset.fromPosition(document, VSCode.Range.end_(range)),
  )

  let contains = (interval, offset) => {
    let (start, end_) = interval
    start <= offset && offset <= end_
  }

  let decode = Json.Decode.pair(Json.Decode.int, Json.Decode.int)
  let encode = Json.Encode.pair(Json.Encode.int, Json.Encode.int)
}
