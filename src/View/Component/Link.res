open Common

type t =
  | SrcLoc(AgdaRange.t)
  | Hole(int)

let toString = x =>
  switch x {
  | SrcLoc(range) => AgdaRange.toString(range)
  | Hole(int) => "?" ++ string_of_int(int)
  }

let decode: Json.Decode.decoder<t> = Util.Decode.sum(x => {
  open Json.Decode
  switch x {
  | "LinkRange" => Contents(Util.Decode.convert(AgdaRange.decode) |> map(range => SrcLoc(range)))
  | "LinkHole" => Contents(int |> map(index => Hole(index)))
  | tag => raise(DecodeError("[View.Link] Unknown constructor: " ++ tag))
  }
})

let encode: Json.Encode.encoder<t> = x => {
  open Json.Encode
  switch x {
  | SrcLoc(range) =>
    object_(list{("tag", string("LinkRange")), ("contents", range |> AgdaRange.encode)})
  | Hole(index) => object_(list{("tag", string("LinkHole")), ("contents", index |> int)})
  }
}
module Event = {
  type t =
    | JumpToTarget(t)
    | MouseOver(t)
    | MouseOut(t)

  let chan: Chan.t<t> = Chan.make()
  let eventContext = React.createContext(chan)

  module Provider = {
    let makeProps = (~value, ~children, ()) =>
      {
        "value": value,
        "children": children,
      }

    let make = React.Context.provider(eventContext)
  }

  let decode: Json.Decode.decoder<t> = {
    open Json.Decode
    Util.Decode.sum(x =>
      switch x {
      | "JumpToTarget" => Contents(decode |> map(link => JumpToTarget(link)))
      | "MouseOver" => Contents(decode |> map(link => MouseOver(link)))
      | "MouseOut" => Contents(decode |> map(link => MouseOut(link)))
      | tag => raise(DecodeError("[Response.EventFromView] Unknown constructor: " ++ tag))
      }
    )
  }

  let encode: Json.Encode.encoder<t> = x => {
    open Json.Encode
    switch x {
    | JumpToTarget(link) =>
      object_(list{("tag", string("JumpToTarget")), ("contents", link |> encode)})
    | MouseOver(link) => object_(list{("tag", string("MouseOver")), ("contents", link |> encode)})
    | MouseOut(link) => object_(list{("tag", string("MouseOut")), ("contents", link |> encode)})
    }
  }
}

@react.component
let make = (~target=SrcLoc(NoRange), ~jump=false, ~hover=false, ~className=[], ~children) => {
  let sanitizedTarget = switch target {
  | SrcLoc(NoRange)
  | SrcLoc(Range(_, [])) =>
    None
  | Hole(index) => Some(Hole(index))
  | SrcLoc(range) => Some(SrcLoc(range))
  }

  let link = React.useContext(Event.eventContext)
  let className = Belt.List.fromArray(className)

  switch sanitizedTarget {
  | None => <span className={String.concat(" ", className)}> children </span>
  | Some(t) =>
    <span
      className={String.concat(" ", list{"component-link", ...className})}
      onClick={_ => {
        if jump {
          link->Chan.emit(Event.JumpToTarget(t))
        }
      }}
      onMouseOver={_ =>
        if hover {
          link->Chan.emit(Event.MouseOver(t))
        }}
      onMouseOut={_ =>
        if hover {
          link->Chan.emit(Event.MouseOut(t))
        }}>
      children
    </span>
  }
}
