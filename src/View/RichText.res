open Belt

module type Module = {
  type t
  // constructors
  let text: string => t
  let concatMany: array<t> => t

  // React component
  let make: (~value: t) => React.element

  // JSON encoding/decoding
  let decode: Js.Json.t => t
  let encode: t => Js.Json.t
}

module Module = {
  module ClassNames = {
    type t = array<string>

    open! Json.Decode
    let decode: decoder<t> = array(string)

    open! Json.Encode
    let encode: encoder<t> = array(string)
  }

  module Inline = {
    type rec t =
      | Icon(string, ClassNames.t)
      | Text(string, ClassNames.t)
      | Link(Common.AgdaRange.t, array<t>, ClassNames.t)
      | Hole(int)

    open! Json.Decode
    open Util.Decode
    let rec decode: unit => decoder<t> = () =>
      sum(x =>
        switch x {
        | "Icon" => Contents(pair(string, ClassNames.decode) |> map(((s, cs)) => Icon(s, cs)))
        | "Text" => Contents(pair(string, ClassNames.decode) |> map(((s, cs)) => Text(s, cs)))
        | "Link" =>
          Contents(
            tuple3(Common.AgdaRange.decode, array(decode()), ClassNames.decode) |> map(((
              r,
              s,
              cs,
            )) => Link(r, s, cs)),
          )

        | "Hole" => Contents(int |> map(i => Hole(i)))
        | tag => raise(DecodeError("[RichText.Inline] Unknown constructor: " ++ tag))
        }
      )
    let decode = decode()

    open! Json.Encode
    let rec encode: encoder<t> = x =>
      switch x {
      | Icon(s, cs) =>
        object_(list{
          ("tag", string("Icon")),
          ("contents", (s, cs) |> pair(string, ClassNames.encode)),
        })

      | Text(s, cs) =>
        object_(list{
          ("tag", string("Text")),
          ("contents", (s, cs) |> pair(string, ClassNames.encode)),
        })

      | Link(r, s, cs) =>
        object_(list{
          ("tag", string("Link")),
          (
            "contents",
            (r, s, cs) |> tuple3(Common.AgdaRange.encode, array(encode), ClassNames.encode),
          ),
        })

      | Hole(i) => object_(list{("tag", string("Hole")), ("contents", i |> int)})
      }
  }
  type t = RichText(array<Inline.t>)

  let empty = RichText([])
  let string = s => RichText([Text(s, [])])
  let hole = i => RichText([Hole(i)])
  let srcLoc = range => RichText([
    Link(
      range,
      [Icon("link ", []), Text(Common.AgdaRange.toString(range), [])],
      ["component-link component-hole"],
    ),
  ])

  let concatMany = xs => RichText(
    xs
    ->Array.map(x =>
      switch x {
      | RichText(xs) => xs
      }
    )
    ->Array.concatMany,
  )

  // from string by Emacs
  let parse = raw =>
    raw
    ->Js.String.splitByRe(
      %re("/([^\\(\\)\\s]+\\:(?:\\d+\\,\\d+\\-\\d+\\,\\d+|\\d+\\,\\d+\\-\\d+))/"),
      _,
    )
    ->Array.keepMap(x => x)
    ->Array.mapWithIndex((i, token) =>
      switch mod(i, 2) {
      | 1 => token->Common.AgdaRange.parse->Option.mapWithDefault(string(token), loc => srcLoc(loc))
      | _ => string(token)
      }
    )
    ->concatMany

  let rec make = (~value: t) => {
    let RichText(elements) = value
    <span>
      {elements
      ->Array.mapWithIndex((i, x) => {
        switch x {
        | Text(text, className) =>
          let className = {String.concat(" ", List.fromArray(className))}
          <span className key={string_of_int(i)}> {React.string(text)} </span>
        | Icon(kind, className) =>
          let className = Array.concat(["codicon", "codicon-" ++ kind], className)
          let className = {String.concat(" ", List.fromArray(className))}
          <div className key={string_of_int(i)} />
        | Link(range, children, className) =>
          let className = Array.concat(["component-link"], className)
          let child = make(~value=RichText(children))
          <Component__Link
            key={string_of_int(i)} className jump=true hover=false target=Common.Link.SrcLoc(range)>
            {child}
          </Component__Link>
        | Hole(index) =>
          let className = ["component-link", "component-hole"]
          <Component__Link
            key={string_of_int(i)} className jump=true hover=false target=Common.Link.Hole(index)>
            {React.string("?" ++ string_of_int(index))}
          </Component__Link>
        }
      })
      ->React.array}
    </span>
  }

  open! Json.Decode
  let decode: decoder<t> = array(Inline.decode) |> map(elems => RichText(elems))

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | RichText(elemss) => elemss |> array(Inline.encode)
    }
}
include Module

@react.component
let make = (~value: t) => Module.make(~value)
