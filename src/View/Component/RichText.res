open Belt

module Parens = {
  @react.component
  let make = (~children) => {
    let (activated, setActivated) = React.useState(_ => false)
    let (contracted, setContracted) = React.useState(_ => false)
    let className = activated ? "component-parentheses activated" : "component-parentheses"
    let onMouseOver = _ => setActivated(_ => true)
    let onMouseOut = _ => setActivated(_ => false)
    let onClick = _ => setContracted(x => !x)
    <>
      <span className onMouseOver onMouseOut onClick> {React.string("(")} </span>
      {contracted ? {React.string("..")} : children}
      <span className onMouseOver onMouseOut onClick> {React.string(")")} </span>
    </>
  }
}

// TODO: for PrHz, refactor this
module Parens2 = {
  @react.component
  let make = (~payload) => {
    // states
    let (activated, setActivated) = React.useState(_ => false)
    let (contracted, setContracted) = React.useState(_ => false)
    // event handlers
    let onMouseOver = _ => setActivated(_ => true)
    let onMouseOut = _ => setActivated(_ => false)
    let onClick = _ => setContracted(x => !x)
    // the opening parenthesis
    let openParenClassName =
      "component-horz-item component-parentheses" ++ (activated ? " activated" : "")
    let openParen =
      <span className=openParenClassName onMouseOver onMouseOut onClick> {React.string("(")} </span>
    // the closing parenthesis
    let closeParenClassName =
      "component-horz-item component-parentheses compact" ++ (activated ? " activated" : "")
    let closeParen =
      <span className=closeParenClassName onMouseOver onMouseOut onClick>
        {React.string(")")}
      </span>

    // display only "(..)" when its contracted
    if contracted {
      <span className="component-horz">
        <span className=openParenClassName onMouseOver onMouseOut onClick>
          {React.string("(..)")}
        </span>
      </span>
    } else {
      let children = Array.concatMany([[openParen], payload, [closeParen]])
      <span className="component-horz"> {React.array(children)} </span>
    }
  }
}

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
      | Horz(array<array<t>>)
      | Vert(array<array<t>>)
      | Parn(array<t>)
      // refactor PrHz
      | PrHz(array<array<t>>)

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
              xs,
              cs,
            )) => Link(r, xs, cs)),
          )
        | "Hole" => Contents(int |> map(s => Hole(s)))
        | "Horz" => Contents(array(array(decode())) |> map(xs => Horz(xs)))
        | "Vert" => Contents(array(array(decode())) |> map(xs => Vert(xs)))
        | "Parn" => Contents(array(decode()) |> map(x => Parn(x)))
        | "PrHz" => Contents(array(array(decode())) |> map(xs => PrHz(xs)))
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
      | Horz(xs) => object_(list{("tag", string("Horz")), ("contents", xs |> array(array(encode)))})
      | Vert(xs) => object_(list{("tag", string("Vert")), ("contents", xs |> array(array(encode)))})
      | Parn(x) => object_(list{("tag", string("Parn")), ("contents", x |> array(encode))})
      | PrHz(xs) => object_(list{("tag", string("PrHz")), ("contents", xs |> array(array(encode)))})
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
  // Regex updated to v10.1.4
  let parse = raw =>
    // ranges at odd indices
    // other tokens at even indices
    raw
    ->Js.String.splitByRe(%re("/([^\(\)\s]+\:(?:\d+\,\d+\-\d+\,\d+|\d+\,\d+\-\d+))/"), _)
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
          let child = make(~value=RichText(children))
          <Link key={string_of_int(i)} className jump=true hover=false target=Link.SrcLoc(range)>
            {child}
          </Link>
        | Hole(index) =>
          let className = ["component-link", "component-hole"]
          <Link key={string_of_int(i)} className jump=true hover=false target=Link.Hole(index)>
            {React.string("?" ++ string_of_int(index))}
          </Link>
        | Horz(elements) =>
          let children =
            elements->Array.mapWithIndex((j, element) =>
              <span className="component-horz-item" key={string_of_int(j)}>
                {make(~value=RichText(element))}
              </span>
            )
          <span className="component-horz" key={string_of_int(i)}> {React.array(children)} </span>
        | Vert(elements) =>
          let children =
            elements->Array.mapWithIndex((j, element) =>
              <span className="component-vert-item" key={string_of_int(j)}>
                {make(~value=RichText(element))}
              </span>
            )
          <span className="component-vert" key={string_of_int(i)}> {React.array(children)} </span>
        | Parn(element) => <Parens> {make(~value=RichText(element))} </Parens>
        | PrHz(elements) =>
          let children =
            elements->Array.mapWithIndex((index, element) =>
              index == 0
                ? <span className="component-horz-item compact">
                    {make(~value=RichText(element))}
                  </span>
                : <span className="component-horz-item"> {make(~value=RichText(element))} </span>
            )
          <Parens2 payload=children />
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
