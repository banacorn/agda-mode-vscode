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
  module Attributes = {
    type t = {
      link: option<Common.Link.t>,
      icon: option<string>,
    }

    let empty = {
      link: None,
      icon: None,
    }

    open! Json.Decode
    let decode: decoder<t> = json => {
      link: json |> field("attrLink", optional(Common.Link.decode)),
      icon: json |> field("attrIcon", optional(string)),
    }

    open! Json.Encode
    let encode: encoder<t> = x =>
      object_(list{
        ("attrLink", x.link |> nullable(Common.Link.encode)),
        ("attrIcon", x.icon |> nullable(string)),
      })
  }

  module Element = {
    type t = Elem(string, Attributes.t)

    open! Json.Decode
    let decode: decoder<t> = pair(string, Attributes.decode) |> map(((s, attrs)) => Elem(s, attrs))

    open! Json.Encode
    let encode: encoder<t> = x =>
      switch x {
      | Elem(s, attrs) => (s, attrs) |> pair(string, Attributes.encode)
      }
  }

  type t = RichText(array<Element.t>)

  let text = s => RichText([Elem(s, Attributes.empty)])
  let hole = (s, i) => RichText([
    Elem(
      s,
      {
        link: Some(Common.Link.ToHole(i)),
        icon: None,
      },
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

  //   let toText = x =>
  //     switch x {
  //     | RichText(elems) => elems->Array.map(Element.toText)->Component.Text.concatMany
  //     }

  let make = (~value: t) => {
    let RichText(elements) = value
    <span>
      {elements
      ->Array.mapWithIndex((i, x) => {
        switch x {
        | Elem(text, attributes) =>
          switch attributes.link {
          | Some(target) =>
            <Component__Link key={string_of_int(i)} jump=true hover=false target>
              {React.string(text)}
            </Component__Link>
          | None =>
            switch attributes.icon {
            | Some(kind) => <div key={string_of_int(i)} className={"codicon codicon-" ++ kind} />
            | None => <span key={string_of_int(i)}> {React.string(text)} </span>
            }
          }
        }
      })
      ->React.array}
    </span>
  }

  open! Json.Decode
  let decode: decoder<t> = array(Element.decode) |> map(elems => RichText(elems))

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | RichText(elemss) => elemss |> array(Element.encode)
    }
}
include Module

@react.component
let make = (~value: t) => Module.make(~value)
