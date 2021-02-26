open Belt
module Module = {
  module Attributes = {
    type t = {
      link: option<Common.Link.t>,
      icon: option<string>,
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

    // NOTE: revamp this after removing Text
    let toText = x => {
      open Component.Text
      switch x {
      | Elem(text, attrs) =>
        switch attrs.link {
        | Some(link) => Text([Segment.Link(text, None, true, false, link)])
        | None =>
          switch attrs.icon {
          | Some(kind) => Text([Segment.Icon(kind)])
          | None => plainText(text)
          }
        }
      }
    }

    open! Json.Decode
    let decode: decoder<t> = pair(string, Attributes.decode) |> map(((s, attrs)) => Elem(s, attrs))

    open! Json.Encode
    let encode: encoder<t> = x =>
      switch x {
      | Elem(s, attrs) => (s, attrs) |> pair(string, Attributes.encode)
      }
  }

  type t = RichText(array<Element.t>)

  let toText = x =>
    switch x {
    | RichText(elems) => elems->Array.map(Element.toText)->Component.Text.concatMany
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
