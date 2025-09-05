
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
      let children = Array.flat([[openParen], payload, [closeParen]])
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
  let decode: JSON.t => t
  let encode: t => JSON.t
}

module Module = {
  module ClassNames = {
    type t = array<string>

    let decode = {
      open JsonCombinators.Json.Decode
      array(string)
    }

    let encode = {
      open JsonCombinators.Json.Encode
      array(string)
    }
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

    let rec decodeRec = () => {
      open JsonCombinators.Json.Decode
      Util.Decode.sum(x =>
        switch x {
        | "Icon" => Payload(pair(string, ClassNames.decode)->map((. (s, cs)) => Icon(s, cs)))
        | "Text" => Payload(pair(string, ClassNames.decode)->map((. (s, cs)) => Text(s, cs)))
        | "Link" =>
          Payload(
            tuple3(Common.AgdaRange.decode, array(decodeRec()), ClassNames.decode)->map((. (
              r,
              xs,
              cs,
            )) => Link(r, xs, cs)),
          )
        | "Hole" => Payload(int->map((. s) => Hole(s)))
        | "Horz" => Payload(array(array(decodeRec()))->map((. xs) => Horz(xs)))
        | "Vert" => Payload(array(array(decodeRec()))->map((. xs) => Vert(xs)))
        | "Parn" => Payload(array(decodeRec())->map((. x) => Parn(x)))
        | "PrHz" => Payload(array(array(decodeRec()))->map((. xs) => PrHz(xs)))
        | tag => raise(DecodeError("[RichText.Inline] Unknown constructor: " ++ tag))
        }
      )
    }
    let decode = decodeRec()

    let rec encodeRec = () => {
      open JsonCombinators.Json.Encode
      Util.Encode.sum(x =>
        switch x {
        | Icon(s, cs) => Payload("Icon", pair(string, ClassNames.encode)((s, cs)))
        | Text(s, cs) => Payload("Text", pair(string, ClassNames.encode)((s, cs)))
        | Link(r, s, cs) =>
          Payload(
            "Link",
            tuple3(Common.AgdaRange.encode, array(encodeRec()), ClassNames.encode)((r, s, cs)),
          )
        | Hole(i) => Payload("Hole", int(i))
        | Horz(xs) => Payload("Horz", array(array(encodeRec()))(xs))
        | Vert(xs) => Payload("Vert", array(array(encodeRec()))(xs))
        | Parn(x) => Payload("Parn", array(encodeRec())(x))
        | PrHz(xs) => Payload("PrHz", array(array(encodeRec()))(xs))
        }, ...
      )
    }
    let encode = encodeRec()
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
    ->Array.flat,
  )

  // from string by Emacs
  // Regex updated to v10.1.4
  let parse = raw =>
    // after String.splitByRegExp
    //  tokens captured by the regex will be placed at odd indices
    //  and the rest will be placed at even indices
    // 3 kinds of tokens are captured as AgdaRange:
    //  1. filepath:line,column-line,column OR filepath:line.column-line.column
    //  2. filepath:line,column-column OR filepath:line.column-column
    //  3. filepath:line,column OR filepath:line.column
    raw
    ->String.splitByRegExp(%re("/([^\(\)\s]+\:(?:\d+[,\.]\d+\-\d+[,\.]\d+|\d+[,\.]\d+\-\d+|\d+[,\.]\d+))/"))
    ->Array.filterMap(x => x)
    ->Array.mapWithIndex((token, i) =>
      switch mod(i, 2) {
      | 1 => token->Common.AgdaRange.parse->Option.mapOr(string(token), loc => srcLoc(loc))
      | _ => string(token)
      }
    )
    ->concatMany


  let rec make = (~value: t) => {
    let RichText(elements) = value
    <span>
      {elements
      ->Array.mapWithIndex((x, i) => {
        switch x {
        | Text(text, className) =>
          let className = {String.concatMany(" ", className)}
          <span className key={string_of_int(i)}> {React.string(text)} </span>
        | Icon(kind, className) =>
          let className = Array.concat(["codicon", "codicon-" ++ kind], className)
          let className = {String.concatMany(" ", className)}
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
            elements->Array.mapWithIndex((element, j) =>
              <span className="component-horz-item" key={string_of_int(j)}>
                {make(~value=RichText(element))}
              </span>
            )
          <span className="component-horz" key={string_of_int(i)}> {React.array(children)} </span>
        | Vert(elements) =>
          let children =
            elements->Array.mapWithIndex((element, j) =>
              <span className="component-vert-item" key={string_of_int(j)}>
                {make(~value=RichText(element))}
              </span>
            )
          <span className="component-vert" key={string_of_int(i)}> {React.array(children)} </span>
        | Parn(element) => <Parens> {make(~value=RichText(element))} </Parens>
        | PrHz(elements) =>
          let children =
            elements->Array.mapWithIndex((element, index) =>
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



  let decode = {
    open JsonCombinators.Json.Decode
    array(Inline.decode)->map((. elems) => RichText(elems))
  }

  let encode = {
    open JsonCombinators.Json.Encode
    x =>
      switch x {
      | RichText(elemss) => array(Inline.encode)(elemss)
      }
  }
}
include Module

@react.component
let make = (~value: t) => Module.make(~value)
