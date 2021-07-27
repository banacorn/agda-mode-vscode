open React

module Item = {
  type t =
    | Labeled(string, string, RichText.t, option<string>, option<Common.AgdaRange.t>) // label // style // body // raw string // range
    | Unlabeled(RichText.t, option<string>, option<Common.AgdaRange.t>) // body // raw string
    | Header(string) // <h1>

  let plainText = s => Unlabeled(RichText.string(s), None, None)
  let error = (s, raw) => Labeled("Error", "error", s, raw, None)
  let warning = (s, raw) => Labeled("Warning", "warning", s, raw, None)

  @react.component
  let make = (~item: t) => {
    let (revealRaw, setRevealRaw) = React.useState(_ => false)

    let onClickRevealRaw = _ => setRevealRaw(state => !state)

    let content = (value, raw) =>
      switch raw {
      | Some(raw) => revealRaw ? <RichText value={RichText.string(raw)} /> : <RichText value />
      | None => <RichText value />
      }
    let revealRawButton = raw =>
      switch raw {
      | Some(_) =>
        revealRaw
          ? <div className="item-raw-button active" onClick=onClickRevealRaw>
              <div className="codicon codicon-code" />
            </div>
          : <div className="item-raw-button" onClick=onClickRevealRaw>
              <div className="codicon codicon-code" />
            </div>
      | None => <> </>
      }
    let locationButton = location =>
      switch location {
      | Some(location) =>
        <Component__Link
          className=["item-location-button"]
          jump=true
          hover=false
          target=Common.Link.SrcLoc(location)>
          <div className="codicon codicon-link" />
        </Component__Link>
      | None => <> </>
      }
    switch item {
    | Labeled(label, style, text, raw, _range) =>
      <li className={"labeled-item " ++ style}>
        <div className="item-label"> {string(label)} </div>
        <div className="item-content"> {content(text, raw)} </div>
        {revealRawButton(raw)}
      </li>
    | Unlabeled(text, raw, range) =>
      <li className="unlabeled-item">
        <div className="item-content"> {content(text, raw)} </div>
        {revealRawButton(raw)}
        {locationButton(range)}
      </li>
    // | HorizontalRule => <li className="horizontalRule-item"></li>
    | Header(s) => <li className="header-item"> <h3> {string(s)} </h3> </li>
    }
  }

  open! Json.Decode
  open Util.Decode

  let decode: decoder<t> = sum(x =>
    switch x {
    | "Labeled" =>
      Contents(
        tuple5(
          RichText.decode,
          optional(string),
          optional(Common.AgdaRange.decode),
          string,
          string,
        ) |> map(((text, raw, range, label, style)) => Labeled(label, style, text, raw, range)),
      )
    | "Unlabeled" =>
      Contents(
        tuple3(RichText.decode, optional(string), optional(Common.AgdaRange.decode)) |> map(((
          text,
          raw,
          range,
        )) => Unlabeled(text, raw, range)),
      )
    | "Header" => Contents(string |> map(s => Header(s)))
    | tag => raise(DecodeError("[Component.Item] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  open! Util.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Labeled(label, style, text, raw, range) =>
      object_(list{
        ("tag", string("Labeled")),
        (
          "contents",
          (text, raw, range, label, style) |> tuple5(
            RichText.encode,
            nullable(string),
            nullable(Common.AgdaRange.encode),
            string,
            string,
          ),
        ),
      })
    | Unlabeled(text, raw, range) =>
      object_(list{
        ("tag", string("Unlabeled")),
        (
          "contents",
          (text, raw, range) |> tuple3(
            RichText.encode,
            nullable(string),
            nullable(Common.AgdaRange.encode),
          ),
        ),
      })
    | Header(s) => object_(list{("tag", string("Header")), ("contents", s |> string)})
    }
}
