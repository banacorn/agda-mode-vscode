open React

module Item = {
  type t =
    | Labeled(string, string, RichText.t, option<string>) // label // style // body // raw string
    | Unlabeled(RichText.t, option<string>) // body // raw string
    // | HorizontalRule // <hr>
    | Header(string) // <h1>

  let plainText = s => Unlabeled(RichText.string(s), None)
  let error = (s, raw) => Labeled("Error", "error", s, raw)
  let warning = (s, raw) => Labeled("Warning", "warning", s, raw)

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
    switch item {
    | Labeled(label, style, text, raw) =>
      <li className={"labeled-item " ++ style}>
        <div className="item-label"> {string(label)} </div>
        <div className="item-content"> {content(text, raw)} </div>
        {revealRawButton(raw)}
      </li>
    | Unlabeled(text, raw) =>
      <li className="unlabeled-item">
        <div className="item-content"> {content(text, raw)} </div> {revealRawButton(raw)}
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
        tuple4(RichText.decode, optional(string), string, string) |> map(((
          text,
          raw,
          label,
          style,
        )) => Labeled(label, style, text, raw)),
      )
    | "Unlabeled" =>
      Contents(
        pair(RichText.decode, optional(string)) |> map(((text, raw)) => Unlabeled(text, raw)),
      )
    // | "HorizontalRule" => TagOnly(HorizontalRule)
    | "Header" => Contents(string |> map(s => Header(s)))
    | tag => raise(DecodeError("[Component.Item] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Labeled(label, style, text, raw) =>
      object_(list{
        ("tag", string("Labeled")),
        (
          "contents",
          (text, raw, label, style) |> tuple4(RichText.encode, nullable(string), string, string),
        ),
      })
    | Unlabeled(text, raw) =>
      object_(list{
        ("tag", string("Unlabeled")),
        ("contents", (text, raw) |> pair(RichText.encode, nullable(string))),
      })
    // | HorizontalRule => object_(list{("tag", string("HorizontalRule"))})
    | Header(s) => object_(list{("tag", string("Header")), ("contents", s |> string)})
    }
}
