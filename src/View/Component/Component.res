open React

module Item = {
  type t =
    | Labeled(string, string, RichText.t, option<string>) // label // style // body // raw string
    | Unlabeled(RichText.t, option<string>) // body // raw string

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
    }
  }

  open! Json.Decode
  open Util.Decode

  let decode: decoder<t> = sum(x =>
    switch x {
    | "Labeled" =>
      Contents(
        tuple4(string, string, RichText.decode, optional(string)) |> map(((
          label,
          style,
          text,
          raw,
        )) => Labeled(label, style, text, raw)),
      )
    | "Unlabeled" =>
      Contents(
        pair(RichText.decode, optional(string)) |> map(((text, raw)) => Unlabeled(text, raw)),
      )
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
          (label, style, text, raw) |> tuple4(string, string, RichText.encode, nullable(string)),
        ),
      })
    | Unlabeled(text, raw) =>
      object_(list{
        ("tag", string("Unlabeled")),
        ("contents", (text, raw) |> pair(RichText.encode, nullable(string))),
      })
    }
}
