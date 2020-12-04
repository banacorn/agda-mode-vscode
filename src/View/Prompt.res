open Belt

@react.component
let make = (
  ~inputMethodActivated: bool,
  ~prompt: option<(option<string>, option<string>, option<string>)>,
  ~onSubmit: option<string> => unit,
  ~onChange: View.EventFromView.Prompt.t => unit,
) =>
  switch prompt {
  | Some((body, placeholder, value)) =>
    let placeholder = placeholder->Option.getWithDefault("")
    let value = value->Option.getWithDefault("")

    // intercept arrow keys when the input method is activated
    // for navigating around symbol candidates
    let onKeyDown = event => {
      let arrowKey = switch event->ReactEvent.Keyboard.keyCode {
      | 38 => Some(View.EventFromView.Prompt.BrowseUp)
      | 40 => Some(BrowseDown)
      | 37 => Some(BrowseLeft)
      | 39 => Some(BrowseRight)
      | _ => None
      }
      if inputMethodActivated {
        arrowKey->Option.forEach(action => {
          onChange(action)
          event->ReactEvent.Keyboard.preventDefault
        })
      }
    }

    let onMouseUp = event => {
      if inputMethodActivated {
        let offset: int = (event->ReactEvent.Mouse.target)["selectionStart"]
        onChange(Select([offset]))
      }
    }

    // on update the text in the input box
    let onChange = event => {
      let value: string = (event->ReactEvent.Form.target)["value"]
      onChange(Change(value))
    }

    let onSubmit = _event => onSubmit(Some(value))

    <div className="agda-mode-prompt">
      <form onSubmit>
        {switch body {
        | None => <> </>
        | Some(message) => <p> {React.string(message)} </p>
        }}
        <input
          type_="text"
          placeholder
          onKeyDown
          onMouseUp
          onChange
          value
          ref={ReactDOMRe.Ref.callbackDomRef(ref => {
            // HACK
            // somehow focus() won't work on some machines (?)
            // delay focus() 100ms to regain focus
            Js.Global.setTimeout(() => {
              ref
              ->Js.Nullable.toOption
              ->Option.flatMap(Webapi.Dom.Element.asHtmlElement)
              ->Option.forEach(Webapi.Dom.HtmlElement.focus)
              ()
            }, 100)->ignore
            ()
          })}
        />
      </form>
    </div>
  | None => <> </>
  }
