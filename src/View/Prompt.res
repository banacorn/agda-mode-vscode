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

    // preserves mouse selection
    let (selectionInterval, setSelectionInterval) = React.useState(_ => None)

    // intercept arrow keys when the input method is activated
    // for navigating around symbol candidates
    let onKeyUp = event => {
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
        event->ReactEvent.Synthetic.persist
        let selectionInterval = (
          ReactEvent.Mouse.target(event)["selectionStart"],
          ReactEvent.Mouse.target(event)["selectionEnd"],
        )
        // preserver mouse selection so that we can restore them later
        setSelectionInterval(_ => Some(selectionInterval))
        onChange(Select(selectionInterval))
      }
    }

    // on update the text in the input box
    let onChange = event => {
      let value: string = ReactEvent.Form.target(event)["value"]
      event->ReactEvent.Synthetic.persist
      // preserver mouse selection so that we can restore them later
      setSelectionInterval(_ => Some((
        ReactEvent.Form.target(event)["selectionStart"],
        ReactEvent.Form.target(event)["selectionEnd"],
      )))
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
          onKeyUp
          onMouseUp
          onChange
          value
          ref={ReactDOMRe.Ref.callbackDomRef(ref => {
            // Update mouse selection in <input>
            ref->Js.Nullable.toOption->Option.forEach(input => {
              selectionInterval->Option.forEach(((start, end_)) => {
                let setSelectionRange = %raw(
                  `(elem, start, end_) => elem.setSelectionRange(start, end_)`
                )
                input->setSelectionRange(start, end_)->ignore
              })
            })

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
