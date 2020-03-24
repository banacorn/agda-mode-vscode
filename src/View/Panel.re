open Belt;

Webapi.Dom.Document.getElementById("root", Webapi.Dom.document)
->Option.forEach(element => {
    ReactDOMRe.render(
      <section>
        <div className="agda-mode-header">
          {ReasonReact.string("head!!")}
        </div>
        <div className="agda-mode-body">
          {ReasonReact.string("body :D")}
        </div>
      </section>,
      element,
    )
  });