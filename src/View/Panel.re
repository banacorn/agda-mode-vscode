open Belt;

Webapi.Dom.Document.getElementById("root", Webapi.Dom.document)
->Option.forEach(element => {
    ReactDOMRe.render(
      <section> <div> {ReasonReact.string("hey!!")} </div> </section>,
      element,
    )
  });