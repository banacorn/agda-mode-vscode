module Impl = (Editor: Sig.Editor) => {
  module Goal = Goal.Impl(Editor);
  module InputMethod = InputMethod.Impl(Editor);
  module Request = Request.Impl(Editor);
  open Belt;
  type editor = Editor.editor;
  type context = Editor.context;
  type t = {
    mutable editor,
    view: Editor.view,
    mutable connection: option(Connection.t),
    mutable goals: array(Goal.t),
    mutable decorations: array((Editor.Decoration.t, Editor.Range.t)),
    mutable cursor: option(int),
    onKillMePlzEmitter: Event.t(unit),
    inputMethod: InputMethod.t,
    subscriptions: array(Editor.Disposable.t),
  };

  //
  // getters
  //
  let getEditor = (state: t) => state.editor;

  //
  // events to the outside world
  //
  let onKillMePlz = state => state.onKillMePlzEmitter.once();
  let emitKillMePlz = state => state.onKillMePlzEmitter.emit();

  //
  // Agda connection/disconnection
  //

  // connect if not connected yet
  let connect = state =>
    switch (state.connection) {
    | None =>
      Connection.make(Editor.Config.getAgdaPath, Editor.Config.setAgdaPath)
      ->Promise.mapError(e => Error.Connection(e))
      ->Promise.tapOk(conn => state.connection = Some(conn))
    | Some(connection) => Promise.resolved(Ok(connection))
    };
  let disconnect = state =>
    switch (state.connection) {
    | None => Promise.resolved()
    | Some(connection) =>
      Connection.destroy(connection);
      Promise.resolved();
    };

  //
  // construction/destruction
  //

  // set context so that only certain key bindings only work
  // when there's a active text editor
  let setLoaded = value => Editor.setContext("agdaMode", value)->ignore;

  let destroy = state => {
    state.view->Editor.View.destroy;
    state.onKillMePlzEmitter.destroy();
    state.goals->Array.forEach(Goal.destroy);
    state.decorations
    ->Array.map(fst)
    ->Array.forEach(Editor.Decoration.destroy);
    setLoaded(false);
    state.subscriptions->Array.forEach(Editor.Disposable.dispose);
    state->disconnect;
  };

  let make = (extentionPath, editor) => {
    setLoaded(true);
    // view initialization
    let view = Editor.View.make(extentionPath, editor);

    let state = {
      editor,
      view,
      connection: None,
      goals: [||],
      decorations: [||],
      cursor: None,
      onKillMePlzEmitter: Event.make(),
      inputMethod: InputMethod.make(),
      subscriptions: [||],
    };

    state;
  };

  //
  // View-related
  //

  let show = state => {
    state.view->Editor.View.show;
    setLoaded(true);
  };
  let hide = state => {
    state.view->Editor.View.hide;
    setLoaded(false);
  };
  let sendRequestToView = (state, request) =>
    Editor.View.send(state.view, View.RequestOrEventToView.Request(request));
  let sendEventToView = (state, event) =>
    Editor.View.send(state.view, View.RequestOrEventToView.Event(event));
};
