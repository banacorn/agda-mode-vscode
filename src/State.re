module Impl = (Editor: Sig.Editor) => {
  module Goal = Goal.Impl(Editor);
  module EditorIM = EditorIM.Impl(Editor);
  module QueryIM = QueryIM.Impl(Editor);
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
    mutable cursor: option(Editor.Point.t),
    editorIM: EditorIM.t,
    queryIM: QueryIM.t,
    subscriptions: array(Editor.Disposable.t),
    // for self destruction
    onRemoveFromRegistry: Event.t(unit),
  };

  //
  // getters
  //
  let getEditor = (state: t) => state.editor;

  //
  // events to the FileName-Dispatch Registry
  //
  let onRemoveFromRegistry = state => state.onRemoveFromRegistry.once();
  let emitRemoveFromRegistry = state => state.onRemoveFromRegistry.emit();

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
    state.onRemoveFromRegistry.destroy();
    state.goals->Array.forEach(Goal.destroy);
    state.decorations
    ->Array.map(fst)
    ->Array.forEach(Editor.Decoration.destroy);
    setLoaded(false);
    state.subscriptions->Array.forEach(Editor.Disposable.dispose);
    state->disconnect;
  };

  let make = (extentionPath, eventEmitter, editor) => {
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
      editorIM: EditorIM.make(eventEmitter),
      queryIM: QueryIM.make(),
      subscriptions: [||],
      onRemoveFromRegistry: Event.make(),
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
