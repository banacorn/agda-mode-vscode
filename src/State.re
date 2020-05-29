module Impl = (Editor: Sig.Editor) => {
  module Goal = Goal.Impl(Editor);
  module Request = Request.Impl(Editor);
  open Belt;
  type editor = Editor.editor;
  type context = Editor.context;
  type t = {
    editor,
    context,
    view: Editor.view,
    mutable connection: option(Connection.t),
    mutable goals: array(Goal.t),
    onDestroyEventEmitter: Event.t(unit),
    onViewInquiryResponse: Event.t(option(string)),
  };

  //
  // getters
  //
  let getEditor = (state: t) => state.editor;

  //
  // events
  //
  let onceDestroyed = state => state.onDestroyEventEmitter.once();

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

  let sendRequest =
      (state, request): Promise.t(result(Connection.t, Error.t)) => {
    state
    ->connect
    ->Promise.mapOk(connection => {
        let version = connection.metadata.version;
        let filepath =
          Editor.getFileName(state.editor)->Option.getWithDefault("");
        let libraryPath = Editor.Config.getLibraryPath();
        let highlightingMethod = Editor.Config.getHighlightingMethod();
        let encoded =
          Request.encode(
            state.editor,
            version,
            filepath,
            libraryPath,
            highlightingMethod,
            request,
          );
        Js.log2("<<<", encoded);
        Connection.send(encoded, connection);
        connection;
      });
  };

  //
  // construction/destruction
  //

  let destroy = state => {
    state.view->Editor.View.destroy;
    state.onDestroyEventEmitter.emit();
    state.onDestroyEventEmitter.destroy();
    state.goals->Array.forEach(Goal.destroy);
    state->disconnect;
  };

  let make = (context, editor) => {
    // view initialization
    let view = Editor.View.make(context, editor);

    let state = {
      editor,
      context,
      view,
      connection: None,
      goals: [||],
      onDestroyEventEmitter: Event.make(),
      onViewInquiryResponse: Event.make(),
    };

    state;
  };
  //
  // View-related
  //

  let show = state => state.view->Editor.View.show;
  let hide = state => state.view->Editor.View.hide;
  let sendRequestToView = (state, request) =>
    Editor.View.send(state.view, request)->Promise.map(_ => ());
};