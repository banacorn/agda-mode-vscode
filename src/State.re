module Impl = (Editor: Sig.Editor) => {
  open Belt;
  type editor = Editor.editor;
  type context = Editor.context;
  type t = {
    editor,
    context,
    view: Editor.view,
    mutable connection: option(Connection.t),
    onDestroyEventEmitter: Event.t(unit),
  };

  //
  // getters
  //
  let getEditor = (state: t) => state.editor;

  //
  // events
  //
  let onDestroy = (state, callback) => {
    state.onDestroyEventEmitter.on(callback)->Editor.Disposable.make;
  };

  //
  // Agda connection/disconnection
  //

  // connect if not connected yet
  let connect = state =>
    switch (state.connection) {
    | None =>
      Connection.make(Editor.Config.getAgdaPath, Editor.Config.setAgdaPath)
      ->Promise.mapError(e => Sig.Error.Connection(e))
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
      (state, request)
      : Promise.t(
          result(
            Event.t(result(Connection.response, Connection.Process.Error.t)),
            Sig.Error.t,
          ),
        ) => {
    let version = "2.6.1"; // TODO
    let filepath = "Editor.getFileName(state.editor)";
    let libraryPath = Editor.Config.getLibraryPath();
    let highlightingMethod = Editor.Config.getHighlightingMethod();
    let encoded =
      Request.encode(
        version,
        filepath,
        libraryPath,
        highlightingMethod,
        request,
      );
    Js.log2("<<<", encoded);
    // let%Ok connection = state->connect;
    // ();
    state
    ->connect
    ->Promise.mapOk(connection => Connection.send(encoded, connection));
    // Connection.send(encoded, connection);
  };

  //
  // construction/destruction
  //

  let destroy = state => {
    // state.view->Editor.View.destroy;
    state.onDestroyEventEmitter.destroy();
    state.onDestroyEventEmitter.emit();
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
      onDestroyEventEmitter: Event.make(),
    };

    // a dictionary of decorations for <Link>

    // update the state on receiving messages from the view
    // view
    // ->Editor.View.recv(onRecvMessageFromView)
    // ->Editor.addToSubscriptions(context);

    state;
  };
  //
  // View-related
  //
};