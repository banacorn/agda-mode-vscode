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
      Js.log("MAKE CONNECTION");
      Connection.make(Editor.Config.getAgdaPath, Editor.Config.setAgdaPath)
      ->Promise.mapError(e => Sig.Error.Connection(e))
      ->Promise.tapOk(conn => state.connection = Some(conn));
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
      (state, request): Promise.t(result(Connection.t, Sig.Error.t)) => {
    let version = "2.6.1"; // TODO
    let filepath =
      Editor.getFileName(state.editor)->Option.getWithDefault(""); //"Editor.getFileName(state.editor)";
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
    ->Promise.mapOk(connection => {
        Connection.send(encoded, connection);
        connection;
      });
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

    state;
  };
  //
  // View-related
  //
};