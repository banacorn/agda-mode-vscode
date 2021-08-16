open Belt

open State__Type
module View = State__View
module Context = State__Type.Context
type t = t 

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  State
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let sendRequest = (
  state: state,
  handleResponse: Response.t => Promise.t<unit>,
  request: Request.t,
): Promise.t<unit> => {
  let sendRequestAndHandleResponses = (state, request, handler) => {
    let onResponse = result =>
      switch result {
      | Error(error) => View.displayConnectionError(state, error)
      | Ok(response) => handler(response)
      }
    Connection.sendRequest(
      state.globalStoragePath,
      Config.Connection.useAgdaLanguageServer(),
      state.document,
      request,
      onResponse,
    )->Promise.flatMap(result =>
      switch result {
      | Error(error) => View.displayConnectionError(state, error)
      | Ok(status) => View.displayConnectionStatus(state, status)
      }
    )
  }

  state.agdaRequestQueue->RequestQueue.push(
    request => sendRequestAndHandleResponses(state, request, handleResponse),
    request,
  )
}

// construction/destruction
let destroy = (state, alsoRemoveFromRegistry) => {
  if alsoRemoveFromRegistry {
    state.onRemoveFromRegistry->Chan.emit()
  }
  state.onRemoveFromRegistry->Chan.destroy
  state.goals->Array.forEach(Goal.destroy)
  state.decoration->Decoration.destroy
  state.subscriptions->Array.forEach(VSCode.Disposable.dispose)
  Connection.stop()
  // TODO: delete files in `.indirectHighlightingFileNames`
}

let make = (chan, globalStoragePath, editor, panel) => {
  editor: editor,
  document: VSCode.TextEditor.document(editor),
  panel: panel,
  panelCache: ViewCache.make(),
  debugBuffer: None,
  debugBufferCache: ViewCache.make(),
  goals: [],
  decoration: Decoration.make(),
  cursor: None,
  editorIM: IM.make(chan),
  promptIM: IM.make(chan),
  subscriptions: [],
  onRemoveFromRegistry: Chan.make(),
  agdaRequestQueue: RequestQueue.make(),
  globalStoragePath: globalStoragePath,
}
