open Belt
open VSCode

// FileName-State bookkeeping
let dict: Js.Dict.t<State.t> = Js.Dict.empty()

let get = fileName => dict->Js.Dict.get(fileName)

let getByEditor = (editor: TextEditor.t) =>
  editor->TextEditor.document->TextDocument.fileName->Parser.filepath->get

// do nothing if the state already exists
let add = (fileName, state) =>
  switch get(fileName) {
  | Some(_) => ()
  | None => dict->Js.Dict.set(fileName, state)
  }

let rename = (oldName, newName) => {
  let delete_: (Js.Dict.t<'a>, string) => unit = %raw("function (dict, key) {delete dict[key]}")
  get(oldName)->Option.forEach(state => {
    Js.log3("[ registry ][ rename ]", oldName, newName)
    delete_(dict, oldName)
    add(newName, state)
  })
}

// remove the entry (without triggering .destroy() )
let remove = fileName => {
  let delete_: (Js.Dict.t<'a>, string) => unit = %raw("function (dict, key) {delete dict[key]}")
  delete_(dict, fileName)
}

let destroy = fileName =>
  switch get(fileName) {
  | None => Promise.resolved()
  | Some(state) =>
    remove(fileName)
    State.destroy(state)
  }

// let forceDestroy = fileName =>
//   switch get(fileName) {
//   | None => Promise.resolved()
//   | Some(state) =>
//     remove(fileName)
//     State.destroy(state)
//   }

let contains = fileName => get(fileName)->Option.isSome
