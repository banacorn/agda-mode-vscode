module Module: {
  let get: string => option<State.t>
  let add: (string, State.t) => unit
  let remove: string => unit
  let removeAndDestroy: string => Promise.t<unit>
  let removeAndDestroyAll: unit => Promise.t<unit>
  let isEmpty: unit => bool
  let onDidRemove: (unit => unit) => VSCode.Disposable.t
  let onWillAdd: (unit => unit) => VSCode.Disposable.t
} = {
  open Belt

  let willAddChan = Chan.make()
  let didRemoveChan = Chan.make()

  // a dictionary of FileName-State entries
  let dict: Js.Dict.t<State.t> = Js.Dict.empty()

  let get = fileName => dict->Js.Dict.get(fileName)

  // do nothing if the state already exists
  let add = (fileName, state) =>
    switch get(fileName) {
    | Some(_) => ()
    | None =>
      willAddChan->Chan.emit()
      dict->Js.Dict.set(fileName, state)
    }

  // let rename = (oldName, newName) => {
  //   let delete_: (Js.Dict.t<'a>, string) => unit = %raw("function (dict, key) {delete dict[key]}")
  //   get(oldName)->Option.forEach(state => {
  //     delete_(dict, oldName)
  //     add(newName, state)
  //   })
  // }

  // remove the entry (but without triggering .destroy() )
  let remove = fileName => {
    let delete_: (Js.Dict.t<'a>, string) => unit = %raw("function (dict, key) {delete dict[key]}")
    delete_(dict, fileName)
    didRemoveChan->Chan.emit()
  }
  let removeAndDestroy = fileName =>
    switch get(fileName) {
    | None => Promise.resolved()
    | Some(state) =>
      remove(fileName)
      State.destroy(state, false)
    }

  let removeAndDestroyAll = () => {
    dict->Js.Dict.keys->Array.map(removeAndDestroy)->Util.oneByOne->Promise.map(_ => ())
  }

  let isEmpty = () => Js.Dict.keys(dict)->Array.length == 0

  let onDidRemove = callback => didRemoveChan->Chan.on(callback)->VSCode.Disposable.make
  let onWillAdd = callback => willAddChan->Chan.on(callback)->VSCode.Disposable.make
}

include Module
