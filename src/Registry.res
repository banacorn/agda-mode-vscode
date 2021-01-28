open Belt

// a dictionary of FileName-State entries
let dict: Js.Dict.t<State.t> = Js.Dict.empty()

let get = fileName => dict->Js.Dict.get(fileName)

// do nothing if the state already exists
let add = (fileName, state) =>
  switch get(fileName) {
  | Some(_) => ()
  | None => dict->Js.Dict.set(fileName, state)
  }

let rename = (oldName, newName) => {
  let delete_: (Js.Dict.t<'a>, string) => unit = %raw("function (dict, key) {delete dict[key]}")
  get(oldName)->Option.forEach(state => {
    delete_(dict, oldName)
    add(newName, state)
  })
}

// remove the entry (but without triggering .destroy() )
let remove = fileName => {
  let delete_: (Js.Dict.t<'a>, string) => unit = %raw("function (dict, key) {delete dict[key]}")
  delete_(dict, fileName)
}
let removeAndDestroy = fileName => {
  get(fileName)->Option.forEach(state => State.destroy(state, false)->ignore)
  remove(fileName)
}
let removeAndDestroyAll = () => {
  dict->Js.Dict.keys->Array.forEach(removeAndDestroy)
}

let size = () => Js.Dict.keys(dict)->Array.length
