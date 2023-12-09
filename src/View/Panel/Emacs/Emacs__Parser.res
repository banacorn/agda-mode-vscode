open Belt

module Dict = {
  open Js.Dict
  let partite = (xs: array<'a>, tagEntry: (('a, int)) => option<string>): t<array<'a>> => {
    let keys: array<(key, int)> =
      xs
      ->Array.mapWithIndex((i, x) => (i, x)) /* zip with index */
      ->Array.keepMap(((i, x)) => tagEntry((x, i))->Option.map(key => (key, i)))
    let intervals: array<(key, int, int)> = keys->Array.mapWithIndex((n, (key, index)) =>
      switch keys[n + 1] {
      | Some((_, next)) => (key, index, next)
      | None => (key, index, Array.length(xs))
      }
    )
    intervals->Array.map(((key, start, end_)) => (
      key,
      xs->Js.Array.slice(~start, ~end_)->Array.keep(x => x !== ""),
    )) |> fromArray
  }
  // given a key and a splitter function, split the value of the key into multiple entries
  // and replace the old entry with the new ones
  let split = (dict: t<'a>, key: key, splitter: 'a => t<'a>): t<array<string>> =>
    switch get(dict, key) {
    | Some(value) =>
      // insert new entries
      entries(splitter(value))->Array.forEach(((k, v)) => set(dict, k, v))
      // remove old entry
      Util.Dict.delete(dict, key)
      dict
    | None => dict
    }
  let update = (dict: t<'a>, key: key, f: 'a => 'a): t<'a> =>
    switch get(dict, key) {
    | Some(value) =>
      set(dict, key, f(value))
      dict
    | None => dict
    }
}

module Array_ = {
  let partite = (xs: array<'a>, p: 'a => bool): array<array<'a>> => {
    let indices: array<int> =
      xs
      ->Array.mapWithIndex((i, x) => (i, x)) /* zip with index */
      ->Array.keep(((_, x)) => p(x)) /* filter bad indices out */
      ->Array.map(fst) /* leave only the indices */
    /* prepend 0 as the first index */
    let indicesWF: array<int> = switch indices[0] {
    | Some(n) => n === 0 ? indices : Array.concat(indices, [0])
    | None => Array.length(indices) === 0 ? [0] : indices
    }
    let intervals: array<(int, int)> = indicesWF->Array.mapWithIndex((n, index) =>
      switch indicesWF[n + 1] {
      | Some(next) => (index, next)
      | None => (index, Array.length(xs))
      }
    )
    intervals->Array.map(((start, end_)) => xs |> Js.Array.slice(~start, ~end_))
  }
  let mergeWithNext: (array<array<'a>>, array<'a> => bool) => array<array<'a>> = (xs, p) =>
    xs->Array.reduce([], (acc, x) => {
      let last = acc[Array.length(acc) - 1]
      switch last {
      | None => [x]
      | Some(l) =>
        if p(l) {
          let _ = acc[Array.length(acc) - 1] = Array.concat(x, l)
          acc
        } else {
          Array.concat([x], acc)
        }
      }
    })
}

// A meta may be split into multiple entries because it's too long
// this function glues them back together
let aggregateLines: array<string> => array<string> = lines => {
  // A line is considered a new line if it starts with a non-whitespace character
  // otherwise it's a continuation of the previous line
  let newlineRegEx = line => Js.Re.test_(%re("/^\S/"), line)
  let newLineIndices: array<int> =
    lines
    ->Array.mapWithIndex((index, line) => (index, newlineRegEx(line)))
    ->Array.keep(((_, isNewline)) => isNewline)
    ->Array.map(fst)
  newLineIndices
  ->Array.mapWithIndex((i, index) =>
    switch newLineIndices[i + 1] {
    | None => (index, Array.length(lines) + 1)
    | Some(n) => (index, n)
    }
  )
  ->Array.map(((start, end_)) => lines->Js.Array2.slice(~start, ~end_)->Util.String.unlines)
}

let captures = (regex, handler, raw) =>
  Js.Re.exec_(regex, raw)
  ->Option.map(result => result->Js.Re.captures->Array.map(Js.Nullable.toOption))
  ->Option.flatMap(handler)

let choice = (res: array<string => option<'a>>, raw) => Js.Array.reduce((result, parse) =>
    switch result {
    /* Done, pass it on */
    | Some(value) => Some(value)
    /* Failed, try this one */
    | None => parse(raw)
    }
  , None, res)

let at = (captured: array<option<string>>, i: int, parser: string => option<'a>): option<'a> =>
  if i >= Array.length(captured) {
    None
  } else {
    captured[i]->Option.flatMap(x => x)->Option.flatMap(parser)
  }
