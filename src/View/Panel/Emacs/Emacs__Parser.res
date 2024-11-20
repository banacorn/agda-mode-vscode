module Dictionary = {
  let partite = (xs: array<'a>, tagEntry: (('a, int)) => option<string>): Dict.t<array<'a>> => {
    let keys: array<(string, int)> =
      xs
      ->Array.mapWithIndex((x, i) => (i, x)) /* zip with index */
      ->Array.filterMap(((i, x)) => tagEntry((x, i))->Option.map(key => (key, i)))
    let intervals: array<(string, int, int)> = keys->Array.mapWithIndex(((key, index), n) =>
      switch keys[n + 1] {
      | Some((_, next)) => (key, index, next)
      | None => (key, index, Array.length(xs))
      }
    )
    intervals
    ->Array.map(((key, start, end)) => (
      key,
      xs->Array.slice(~start, ~end)->Array.filter(x => x !== ""),
    ))
    ->Dict.fromArray
  }
  // given a key and a splitter function, split the value of the key into multiple entries
  // and replace the old entry with the new ones
  let split = (dict: Dict.t<'a>, key: string, splitter: 'a => Dict.t<'a>): Dict.t<array<string>> =>
    switch Dict.get(dict, key) {
    | Some(value) =>
      // insert new entries
      Dict.toArray(splitter(value))->Array.forEach(((k, v)) => Dict.set(dict, k, v))
      // remove old entry
      Dict.delete(dict, key)
      dict
    | None => dict
    }
  let update = (dict: Dict.t<'a>, key: string, f: 'a => 'a): Dict.t<'a> =>
    switch Dict.get(dict, key) {
    | Some(value) =>
      Dict.set(dict, key, f(value))
      dict
    | None => dict
    }
}

module Array_ = {
  let partite = (xs: array<'a>, p: 'a => bool): array<array<'a>> => {
    let indices: array<int> =
      xs
      ->Array.mapWithIndex((x, i) => (i, x)) /* zip with index */
      ->Array.filter(((_, x)) => p(x)) /* filter bad indices out */
      ->Array.map(fst) /* leave only the indices */
    /* prepend 0 as the first index */
    let indicesWF: array<int> = switch indices[0] {
    | Some(n) => n === 0 ? indices : Array.concat(indices, [0])
    | None => Array.length(indices) === 0 ? [0] : indices
    }
    let intervals: array<(int, int)> = indicesWF->Array.mapWithIndex((index, n) =>
      switch indicesWF[n + 1] {
      | Some(next) => (index, next)
      | None => (index, Array.length(xs))
      }
    )
    intervals->Array.map(((start, end)) => Array.slice(~start, ~end, xs))
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
  let newlineRegEx = line => RegExp.test(%re("/^\S/"), line)
  let newLineIndices: array<int> =
    lines
    ->Array.mapWithIndex((line, index) => (index, newlineRegEx(line)))
    ->Array.filter(((_, isNewline)) => isNewline)
    ->Array.map(fst)
  newLineIndices
  ->Array.mapWithIndex((index, i) =>
    switch newLineIndices[i + 1] {
    | None => (index, Array.length(lines) + 1)
    | Some(n) => (index, n)
    }
  )
  ->Array.map(((start, end)) => lines->Array.slice(~start, ~end)->Util.String.unlines)
}

let captures = (regex, handler: array<option<string>> => option<'a>, raw) =>
  RegExp.exec(regex, raw)->Option.flatMap(handler)

let choice = (res: array<string => option<'a>>, raw) =>
  res->Array.reduce(None, (result, parse) =>
    switch result {
    /* Done, pass it on */
    | Some(value) => Some(value)
    /* Failed, try this one */
    | None => parse(raw)
    }
  )

let at = (captured: array<option<string>>, i: int, parser: string => option<'a>): option<'a> =>
  if i >= Array.length(captured) {
    None
  } else {
    captured[i]->Option.flatMap(x => x)->Option.flatMap(parser)
  }
