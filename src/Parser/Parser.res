let splitToLines = s =>
  // RegEx updated to v10.1.4
  s
  ->String.splitByRegExp(%re("/\r\n|\n/"))
  ->Array.map(x =>
    switch x {
    | None => None
    | Some(chunk) => Some(chunk)
    }
  )
  ->Array.filterMap(x => x)

module Incr = {
  module Gen = {
    type t<'a> =
      | Yield('a)
      | Stop
    let map = (f, x) =>
      switch x {
      | Yield(x) => Yield(f(x))
      | Stop => Stop
      }
    let tap = (f, x) =>
      switch x {
      | Yield(x) =>
        f(x)
        Yield(x)
      | Stop => Stop
      }
    let flatMap = (f, x) =>
      switch x {
      | Yield(x) => f(x)
      | Stop => Stop
      }
  }

  type rec continuation<'a, 'e> =
    | Error('e)
    | Continue(string => continuation<'a, 'e>)
    | Done('a)

  type t<'a, 'e> = {
    initialContinuation: string => continuation<'a, 'e>,
    continuation: ref<option<string => continuation<'a, 'e>>>,
    callback: Gen.t<result<'a, 'e>> => unit,
  }

  let make = (initialContinuation, callback) => {
    initialContinuation,
    continuation: ref(None),
    callback,
  }

  // parsing with continuation
  let feed = (self: t<'a, 'e>, input: string): unit => {
    // get the existing continuation or initialize a new one
    let continue = self.continuation.contents->Option.getOr(self.initialContinuation)

    // continue parsing with the given continuation
    switch continue(input) {
    | Error(err) => self.callback(Yield(Error(err)))
    | Continue(continue) => self.continuation := Some(continue)
    | Done(result) =>
      self.callback(Yield(Ok(result)))
      self.continuation := None
    }
  }

  let stop = (self: t<'a, 'e>): unit => self.callback(Stop)
}

/* Parsing S-Expressions */
/* Courtesy of @NightRa */

// indicates at which stage the parse error happened
module SExprParseError = {
  type t = StackEmpty | StackElementNullReference | PreprocessError

  let toString = x => {
    switch x {
    | StackEmpty => "StackEmpty"
    | StackElementNullReference => "StackElementNullReference"
    | PreprocessError => "PreprocessError"
    }
  }
}

module SExpression = {
  type rec t =
    | A(string)
    | L(array<t>)

  let rec toString = x =>
    switch x {
    | A(s) => "\"" ++ (s ++ "\"")
    | L(xs) => "[" ++ (xs->Array.map(toString)->Array.join(", ") ++ "]")
    }

  type state = {
    stack: array<ref<t>>,
    word: ref<string>,
    escaped: ref<bool>,
    in_str: ref<bool>,
  }

  let preprocess = (string: string): result<string, string> =>
    if String.substring(~start=0, ~end=13, string) === "cannot read: " {
      Error(String.sliceToEnd(~start=12, string))
    } else {
      Ok(string)
    }

  let rec flatten: t => array<string> = x =>
    switch x {
    | A(s) => [s]
    | L(xs) => xs->Array.map(flatten)->Array.flat
    }

  let parseWithContinuation = (string: string): Incr.continuation<
    t,
    (SExprParseError.t, string),
  > => {
    let rec parseSExpression = (state: state, string: string): Incr.continuation<
      t,
      (SExprParseError.t, string),
    > => {
      let {stack, word, escaped, in_str} = state

      let pushToTheTop = (elem: t) => {
        let index = Array.length(stack) - 1

        switch stack[index] {
        | Some(expr) =>
          switch expr.contents {
          | A(_) => expr := L([expr.contents, elem])
          | L(xs) => xs->Array.push(elem)
          }
        | None => ()
        }
      }
      /* iterates through the string */
      let totalLength = String.length(string)

      for i in 0 to totalLength - 1 {
        let char = string->String.charAt(i)

        if escaped.contents {
          /* something was being escaped */
          /* put the backslash \ back in */
          if char == "n" {
            word := word.contents ++ "\\"
          }
          word := word.contents ++ char
          escaped := false
        } else if char == "\'" && !in_str.contents {
          ()
        } else if char == "(" && !in_str.contents {
          stack->Array.push(ref(L([])))
        } else if char == ")" && !in_str.contents {
          if word.contents != "" {
            pushToTheTop(A(word.contents))
            word := ""
          }
          switch Array.pop(stack) {
          | Some(expr) => pushToTheTop(expr.contents)
          | None => ()
          }
        } else if char == " " && !in_str.contents {
          if word.contents != "" {
            pushToTheTop(A(word.contents))
            word := ""
          }
        } else if char == "\"" {
          in_str := !in_str.contents
        } else if char == "\\" && in_str.contents {
          /* something is being escaped */
          escaped := true
        } else {
          word := word.contents ++ char
        }
      }
      switch Array.length(stack) {
      | 0 => Error((StackEmpty, string))
      | 1 =>
        switch stack[0] {
        | None => Error((StackEmpty, string))
        | Some(v) =>
          switch v.contents {
          | L(xs) =>
            switch xs[0] {
            | None => Continue(parseSExpression(state, ...))
            | Some(w) => Done(w)
            }
          | _ => Error((StackElementNullReference, string))
          }
        }
      | _ => Continue(parseSExpression(state, ...))
      }
    }

    let initialState = () => {
      stack: [ref(L([]))],
      word: ref(""),
      escaped: ref(false),
      in_str: ref(false),
    }

    switch preprocess(string) {
    | Error(_) => Error((PreprocessError, string))
    | Ok(processed) => parseSExpression(initialState(), processed)
    }
  }

  // returns an array of S-expressions and errors
  let parse = (input: string): array<result<t, (SExprParseError.t, string)>> => {
    let resultAccum: ref<array<result<t, (SExprParseError.t, string)>>> = ref([])
    let continuation = ref(None)
    input
    ->splitToLines
    ->Array.forEach(line => {
      // get the parsing continuation or initialize a new one
      let continue = continuation.contents->Option.getOr(parseWithContinuation)

      // continue parsing with the given continuation
      switch continue(line) {
      | Error(err) => resultAccum.contents->Array.push(Error(err))
      | Continue(continue) => continuation := Some(continue)
      | Done(result) =>
        resultAccum.contents->Array.push(Ok(result))
        continuation := None
      }
    })
    resultAccum.contents
  }

  type incr = Incr.t<t, (SExprParseError.t, string)>
  let makeIncr = callback => Incr.make(parseWithContinuation, callback)
}

// indicates at which stage the parse error happened
module Error = {
  type t =
    | SExpression(SExprParseError.t, string)
    | Response(int, SExpression.t)

  let toString = x =>
    switch x {
    | SExpression(error, string) =>
      "Something went wrong when parsing S-expressions. Error code " ++
      (SExprParseError.toString(error) ++
      (" \"" ++ (string ++ "\"")))
    | Response(errno, sexpr) =>
      "Perhaps the underlying protocol used by Agda for communicating with agda-mode has changed.\nPlease report which version of Agda you are using.\nError code: R" ++
      (string_of_int(errno) ++
      (" \"" ++ (SExpression.toString(sexpr) ++ "\"")))
    }
}

module Filepath: {
  type t
  // constructor & destructor
  let make: string => t
  let toString: t => string
} = {
  type t = string

  // remove the Windows Bidi control character
  let removedBidi = raw =>
    if String.charCodeAt(raw, 0) === 8234.0 {
      String.sliceToEnd(~start=1, raw)
    } else {
      raw
    }

  let make = raw => {
    // replace all backslashes OR slashes WITH platform-specific path separator
    let replaceSeparator = s => s->String.replaceAllRegExp(%re("/[\\/]/g"), NodeJs.Path.sep)

    // convert small case Windows roots to upper case
    let makeRootsUpperCaseOnWindows = path => {
      let obj = NodeJs.Path.parse(path)
      let rootLength = String.length(obj.root)
      let oldRoot = String.slice(~start=0, ~end=rootLength, path)
      let rest = String.sliceToEnd(~start=rootLength, path)
      let newRoot = String.toUpperCase(oldRoot)
      newRoot ++ rest
    }

    raw->removedBidi->NodeJs.Path.normalize->replaceSeparator->makeRootsUpperCaseOnWindows
  }

  // Like `NodeJs.Path.format`, but:
  //  1. Make all characters in the root upper case
  //  2. Replace all backslashes OR slashes WITH platform-specific path separator
  // ┌─────────────────────┬────────────┐
  // │          dir        │    base    │
  // ├──────┬              ├──────┬─────┤
  // │ root │              │ name │ ext │
  // " C:\      path\dir   \ file  .txt "
  // └──────┴──────────────┴──────┴─────┘
  let toString = x => x
}

// 1. Normalize the path with `Node.Path.normalize`
// 2. Remove the Windows Bidi control character
// 3. Replace Windows' backslash with slash
// 4. Convert small case Windows roots from Agda like "c://" to "C://" (Issue #44)
let filepath = s => {
  // remove the Windows Bidi control character
  let removedBidi = if String.charCodeAt(s, 0) === 8234.0 {
    String.sliceToEnd(~start=1, s)
  } else {
    s
  }

  // normalize the path with Node.Path.normalize
  let normalized = NodeJs.Path.normalize(removedBidi)

  // convert small case Windows roots to upper case
  let makeRootsUpperCaseOnWindows = path => {
    let obj = NodeJs.Path.parse(path)
    let rootLength = String.length(obj.root)
    let oldRoot = String.slice(~start=0, ~end=rootLength, path)
    let rest = String.sliceToEnd(~start=rootLength, path)
    let newRoot = String.toUpperCase(oldRoot)
    newRoot ++ rest
  }

  let upperCased = if !OS.onUnix {
    let result = makeRootsUpperCaseOnWindows(normalized)
    result
  } else {
    normalized
  }

  // replace Windows' stupid backslash with slash
  let replaced = upperCased->String.replaceRegExp(%re("/\\/g"), "/")

  replaced
}

// Escapes human-readable strings to Agda-readable strings,
// by inserting backslashes before the following characters:
//
//      LF      => \n
//      CR LF   => \r\n
//      "       -> \"
//      \       -> \\
let escape = (s: string): string =>
  s
  ->String.replaceRegExp(%re("/\\/g"), "\\\\")
  ->String.replaceRegExp(%re("/\"/g"), "\\\"")
  ->String.replaceRegExp(%re("/\r\n/g"), "\\r\\n")
  ->String.replaceRegExp(%re("/\n/g"), "\\n")

// Almost the inverse of escape, but only for EOL characters.
//
//      \n    => LF
//      \r\n  => CR LF
let unescapeEOL = (s: string): string =>
  s
  ->String.replaceRegExp(%re("/\\r\\n/g"), "\r\n")
  ->String.replaceRegExp(%re("/\\n/g"), "\n")
