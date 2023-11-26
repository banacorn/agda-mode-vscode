open Belt

let split = s =>
  s
  ->Js.String.splitByRe(%re("/\r\n|\n/"), _)
  ->Array.map(x =>
    switch x {
    | None => None
    | Some("") => None
    | Some(chunk) => Some(chunk)
    }
  )
  ->Array.keepMap(x => x)

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
    initialContinuation: initialContinuation,
    continuation: ref(None),
    callback: callback,
  }

  // parsing with continuation
  let feed = (self: t<'a, 'e>, input: string): unit => {
    // get the existing continuation or initialize a new one
    let continue = self.continuation.contents->Option.getWithDefault(self.initialContinuation)

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

module SExpression = {
  type rec t =
    | A(string)
    | L(array<t>)

  let rec toString = x =>
    switch x {
    | A(s) => "\"" ++ (s ++ "\"")
    | L(xs) => "[" ++ (Js.Array.joinWith(", ", Array.map(xs, toString)) ++ "]")
    }

  type state = {
    stack: array<ref<t>>,
    word: ref<string>,
    escaped: ref<bool>,
    in_str: ref<bool>,
  }

  let preprocess = (string: string): result<string, string> =>
    if Js.String.substring(~from=0, ~to_=13, string) === "cannot read: " {
      Error(Js.String.sliceToEnd(~from=12, string))
    } else {
      Ok(string)
    }

  let rec flatten: t => array<string> = x =>
    switch x {
    | A(s) => [s]
    | L(xs) => xs->Array.map(flatten)->Array.concatMany
    }

  let parseWithContinuation = (string: string): Incr.continuation<t, (int, string)> => {
    let rec parseSExpression = (state: state, string: string): Incr.continuation<
      t,
      (int, string),
    > => {
      let {stack, word, escaped, in_str} = state

      let pushToTheTop = (elem: t) => {
        let index = Array.length(stack) - 1

        switch stack[index] {
        | Some(expr) =>
          switch expr.contents {
          | A(_) => expr := L([expr.contents, elem])
          | L(xs) => xs |> Js.Array.push(elem) |> ignore
          }
        | None => ()
        }
      }
      /* iterates through the string */
      let totalLength = String.length(string)

      for i in 0 to totalLength - 1 {
        let char = string |> Js.String.charAt(i)

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
          stack |> Js.Array.push(ref(L([]))) |> ignore
        } else if char == ")" && !in_str.contents {
          if word.contents != "" {
            pushToTheTop(A(word.contents))
            word := ""
          }
          switch stack |> Js.Array.pop {
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
      | 0 => Error((0, string))
      | 1 =>
        switch stack[0] {
        | None => Error((1, string))
        | Some(v) =>
          switch v.contents {
          | L(xs) =>
            switch xs[0] {
            | None => Continue(parseSExpression(state))
            | Some(w) => Done(w)
            }
          | _ => Error((3, string))
          }
        }
      | _ => Continue(parseSExpression(state))
      }
    }

    let initialState = () => {
      stack: [ref(L([]))],
      word: ref(""),
      escaped: ref(false),
      in_str: ref(false),
    }

    switch preprocess(string) {
    | Error(_) => Error((4, string))
    | Ok(processed) => parseSExpression(initialState(), processed)
    }
  }

  // returns an array of S-expressions and errors
  let parse = (input: string): array<result<t, (int, string)>> => {
    let resultAccum: ref<array<result<t, (int, string)>>> = ref([])
    let continuation = ref(None)
    input
    ->split
    ->Array.forEach(line => {
      // get the parsing continuation or initialize a new one
      let continue = continuation.contents->Option.getWithDefault(parseWithContinuation)

      // continue parsing with the given continuation
      switch continue(line) {
      | Error(err) => Js.Array.push(Error(err), resultAccum.contents) |> ignore
      | Continue(continue) => continuation := Some(continue)
      | Done(result) =>
        Js.Array.push(Ok(result), resultAccum.contents) |> ignore
        continuation := None
      }
    })
    resultAccum.contents
  }

  type incr = Incr.t<t, (int, string)>
  let makeIncr = callback => Incr.make(parseWithContinuation, callback)
}

// indicates at which stage the parse error happened
module Error = {
  type t =
    | SExpression(int, string)
    | Response(int, SExpression.t)

  let toString = x =>
    switch x {
    | SExpression(errno, string) =>
      "Something went wrong when parsing S-expressions. Error code: S" ++
      (string_of_int(errno) ++
      (" \"" ++ (string ++ "\"")))
    | Response(errno, sexpr) =>
      "Perhaps the underlying protocol used by Agda for communicating with agda-mode has changed.\nPlease report which version of Agda you are using.\nError code: R" ++
      (string_of_int(errno) ++
      (" \"" ++ (SExpression.toString(sexpr) ++ "\"")))
    }
}

let userInput = (s: string): string =>
  // let trim = s =>
  //   Atom.Config.get("agda-mode.trimSpaces") ? String.trim(s) : s;
  s
  |> Js.String.replaceByRe(%re("/\\\\/g"), "\\\\")
  |> Js.String.replaceByRe(%re("/\\\"/g"), "\\\"")
  |> Js.String.replaceByRe(%re("/\\n/g"), "\\n")
  |> Js.String.trim

let filepath = s => {
  // remove the Windows Bidi control character
  let removedBidi = if Js.String.charCodeAt(0, s) === 8234.0 {
    Js.String.sliceToEnd(~from=1, s)
  } else {
    s
  }

  // normalize the path with Node.Path.normalize
  let normalized = Node.Path.normalize(removedBidi)

  // replace Windows' stupid backslash with slash
  let replaced = Js.String.replaceByRe(%re("/\\\\/g"), "/", normalized)

  replaced
}

let agdaOutput = Js.String.replaceByRe(%re("/\\\\n/g"), "\n")

let commandLineArgs = s => s |> Js.String.replaceByRe(%re("/\\s+/g"), " ") |> Js.String.split(" ")
