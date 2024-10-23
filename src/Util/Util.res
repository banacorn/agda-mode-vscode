module Result = {
  let mapError = (x, f) =>
    switch x {
    | Error(e) => Error(f(e))
    | Ok(v) => Ok(v)
    }
}

exception Error(string)

module Decode = {
  open JsonCombinators.Json.Decode
  type fieldType_<'a> =
    | Payload(t<'a>)
    | TagOnly('a)

  let sum = decoder => {
    field("tag", string)->flatMap(tag =>
      switch decoder(tag) {
      | Payload(d) => field("contents", d)
      | TagOnly(d) => custom(_ => d)
      }
    )
  }

  let tuple5 = (decodeA, decodeB, decodeC, decodeD, decodeE) => {
    custom(json => {
      if !Js.Array.isArray(json) {
        Error.expected("array", json)
      }

      let arr: array<Js.Json.t> = Obj.magic(json)
      if Array.length(arr) != 5 {
        raise(
          DecodeError(
            `Expected array of length 5, got array of length ${Array.length(arr)->string_of_int}`,
          ),
        )
      }

      let run = (decoder, xs, i) =>
        switch xs[i] {
        | Some(x) =>
          switch x->decode(decoder) {
          | Ok(x) => x
          | Error(err) => raise(DecodeError(err))
          }
        | None => raise(DecodeError("Unable to get index " ++ string_of_int(i)))
        }

      try (
        run(decodeA, arr, 0),
        run(decodeB, arr, 1),
        run(decodeC, arr, 2),
        run(decodeD, arr, 3),
        run(decodeE, arr, 4),
      ) catch {
      | DecodeError(msg) => raise(DecodeError(`${msg}\n\tin tuple5`))
      }
    })
  }

  let tuple6 = (decodeA, decodeB, decodeC, decodeD, decodeE, decodeF) => {
    custom(json => {
      if !Js.Array.isArray(json) {
        Error.expected("array", json)
      }

      let arr: array<Js.Json.t> = Obj.magic(json)
      if Array.length(arr) != 6 {
        raise(
          DecodeError(
            `Expected array of length 6, got array of length ${Array.length(arr)->string_of_int}`,
          ),
        )
      }

      let run = (decoder, xs, i) =>
        switch xs[i] {
        | Some(x) =>
          switch x->decode(decoder) {
          | Ok(x) => x
          | Error(err) => raise(DecodeError(err))
          }
        | None => raise(DecodeError("Unable to get index " ++ string_of_int(i)))
        }

      try (
        run(decodeA, arr, 0),
        run(decodeB, arr, 1),
        run(decodeC, arr, 2),
        run(decodeD, arr, 3),
        run(decodeE, arr, 4),
        run(decodeF, arr, 5),
      ) catch {
      | DecodeError(msg) => raise(DecodeError(`${msg}\n\tin tuple6`))
      }
    })
  }
}

module Encode = {
  open JsonCombinators.Json.Encode

  type fieldType<'a> =
    | Payload(string, Js.Json.t)
    | TagOnly('a)

  let sum = f => x =>
    switch f(x) {
    | Payload(tag, json) =>
      Unsafe.object({
        "tag": string(tag),
        "contents": json,
      })
    | TagOnly(tag) =>
      Unsafe.object({
        "tag": string(tag),
      })
    }

  let tuple5 = (encodeA, encodeB, encodeC, encodeD, encodeE, (a, b, c, d, e)) =>
    [a->encodeA, b->encodeB, c->encodeC, d->encodeD, e->encodeE]->jsonArray
  let tuple6 = (encodeA, encodeB, encodeC, encodeD, encodeE, encodeF, (a, b, c, d, e, f)) =>
    [a->encodeA, b->encodeB, c->encodeC, d->encodeD, e->encodeE, f->encodeF]->jsonArray
}

module React' = React
module React = {
  open React'

  let manyIn = (elems, elem) =>
    ReactDOMRe.createDOMElementVariadic(elem, ~props=ReactDOMRe.domProps(), elems)

  let manyIn2 = (elems, elem, props) => ReactDOMRe.createDOMElementVariadic(elem, ~props, elems)

  let sepBy' = (item: list<element>, sep: element) =>
    switch item {
    | list{} => <> </>
    | list{x} => x
    | list{x, ...xs} =>
      list{
        x,
        ...List.map(xs, i => <>
          sep
          i
        </>),
      }
      ->List.toArray
      ->manyIn("span")
    }
  let sepBy = (sep: element, xs) => xs->List.fromArray->sepBy'(sep)

  let enclosedBy = (front: element, back: element, item: element) => <>
    front
    {string(" ")}
    item
    {string(" ")}
    back
  </>

  let when_ = (p, className) => p ? " " ++ className : ""
  let showWhen = x =>
    switch x {
    | true => ""
    | false => " hidden"
    }
}

module Version = {
  type ordering =
    | LT
    | EQ
    | GT

  @module
  external compareVersionsPrim: (string, string) => int = "compare-versions"
  let trim = Js.String.replaceByRe(%re("/-.*/"), "")
  let compare = (a, b) =>
    switch compareVersionsPrim(trim(a), trim(b)) {
    | -1 => LT
    | 0 => EQ
    | _ => GT
    }
  let gte = (a, b) =>
    switch compare(a, b) {
    | EQ
    | GT => true
    | LT => false
    }
}

module Pretty = {
  let array = xs => "[" ++ (Js.Array.joinWith(", ", xs) ++ "]")
  let list = xs => xs->List.toArray->array
}

let rec oneByOne' = async x =>
  switch x {
  | list{} => list{}
  | list{x, ...xs} =>
    let x' = await x
    let result = await oneByOne'(xs)
    list{x', ...result}
  }

let oneByOne = async xs => {
  let xs' = await oneByOne'(List.fromArray(xs))
  List.toArray(xs')
}
module JsError = {
  let toString = (_e: Js.Exn.t): string => %raw("_e.toString()")
}

module List = {
  let rec span = (p, xs) =>
    switch xs {
    | list{} => (list{}, list{})
    | list{x, ...xs} =>
      if p(x) {
        let (ys, zs) = span(p, xs)
        (list{x, ...ys}, zs)
      } else {
        (list{}, xs)
      }
    }
  let rec dropWhile = (p, xs) =>
    switch xs {
    | list{} => list{}
    | list{x, ...xs} =>
      if p(x) {
        dropWhile(p, xs)
      } else {
        list{x, ...xs}
      }
    }
}

// module P = {
//   external toJsExn: Js.Promise.error => Js.Exn.t = "%identity"

//   let toPromise = (p: promise<result<'a, Js.Exn.t>>): Promise.t<result<'a, Js.Exn.t>> => {
//     p
//     ->Promise.Js.fromBsPromise
//     ->Promise.Js.toResult
//     ->Promise.map(x =>
//       switch x {
//       | Ok(Ok(x)) => Ok(x)
//       | Ok(Error(e)) => Error(e)
//       | Error(e) => Error(toJsExn(e))
//       }
//     )
//   }
// }

module Promise_ = {
  // Like `Promise.make` but without having to supply a callback
  let pending: unit => (promise<'a>, 'a => unit, 'e => unit) = () => {
    let resolve = ref(None)
    let reject = ref(None)
    let promise = Promise.make((res, rej) => {
      resolve := Some(res)
      reject := Some(rej)
    })

    switch (resolve.contents, reject.contents) {
    | (Some(resolve), Some(reject)) => (promise, resolve, reject)
    | _ => raise(Failure("Promise is not initialized"))
    }
  }
}

module String = {
  // let eol = switch VSCode.TextDocument.eol {
  // | VSCode.EndOfLine.LF => "\n"
  // | VSCode.EndOfLine.CRLF => "\r\n"
  // }

  // let lines = s => s->Js.String2.split(NodeJs.Os.eol)
  let lines = s => {
    s->Js.String2.splitByRe(%re("/\r\n|\n/g"))
  }
  // let unlines = xs => xs->Js.Array2.joinWith(NodeJs.Os.eol)
  let unlines = xs => xs->Js.Array2.joinWith("\n")
}