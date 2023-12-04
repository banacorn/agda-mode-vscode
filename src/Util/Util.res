open Belt

module Result = {
  let mapError = (x, f) =>
    switch x {
    | Error(e) => Error(f(e))
    | Ok(v) => Ok(v)
    }
}

exception Error(string)

module Decode = {
  exception TempDecodeError(string, string, Js.Json.t)
  let convert = (label, translation, json) =>
    switch json->JsonCombinators.Json.decode(translation) {
    | Ok(translation) => translation
    | Error(err) =>
      Js.log(err)
      raise(TempDecodeError(label, err, json))
    }

  type fieldType_<'a> =
    | Payload(JsonCombinators.Json.Decode.t<'a>)
    | TagOnly('a)

  let sum_ = decoder => {
    open JsonCombinators.Json.Decode
    field("tag", string)->flatMap((. tag) =>
      switch decoder(tag) {
      | Payload(d) => field("contents", d)
      | TagOnly(d) => custom((. _) => d)
      }
    )
  }

  let tuple6_ = (decodeA, decodeB, decodeC, decodeD, decodeE, decodeF) => {
    open JsonCombinators.Json.Decode
    custom((. json) => {
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

      // arr[0]->Option.flatMap(x => decode(x, decodeA))

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

  let tuple5_ = (decodeA, decodeB, decodeC, decodeD, decodeE) => {
    open JsonCombinators.Json.Decode
    custom((. json) => {
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

      // arr[0]->Option.flatMap(x => decode(x, decodeA))

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

  open Json.Decode

  type fieldType<'a> =
    | Contents(decoder<'a>)
    | TagOnly('a)

  let sum = decoder =>
    field("tag", string) |> andThen(tag =>
      switch decoder(tag) {
      | Contents(d) => field("contents", d)
      | TagOnly(d) => _ => d
      }
    )

  let maybe: decoder<'a> => decoder<option<'a>> = decoder =>
    sum(x =>
      switch x {
      | "Just" => Contents(json => Some(decoder(json)))
      | _ => TagOnly(None)
      }
    )

  let tuple5 = (decodeA, decodeB, decodeC, decodeD, decodeE, json) =>
    if Js.Array.isArray(json) {
      let source: array<Js.Json.t> = Obj.magic((json: Js.Json.t))
      let length = Js.Array.length(source)
      if length == 5 {
        try (
          decodeA(Js.Array.unsafe_get(source, 0)),
          decodeB(Js.Array.unsafe_get(source, 1)),
          decodeC(Js.Array.unsafe_get(source, 2)),
          decodeD(Js.Array.unsafe_get(source, 3)),
          decodeE(Js.Array.unsafe_get(source, 4)),
        ) catch {
        | DecodeError(msg) => raise(DecodeError(msg ++ "\n\tin tuple5"))
        }
      } else {
        raise(
          DecodeError(`Expected array of length 5, got array of length ${Int.toString(length)}`),
        )
      }
    } else {
      raise(DecodeError("Expected array, got " ++ Js.Json.stringify(json)))
    }

  let tuple6 = (decodeA, decodeB, decodeC, decodeD, decodeE, decodeF, json) =>
    if Js.Array.isArray(json) {
      let source: array<Js.Json.t> = Obj.magic((json: Js.Json.t))
      let length = Js.Array.length(source)
      if length == 6 {
        try (
          decodeA(Js.Array.unsafe_get(source, 0)),
          decodeB(Js.Array.unsafe_get(source, 1)),
          decodeC(Js.Array.unsafe_get(source, 2)),
          decodeD(Js.Array.unsafe_get(source, 3)),
          decodeE(Js.Array.unsafe_get(source, 4)),
          decodeF(Js.Array.unsafe_get(source, 5)),
        ) catch {
        | DecodeError(msg) => raise(DecodeError(msg ++ "\n\tin tuple6"))
        }
      } else {
        raise(
          DecodeError(`Expected array of length 6, got array of length ${Int.toString(length)}`),
        )
      }
    } else {
      raise(DecodeError("Expected array, got " ++ Js.Json.stringify(json)))
    }
}

module Encode = {
  open JsonCombinators.Json.Encode

  type fieldType<'a> =
    | Payload(string, Js.Json.t)
    | TagOnly('a)

  let sum = (f, x) =>
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

let rec oneByOne' = x =>
  switch x {
  | list{} => Promise.resolved(list{})
  | list{x, ...xs} => x->Promise.flatMap(x' => oneByOne'(xs)->Promise.map(xs' => list{x', ...xs'}))
  }

let oneByOne = xs => oneByOne'(List.fromArray(xs))->Promise.map(List.toArray)

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

module P = {
  external toJsExn: Js.Promise.error => Js.Exn.t = "%identity"

  let toPromise = (p: promise<result<'a, Js.Exn.t>>): Promise.t<result<'a, Js.Exn.t>> => {
    p
    ->Promise.Js.fromBsPromise
    ->Promise.Js.toResult
    ->Promise.map(x =>
      switch x {
      | Ok(Ok(x)) => Ok(x)
      | Ok(Error(e)) => Error(e)
      | Error(e) => Error(toJsExn(e))
      }
    )
  }
}
