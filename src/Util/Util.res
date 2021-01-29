open Belt

exception Error(string)

module Decode = {
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
        raise(DecodeError(j`Expected array of length 5, got array of length $length`))
      }
    } else {
      raise(DecodeError("Expected array, got " ++ Js.Json.stringify(json)))
    }
}

module Encode = {
  open Json.Encode
  let tuple5 = (encodeA, encodeB, encodeC, encodeD, encodeE, (a, b, c, d, e)) =>
    jsonArray([encodeA(a), encodeB(b), encodeC(c), encodeD(d), encodeE(e)])
}

module React = {
  open ReasonReact

  let manyIn = (elems, elem) =>
    ReactDOMRe.createDOMElementVariadic(elem, ~props=ReactDOMRe.domProps(), elems)

  let manyIn2 = (elems, elem, props) => ReactDOMRe.createDOMElementVariadic(elem, ~props, elems)

  let sepBy' = (item: list<reactElement>, sep: reactElement) =>
    switch item {
    | list{} => <> </>
    | list{x} => x
    | list{x, ...xs} => list{x, ...List.map(xs, i => <> sep i </>)}->List.toArray->manyIn("span")
    }
  let sepBy = (sep: reactElement, xs) => xs->List.fromArray->sepBy'(sep)

  let enclosedBy = (front: reactElement, back: reactElement, item: reactElement) => <>
    front {string(" ")} item {string(" ")} back
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

  @bs.module
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
  let toString = (_e: Js.Exn.t) => %raw("_e.toString()")
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
