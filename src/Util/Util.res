//
//  WARNING: No Node.js API calls are allowed in this module, because it is being imported by the View modules, which are sandboxed in a special environment.
//

module Disposable = {
  // Swap of Array.push
  let add = (disposables, disposable) => {
    Array.push(disposable, disposables)
  }
}

module Result = {
  let mapError = (x, f) =>
    switch x {
    | Error(e) => Error(f(e))
    | Ok(v) => Ok(v)
    }

  let partition = (xs: array<result<'a, 'b>>): (array<'a>, array<'b>) => {
    let errors = []
    let oks = []

    xs->Array.forEach(x =>
      switch x {
      | Error(a) => Array.push(errors, a)
      | Ok(b) => Array.push(oks, b)
      }
    )

    (oks, errors)
  }
}

module Decode = {
  open! JsonCombinators.Json.Decode
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
      if !Array.isArray(json) {
        Error.expected("array", json)
      }

      let arr: array<JSON.t> = Obj.magic(json)
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
      if !Array.isArray(json) {
        Error.expected("array", json)
      }

      let arr: array<JSON.t> = Obj.magic(json)
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
    | Payload(string, JSON.t)
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

module Version: {
  let compare: (string, string) => Ordering.t
  let gte: (string, string) => bool
} = {
  @module
  external compareVersionsPrim: (string, string) => int = "compare-versions"
  let trim = s => s->String.replaceRegExp(%re("/-.*/"), "")
  let compare = (a, b) =>
    switch compareVersionsPrim(trim(a), trim(b)) {
    | -1 => Ordering.less
    | 0 => Ordering.equal
    | _ => Ordering.greater
    }
  let gte = (a, b) => {
    let result = compare(a, b)
    Ordering.isGreater(result) || Ordering.isEqual(result)
  }
}

module Pretty = {
  let array = (xs, f) => "[" ++ (Array.join(Array.map(xs, f), ", ") ++ "]")
  let list = (xs, f) => xs->List.toArray->array(f)
  let map = (xs, f, g) =>
    xs
    ->Map.entries
    ->Iterator.toArray
    ->Array.map(((k, v)) => f(k) ++ ": " ++ g(v))
    ->(xs => "[" ++ (Array.join(xs, ", ") ++ "]"))
}

module JsError = {
  let toString = (e: Js.Exn.t): string => {
    // Use a more comprehensive JavaScript error extraction approach
    %raw("
      (function(error) {
        // Handle null/undefined
        if (!error) {
          return 'Unknown error (null/undefined)';
        }
        
        // If it's a proper Error object with stack, prefer stack trace
        if (error.stack && typeof error.stack === 'string') {
          return error.stack;
        }
        
        // Build error string from name and message
        let name = error.name || 'Error';
        let message = error.message || '';
        
        if (message) {
          return name + ': ' + message;
        }
        
        // Try toString() method
        if (typeof error.toString === 'function') {
          try {
            let stringified = error.toString();
            if (stringified !== '[object Object]' && stringified !== 'Error') {
              return stringified;
            }
          } catch (_) {
            // toString() failed, continue to next approach
          }
        }
        
        // Handle primitive values that were converted to Error objects
        if (typeof error.valueOf === 'function') {
          try {
            let value = error.valueOf();
            if (value !== error) {
              return String(value);
            }
          } catch (_) {
            // valueOf() failed, continue
          }
        }
        
        // Try to extract meaningful properties for structured errors
        if (typeof error === 'object') {
          try {
            // Common error properties to check
            let parts = [];
            if (error.code) parts.push('Code: ' + error.code);
            if (error.errno) parts.push('Errno: ' + error.errno);
            if (error.syscall) parts.push('Syscall: ' + error.syscall);
            if (error.path) parts.push('Path: ' + error.path);
            if (error.address) parts.push('Address: ' + error.address);
            if (error.port) parts.push('Port: ' + error.port);
            
            if (parts.length > 0) {
              return (name === 'Error' ? 'SystemError' : name) + ': ' + parts.join(', ');
            }
            
            // Last resort: JSON stringify
            return name + ': ' + JSON.stringify(error, null, 2);
          } catch (_) {
            // JSON.stringify failed
            return name + ': [Object (cannot serialize)]';
          }
        }
        
        // Final fallback for primitive values
        return name + ': ' + String(error);
      })
    ")(e)
  }
}

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

  let catch = async f =>
    switch await f() {
    | result => Ok(result)
    | exception Js.Exn.Error(e) => Error(e)
    }

  let setTimeout = async time => {
    let (promise, resolve, _) = pending()
    let id = Js.Global.setTimeout(resolve, time)
    await promise
    Js.Global.clearTimeout(id)
  }

  let rec oneByOne' = async xs =>
    switch xs {
    | list{} => list{}
    | list{x, ...xs} =>
      let x' = await x()
      let result = await oneByOne'(xs)
      list{x', ...result}
    }
  // execute promises one by one
  let oneByOne = async xs => {
    let xs' = await oneByOne'(List.fromArray(xs))
    List.toArray(xs')
  }
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

module String = {
  let lines = s => {
    s->String.splitByRegExp(%re("/\r\n|\n/g"))->Array.filterMap(x => x)
  }
  let unlines = xs => xs->Array.join("\n")
}

module Array = {
  // structural equality
  let includes = (xs: array<'a>, x: 'a) => Array.reduce(xs, false, (acc, y) => acc || x == y)
}

let log: (string, 'a) => unit = %raw(`
  function(message, payload) {
    console.log(message, payload);
  }
`)
