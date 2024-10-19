// NOTE:
//
//  VSCode imports should not be allowed in this module, otherwise it would contaminate the view
//

module AgdaPosition = {
  type t = {
    line: int,
    col: int,
    pos: int,
  }

  let decode = {
    open JsonCombinators.Json.Decode
    tuple3(int, int, int)->map((. (line, col, pos)) => {
      line,
      col,
      pos,
    })
  }

  let encode = ({line, col, pos}) => {
    open JsonCombinators.Json.Encode
    tuple3(int, int, int)((line, col, pos))
  }
}

module AgdaInterval = {
  type t = {
    start: AgdaPosition.t,
    end_: AgdaPosition.t,
  }

  let make = (start, end_) => {start, end_}

  let fuse = (a, b) => {
    let start = if a.start.pos > b.start.pos {
      b.start
    } else {
      a.start
    }
    let end_ = if a.end_.pos > b.end_.pos {
      a.end_
    } else {
      b.end_
    }
    {start, end_}
  }

  let toString = (self): string =>
    if self.start.line === self.end_.line {
      string_of_int(self.start.line) ++
      ("," ++
      (string_of_int(self.start.col) ++ ("-" ++ string_of_int(self.end_.col))))
    } else {
      string_of_int(self.start.line) ++
      ("," ++
      (string_of_int(self.start.col) ++
      ("-" ++
      (string_of_int(self.end_.line) ++ ("," ++ string_of_int(self.end_.col))))))
    }

  let decode = {
    open JsonCombinators.Json.Decode
    tuple2(AgdaPosition.decode, AgdaPosition.decode)->map((. (start, end_)) => {
      start,
      end_,
    })
  }

  let encode = ({start, end_}) => {
    open JsonCombinators.Json.Encode
    tuple2(AgdaPosition.encode, AgdaPosition.encode)((start, end_))
  }
}

module AgdaRange = {
  type t =
    | NoRange
    | Range(option<string>, array<AgdaInterval.t>)

  let parse = %re(
    // Regex updated to v10.1.4
    // There are 3 types of range:
    //  type 1: filepath:line,col-line,col
    //  type 2: filepath:line,col-col
    //  type 3: filepath:line,col

    /* filepath  | line,col-line,col       |    line,col-col   |   line,col | */
    "/^(\S+)\:(?:(\d+)\,(\d+)\-(\d+)\,(\d+)|(\d+)\,(\d+)\-(\d+)|(\d+)\,(\d+))$/"
  )->Emacs__Parser.captures(captured => {
    open Belt
    open Belt.Option
    let flatten = xs => xs->flatMap(x => x)
    // filepath: captured[1]
    // type 1: captured[2] ~ captured[5]
    // type 2: captured[6] ~ captured[8]
    // type 3: captured[9] ~ captured[10]
    let srcFile = captured[1]->flatten
    let isType1 = captured[2]->flatten->isSome
    let isType2 = captured[6]->flatten->isSome
    if isType1 {
      captured[2]
      ->flatten
      ->flatMap(int_of_string_opt)
      ->flatMap(rowStart =>
        captured[3]
        ->flatten
        ->flatMap(int_of_string_opt)
        ->flatMap(
          colStart =>
            captured[4]
            ->flatten
            ->flatMap(int_of_string_opt)
            ->flatMap(
              rowEnd =>
                captured[5]
                ->flatten
                ->flatMap(int_of_string_opt)
                ->flatMap(
                  colEnd => Some(
                    Range(
                      srcFile,
                      [
                        {
                          start: {
                            pos: 0,
                            line: rowStart,
                            col: colStart,
                          },
                          end_: {
                            pos: 0,
                            line: rowEnd,
                            col: colEnd,
                          },
                        },
                      ],
                    ),
                  ),
                ),
            ),
        )
      )
    } else if isType2 {
      captured[6]
      ->flatten
      ->flatMap(int_of_string_opt)
      ->flatMap(row =>
        captured[7]
        ->flatten
        ->flatMap(int_of_string_opt)
        ->flatMap(
          colStart =>
            captured[8]
            ->flatten
            ->flatMap(int_of_string_opt)
            ->flatMap(
              colEnd => Some(
                Range(
                  srcFile,
                  [
                    {
                      start: {
                        pos: 0,
                        line: row,
                        col: colStart,
                      },
                      end_: {
                        pos: 0,
                        line: row,
                        col: colEnd,
                      },
                    },
                  ],
                ),
              ),
            ),
        )
      )
    } else {
      captured[9]
      ->flatten
      ->flatMap(int_of_string_opt)
      ->flatMap(row =>
        captured[10]
        ->flatten
        ->flatMap(int_of_string_opt)
        ->flatMap(
          col => Some(
            Range(
              srcFile,
              [
                {
                  start: {
                    pos: 0,
                    line: row,
                    col,
                  },
                  end_: {
                    pos: 0,
                    line: row,
                    col,
                  },
                },
              ],
            ),
          ),
        )
      )
    }
  }, ...)

  let fuse = (a: t, b: t): t => {
    open AgdaInterval

    let mergeTouching = (l, e, s, r) =>
      Belt.List.concat(Belt.List.concat(l, list{{start: e.start, end_: s.end_}}), r)

    let rec fuseSome = (s1, r1, s2, r2) => {
      let r1' = Util.List.dropWhile(x => x.end_.pos <= s2.end_.pos, r1)
      helpFuse(r1', list{AgdaInterval.fuse(s1, s2), ...r2})
    }
    and outputLeftPrefix = (s1, r1, s2, is2) => {
      let (r1', r1'') = Util.List.span(s => s.end_.pos < s2.start.pos, r1)
      Belt.List.concat(Belt.List.concat(list{s1}, r1'), helpFuse(r1'', is2))
    }
    and helpFuse = (a: Belt.List.t<AgdaInterval.t>, b: Belt.List.t<AgdaInterval.t>) =>
      switch (a, Belt.List.reverse(a), b, Belt.List.reverse(b)) {
      | (list{}, _, _, _) => a
      | (_, _, list{}, _) => b
      | (list{s1, ...r1}, list{e1, ...l1}, list{s2, ...r2}, list{e2, ...l2}) =>
        if e1.end_.pos < s2.start.pos {
          Belt.List.concat(a, b)
        } else if e2.end_.pos < s1.start.pos {
          Belt.List.concat(b, a)
        } else if e1.end_.pos === s2.start.pos {
          mergeTouching(l1, e1, s2, r2)
        } else if e2.end_.pos === s1.start.pos {
          mergeTouching(l2, e2, s1, r1)
        } else if s1.end_.pos < s2.start.pos {
          outputLeftPrefix(s1, r1, s2, b)
        } else if s2.end_.pos < s1.start.pos {
          outputLeftPrefix(s2, r2, s1, a)
        } else if s1.end_.pos < s2.end_.pos {
          fuseSome(s1, r1, s2, r2)
        } else {
          fuseSome(s2, r2, s1, r1)
        }
      | _ => failwith("something wrong with Range::fuse")
      }
    switch (a, b) {
    | (NoRange, r2) => r2
    | (r1, NoRange) => r1
    | (Range(f, r1), Range(_, r2)) =>
      Range(f, helpFuse(Belt.List.fromArray(r1), Belt.List.fromArray(r2))->Belt.List.toArray)
    }
  }

  open Belt
  let toString = (self: t): string =>
    switch self {
    | NoRange => ""
    | Range(Some(filepath), []) => filepath
    | Range(None, xs) =>
      switch (xs[0], xs[Array.length(xs) - 1]) {
      | (Some(first), Some(last)) => AgdaInterval.toString({start: first.start, end_: last.end_})
      | _ => ""
      }
    | Range(Some(filepath), xs) =>
      switch (xs[0], xs[Array.length(xs) - 1]) {
      | (Some(first), Some(last)) =>
        filepath ++ ":" ++ AgdaInterval.toString({start: first.start, end_: last.end_})
      | _ => ""
      }
    }

  let decode = {
    open JsonCombinators.Json.Decode
    Util.Decode.sum(x => {
      switch x {
      | "Range" =>
        Payload(
          pair(option(string), array(AgdaInterval.decode))->map((. (source, intervals)) => Range(
            source,
            intervals,
          )),
        )
      | "NoRange" => TagOnly(NoRange)
      | tag => raise(DecodeError("[AgdaRange] Unknown constructor: " ++ tag))
      }
    })
  }

  let encode = {
    open JsonCombinators.Json.Encode
    Util.Encode.sum(x =>
      switch x {
      | NoRange => TagOnly("NoRange")
      | Range(source, intervals) =>
        Payload("Range", pair(option(string), array(AgdaInterval.encode))((source, intervals)))
      }
    , ...)
  }
}

// NOTE: This is not related to VSCode or Agda
// NOTE: eliminate this
module Interval = {
  type t = (int, int)

  let contains = (interval, offset) => {
    let (start, end_) = interval
    start <= offset && offset <= end_
  }

  let decode = {
    open JsonCombinators.Json.Decode
    pair(int, int)
  }

  let encode = {
    open JsonCombinators.Json.Encode
    pair(int, int)
  }
}
