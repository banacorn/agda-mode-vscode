open Belt;
module Link = Component__Link;
module Range = Component__Range;
open React;

module Term = {
  type t =
    | Plain(string)
    | QuestionMark(int)
    | Underscore(string);
  let toString =
    fun
    | Plain(string) => string
    | QuestionMark(int) => "?" ++ string_of_int(int)
    | Underscore(string) => "_" ++ string;

  let jump = true;
  let hover = true;

  [@react.component]
  let make = (~term: t) => {
    switch (term) {
    | Plain(s) => <span className="expr"> {string(s)} </span>
    | QuestionMark(i) =>
      <Link
        className=["expr", "question-mark"]
        jump
        hover
        target={View.Link.ToHole(i)}>
        {string("?" ++ string_of_int(i))}
      </Link>
    | Underscore(s) => <span className="expr underscore"> {string(s)} </span>
    };
  };
};

module Expr = {
  type t = array(Term.t);
  let toString = xs =>
    xs->Array.map(Term.toString)->Js.String.concatMany(" ");
  let parse = raw => {
    raw
    ->Js.String.trim
    /*                            1         2                        */
    ->Js.String.splitByRe([%re "/(\\?\\d+)|(\\_\\d+[^\\}\\)\\s]*)/"], _)
    ->Array.mapWithIndex((i, token) =>
        switch (i mod 3) {
        | 1 =>
          token
          ->Option.map(Js.String.sliceToEnd(~from=1))
          ->Option.flatMap(int_of_string_opt)
          ->Option.map(x => Term.QuestionMark(x))
        | 2 => token->Option.map(x => Term.Underscore(x))
        | _ => token->Option.map(x => Term.Plain(x))
        }
      )
    ->Array.keepMap(x => x)
    ->(x => Some(x));
  };
  [@react.component]
  let make = (~expr: t) =>
    <span>
      {expr
       ->Array.mapWithIndex((i, term) =>
           <Term key={string_of_int(i)} term />
         )
       ->React.array}
    </span>;
};

module OutputConstraint = {
  type t =
    | OfType(Expr.t, Expr.t)
    | JustType(Expr.t)
    | JustSort(Expr.t)
    | Others(Expr.t);

  let toString =
    fun
    | OfType(e, t) => Expr.toString(e) ++ " : " ++ Expr.toString(t)
    | JustType(t) => Expr.toString(t)
    | JustSort(t) => Expr.toString(t)
    | Others(t) => Expr.toString(t);

  let parseOfType =
    [%re "/^([^\\:]*) \\: ((?:\\n|.)+)/"]
    ->Emacs__Parser.captures(captured =>
        captured
        ->Emacs__Parser.at(2, Expr.parse)
        ->Option.flatMap(type_ =>
            captured
            ->Emacs__Parser.at(1, Expr.parse)
            ->Option.flatMap(term => Some(OfType(term, type_)))
          )
      );
  let parseJustType =
    [%re "/^Type ((?:\\n|.)+)/"]
    ->Emacs__Parser.captures(captured =>
        captured
        ->Emacs__Parser.at(1, Expr.parse)
        ->Option.map(type_ => JustType(type_))
      );
  let parseJustSort =
    [%re "/^Sort ((?:\\n|.)+)/"]
    ->Emacs__Parser.captures(captured =>
        captured
        ->Emacs__Parser.at(1, Expr.parse)
        ->Option.map(sort => JustSort(sort))
      );
  let parseOthers = raw => raw->Expr.parse->Option.map(raw' => Others(raw'));

  let parse =
    Emacs__Parser.choice([|
      parseOfType,
      parseJustType,
      parseJustSort,
      parseOthers,
    |]);

  [@react.component]
  let make = (~value: t, ~range: option(View.Range.t)) => {
    let range =
      Option.mapWithDefault(range, null, range => <Range range abbr=true />);
    switch (value) {
    | OfType(e, t) =>
      <li className="output">
        <Expr expr=e />
        {string(" : ")}
        <Expr expr=t />
        range
      </li>
    | JustType(e) =>
      <li className="output"> {string("Type ")} <Expr expr=e /> range </li>
    | JustSort(e) =>
      <li className="output"> {string("Sort ")} <Expr expr=e /> range </li>
    | Others(e) => <li className="output"> <Expr expr=e /> range </li>
    };
  };
};

module Labeled = {
  [@react.component]
  let make = (~label: string, ~expr: Expr.t) => {
    <li className="labeled">
      <span className="label"> {string(label)} </span>
      <Expr expr />
    </li>;
  };
};

module Output = {
  type t =
    | Output(OutputConstraint.t, option(View.Range.t));
  let toString =
    fun
    | Output(c, None) => "Output " ++ OutputConstraint.toString(c)
    | Output(c, Some(range)) =>
      "Output "
      ++ OutputConstraint.toString(c)
      ++ " "
      ++ View.Range.toString(range);

  let parseOutputWithoutRange = raw =>
    raw->OutputConstraint.parse->Option.map(x => Output(x, None));
  let parseOutputWithRange =
    [%re "/((?:\\n|.)*\\S+)\\s*\\[ at ([^\\]]+) \\]/"]
    ->Emacs__Parser.captures(captured =>
        captured[1]
        ->Option.flatMap(x => x)
        ->Option.flatMap(OutputConstraint.parse)
        ->Option.map(oc => {
            let r =
              captured[2]
              ->Option.flatMap(x => x)
              ->Option.flatMap(View.Range.parse);
            Output(oc, r);
          })
      );
  let parse = raw => {
    let rangeRe = [%re
      "/\\[ at (\\S+\\:(?:\\d+\\,\\d+\\-\\d+\\,\\d+|\\d+\\,\\d+\\-\\d+)) \\]$/"
    ];
    let hasRange = Js.Re.test_(rangeRe, raw);
    if (hasRange) {
      parseOutputWithRange(raw);
    } else {
      parseOutputWithoutRange(raw);
    };
  };

  [@react.component]
  let make = (~value: t) => {
    let Output(oc, range) = value;
    <OutputConstraint value=oc range />;
  };
};

module PlainText = {
  type t =
    | Text(string)
    | Range(View.Range.t);

  let toString =
    fun
    | Text(s) => s
    | Range(r) => View.Range.toString(r);
  let parse = raw =>
    raw
    ->Js.String.splitByRe(
        [%re
          "/([^\\(\\)\\s]+\\:(?:\\d+\\,\\d+\\-\\d+\\,\\d+|\\d+\\,\\d+\\-\\d+))/"
        ],
        _,
      )
    ->Array.keepMap(x => x)
    ->Array.mapWithIndex((i, token) =>
        switch (i mod 2) {
        | 1 =>
          token
          ->View.Range.parse
          ->Option.mapWithDefault(Text(token), x => Range(x))
        | _ => Text(token)
        }
      )
    ->(x => Some(x));

  [@react.component]
  let make = (~value: array(t)) =>
    <span>
      {value
       ->Array.mapWithIndex(i =>
           fun
           | Text(plainText) => string(plainText)
           | Range(range) => <Range key={string_of_int(i)} range />
         )
       ->React.array}
    </span>;
};

module WarningError = {
  type t =
    | WarningMessage(array(PlainText.t))
    | ErrorMessage(array(PlainText.t));
  let toString =
    fun
    | WarningMessage(xs) =>
      xs->Array.map(PlainText.toString)->Util.Pretty.array
    | ErrorMessage(xs) =>
      xs->Array.map(PlainText.toString)->Util.Pretty.array;
  let parse = (isWarning, raw) =>
    raw
    ->PlainText.parse
    ->Option.map(body =>
        isWarning ? WarningMessage(body) : ErrorMessage(body)
      );

  let parseWarning = parse(true);

  let parseError = parse(false);

  [@react.component]
  let make = (~value: t) => {
    switch (value) {
    | WarningMessage(body) =>
      <li className="item-warning-error">
        <span className="item-error-label"> {string("warning")} </span>
        <PlainText value=body />
      </li>
    | ErrorMessage(body) =>
      <li className="item-warning-error">
        <span className="item-error-label"> {string("error")} </span>
        <PlainText value=body />
      </li>
    };
  };
};
