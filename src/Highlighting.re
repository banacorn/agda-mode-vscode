open Belt;

// https://github.com/agda/agda/blob/master/src/full/Agda/Interaction/Highlighting/Precise.hs
module Aspect = {
  // a mix of Aspect, OtherAspect and NameKind
  type t =
    // the Aspect part
    | Comment
    | Keyword
    | String
    | Number
    | Symbol
    | PrimitiveType
    | Pragma
    | Background
    | Markup
    // the OtherAspect part
    | Error
    | DottedPattern
    | UnsolvedMeta
    | UnsolvedConstraint
    | TerminationProblem
    | PositivityProblem
    | Deadcode
    | CoverageProblem
    | IncompletePattern
    | TypeChecks
    | CatchallClause
    | ConfluenceProblem
    // the NameKind part
    | Bound
    | Generalizable
    | ConstructorInductive
    | ConstructorCoInductive
    | Datatype
    | Field
    | Function
    | Module
    | Postulate
    | Primitive
    | Record
    | Argument
    | Macro
    // when the second field of Aspect.Name is True
    | Operator;

  let parse =
    fun
    | "comment" => Comment
    | "keyword" => Keyword
    | "string" => String
    | "number" => Number
    | "symbol" => Symbol
    | "primitivetype" => PrimitiveType
    | "pragma" => Pragma
    | "background" => Background
    | "markup" => Markup

    | "error" => Error
    | "dottedpattern" => DottedPattern
    | "unsolpedmeta" => UnsolvedMeta
    | "unsolvedconstraint" => UnsolvedConstraint
    | "terminationproblem" => TerminationProblem
    | "positivityproblem" => PositivityProblem
    | "deadcode" => Deadcode
    | "coverageproblem" => CoverageProblem
    | "incompletepattern" => IncompletePattern
    | "typechecks" => TypeChecks
    | "catchallclause" => CatchallClause
    | "confluenceproblem" => ConfluenceProblem

    | "bound" => Bound
    | "generalizable" => Generalizable
    | "inductiveconstructor" => ConstructorInductive
    | "coinductiveconstructor" => ConstructorCoInductive
    | "datatype" => Datatype
    | "field" => Field
    | "function" => Function
    | "module" => Module
    | "postulate" => Postulate
    | "primitive" => Primitive
    | "record" => Record
    | "argument" => Argument
    | "macro" => Macro

    | "operator" => Operator
    | _ => Operator;

  // the type of annotations that we want to highlight (in the moment)
  let shouldHighlight: t => bool =
    fun
    | UnsolvedMeta
    | UnsolvedConstraint
    | TerminationProblem
    | CoverageProblem => true
    | _ => false;
};

module Token = Parser.SExpression;
open Token;
type filepath = string;
type t = {
  start: int,
  end_: int,
  aspects: array(Aspect.t), // a list of names of aspects
  source: option((filepath, int)) // The defining module and the position in that module
};
let toString = self =>
  "Annotation "
  ++ string_of_int(self.start)
  ++ " "
  ++ string_of_int(self.end_)
  ++ " "
  ++ Util.Pretty.list(List.fromArray(self.aspects))
  ++ (
    switch (self.source) {
    | None => ""
    | Some((s, i)) => s ++ " " ++ string_of_int(i)
    }
  );
let parse: Token.t => option(t) =
  fun
  | A(_) => None
  | L(xs) =>
    switch (xs) {
    | [|
        A(start'),
        A(end_'),
        aspects,
        _,
        _,
        L([|A(filepath), _, A(index')|]),
      |] =>
      Parser.int(start')
      ->Option.flatMap(start =>
          Parser.int(end_')
          ->Option.flatMap(end_ =>
              Parser.int(index')
              ->Option.map(index =>
                  {
                    start,
                    end_,
                    aspects: flatten(aspects)->Array.map(Aspect.parse),
                    source: Some((filepath, index)),
                  }
                )
            )
        )

    | [|A(start'), A(end_'), aspects|] =>
      Parser.int(start')
      ->Option.flatMap(start =>
          Parser.int(end_')
          ->Option.map(end_ =>
              {
                start,
                end_,
                aspects: flatten(aspects)->Array.map(Aspect.parse),
                source: None,
              }
            )
        )
    | [|A(start'), A(end_'), aspects, _|] =>
      Parser.int(start')
      ->Option.flatMap(start =>
          Parser.int(end_')
          ->Option.map(end_ =>
              {
                start,
                end_,
                aspects: flatten(aspects)->Array.map(Aspect.parse),
                source: None,
              }
            )
        )
    | _ => None
    };

let parseDirectHighlightings: array(Token.t) => array(t) =
  tokens => {
    tokens
    ->Js.Array.sliceFrom(2, _)
    ->Array.map(parse)
    ->Array.keepMap(x => x);
  };
let parseIndirectHighlightings: array(Token.t) => array(t) =
  tokens =>
    tokens
    ->Js.Array.sliceFrom(1, _)
    ->Array.map(parse)
    ->Array.keepMap(x => x);