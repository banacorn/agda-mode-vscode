open Belt

// https://github.com/agda/agda/blob/master/src/full/Agda/Interaction/Highlighting/Precise.hs
module Aspect = {
  // a mix of Aspect, OtherAspect and NameKind
  type t =
    | // the Aspect part
    Comment
    | Keyword
    | String
    | Number
    | Symbol
    | PrimitiveType
    | Pragma
    | Background
    | Markup
    | // the OtherAspect part
    Error
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
    | // the NameKind part
    Bound
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
    | // when the second field of Aspect.Name is True
    Operator

  let toString = x =>
    switch x {
    // the Aspect part
    | Comment => "Comment"
    | Keyword => "Keyword"
    | String => "String"
    | Number => "Number"
    | Symbol => "Symbol"
    | PrimitiveType => "PrimitiveType"
    | Pragma => "Pragma"
    | Background => "Background"
    | Markup => "Markup"
    // the OtherAspect part
    | Error => "Error"
    | DottedPattern => "DottedPattern"
    | UnsolvedMeta => "UnsolvedMeta"
    | UnsolvedConstraint => "UnsolvedConstraint"
    | TerminationProblem => "TerminationProblem"
    | PositivityProblem => "PositivityProblem"
    | Deadcode => "Deadcode"
    | CoverageProblem => "CoverageProblem"
    | IncompletePattern => "IncompletePattern"
    | TypeChecks => "TypeChecks"
    | CatchallClause => "CatchallClause"
    | ConfluenceProblem => "ConfluenceProblem"
    // the NameKind part
    | Bound => "Bound"
    | Generalizable => "Generalizable"
    | ConstructorInductive => "ConstructorInductive"
    | ConstructorCoInductive => "ConstructorCoInductive"
    | Datatype => "Datatype"
    | Field => "Field"
    | Function => "Function"
    | Module => "Module"
    | Postulate => "Postulate"
    | Primitive => "Primitive"
    | Record => "Record"
    | Argument => "Argument"
    | Macro => "Macro"
    // when the second field of Aspect.Name is True
    | Operator => "Operator"
    }

  let parse = x =>
    switch x {
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
    | "unsolvedmeta" => UnsolvedMeta
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
    | _ => Operator
    }
}

module Token = Parser.SExpression
open Token

module Info = {
  type filepath = string
  type t = {
    start: int, // agda offset
    end_: int, // agda offset
    aspects: array<Aspect.t>, // a list of names of aspects
    isTokenBased: bool,
    note: option<string>,
    source: option<(filepath, int)>, // The defining module and the position in that module
  }
  let toString = self =>
    "Annotation " ++
    (string_of_int(self.start) ++
    (" " ++
    (string_of_int(self.end_) ++
    (" " ++
    (Util.Pretty.list(List.fromArray(self.aspects)) ++
    switch self.source {
    | None => ""
    | Some((s, i)) => s ++ (" " ++ string_of_int(i))
    })))))
  let parse: Token.t => option<t> = x =>
    switch x {
    | A(_) => None
    | L(xs) =>
      switch xs {
      | [A(start'), A(end_'), aspects, _, _, L([A(filepath), _, A(index')])] =>
        int_of_string_opt(start')->Option.flatMap(start =>
          int_of_string_opt(end_')->Option.flatMap(end_ =>
            int_of_string_opt(index')->Option.map(index => {
              start: start - 1,
              end_: end_ - 1,
              aspects: flatten(aspects)->Array.map(Aspect.parse),
              isTokenBased: false, // NOTE: fix this
              note: None, // NOTE: fix this
              source: Some((filepath, index)),
            })
          )
        )

      | [A(start'), A(end_'), aspects] =>
        int_of_string_opt(start')->Option.flatMap(start =>
          int_of_string_opt(end_')->Option.map(end_ => {
            start: start - 1,
            end_: end_ - 1,
            aspects: flatten(aspects)->Array.map(Aspect.parse),
            isTokenBased: false, // NOTE: fix this
            note: None, // NOTE: fix this
            source: None,
          })
        )
      | [A(start'), A(end_'), aspects, _] =>
        int_of_string_opt(start')->Option.flatMap(start =>
          int_of_string_opt(end_')->Option.map(end_ => {
            start: start - 1,
            end_: end_ - 1,
            aspects: flatten(aspects)->Array.map(Aspect.parse),
            isTokenBased: false, // NOTE: fix this
            note: None, // NOTE: fix this
            source: None,
          })
        )
      | _ => None
      }
    }
  let parseDirectHighlightings: array<Token.t> => array<t> = tokens =>
    tokens->Js.Array.sliceFrom(2, _)->Array.map(parse)->Array.keepMap(x => x)

  let decode: Json.Decode.decoder<t> = {
    open Json.Decode
    Util.Decode.tuple6(
      int,
      int,
      array(string),
      bool,
      optional(string),
      optional(pair(string, int)),
    ) |> map(((start, end_, aspects, isTokenBased, note, source)) => {
      start: start - 1,
      end_: end_ - 1,
      aspects: aspects->Array.map(Aspect.parse),
      isTokenBased: isTokenBased,
      note: note,
      source: source,
    })
  }
}

module Infos = {
  module Format = {
    type t =
      | Emacs(string)
      | JSON(string)
    let toFilepath = format =>
      switch format {
      | Emacs(filepath) => filepath
      | JSON(filepath) => filepath
      }
  }

  // request from Agda to add new highlighting infos
  module AddNewInfos = {
    // removeTokenBasedHighlighting:
    //    Should token-based highlighting be removed in conjunction with
    //    the application of new highlighting (in order to reduce the risk of flicker)?
    type t = AddNewInfos(bool, array<Info.t>) // removeTokenBasedHighlighting, highlighting infos

    let decode: Json.Decode.decoder<t> = {
      open Json.Decode
      pair(bool, array(Info.decode)) |> map(((keepHighlighting, xs)) => AddNewInfos(
        keepHighlighting,
        xs,
      ))
    }

    let parseEmacs = (buffer): t => {
      open! Parser.SExpression
      let tokens = switch Parser.SExpression.parse(Node.Buffer.toString(buffer))[0] {
      | Some(Ok(L(xs))) => xs
      | _ => []
      }
      // RemoveTokenBasedHighlighting
      let removeTokenBasedHighlighting = switch tokens[0] {
      | Some(A("remove")) => true
      | _ => false
      }
      let infos = Js.Array.sliceFrom(1, tokens)->Array.keepMap(Info.parse)
      AddNewInfos(removeTokenBasedHighlighting, infos)
    }

    let parseJSON = (buffer): t => {
      let raw = Node.Buffer.toString(buffer)
      switch Js.Json.parseExn(raw) {
      | exception _e => AddNewInfos(false, [])
      | json => decode(json)
      }
    }

    let readAndParse = format => {
      N.Util.promisify(N.Fs.readFile)(. Format.toFilepath(format))
      ->Promise.Js.fromBsPromise
      ->Promise.Js.toResult
      ->Promise.map(x =>
        switch x {
        | Ok(buffer) =>
          switch format {
          | Emacs(_) => parseEmacs(buffer)
          | JSON(_) => parseJSON(buffer)
          }
        | Error(_err) => AddNewInfos(false, [])
        }
      )
    }

    let toInfos = x =>
      switch x {
      | AddNewInfos(_removeTokenBasedHighlighting, xs) => xs
      }
  }

  type t = {
    // from addEmacsFilePath/addJSONFilePath
    mutable tempFilePaths: array<Format.t>,
    // Infos indexed by Range
    mutable infos: IntervalTree.t<Info.t>,
  }

  let make = () => {
    tempFilePaths: [],
    infos: IntervalTree.make(),
  }

  let addEmacsFilePath = (self, filepath) =>
    Js.Array.push(Format.Emacs(filepath), self.tempFilePaths)->ignore
  let addJSONFilePath = (self, filepath) =>
    Js.Array.push(Format.JSON(filepath), self.tempFilePaths)->ignore

  // insert a bunch of Infos
  // merge Aspects with the existing Info that occupies the same Range
  let insert = (self, infos: array<Info.t>) => {
    infos->Array.forEach(info => {
      let existing = self.infos->IntervalTree.search((info.start, info.end_ - 1))
      switch existing[0] {
      | None => self.infos->IntervalTree.insert((info.start, info.end_ - 1), info)->ignore
      | Some(old) =>
        // merge Aspects
        self.infos->IntervalTree.remove((info.start, info.end_ - 1))->ignore
        let new = {
          ...old,
          aspects: Array.concat(old.aspects, info.aspects),
        }
        self.infos->IntervalTree.insert((info.start, info.end_ - 1), new)->ignore
      }
    })
  }

  let readTempFiles = self =>
    self.tempFilePaths
    ->Array.map(AddNewInfos.readAndParse)
    ->Promise.allArray
    ->Promise.map(xs => xs->Array.map(AddNewInfos.toInfos)->Array.concatMany)
    ->Promise.map(infos => {
      insert(self, infos)
      self.tempFilePaths = []
    })

  let destroy = self => {
    self.tempFilePaths->Array.forEach(format => {
      N.Fs.unlink(Format.toFilepath(format), _ => ())
    })
    self.infos = IntervalTree.make()
  }

}
