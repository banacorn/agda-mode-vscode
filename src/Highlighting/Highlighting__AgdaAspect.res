// https://github.com/agda/agda/blob/master/src/full/Agda/Interaction/Highlighting/Precise.hs
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

let toDecoration = (x: t): option<Highlighting__Decoration.t> =>
  switch x {
  | Comment =>
    Some({
      light: Foreground("#B0B0B0"),
      dark: Foreground("#505050"),
    })
  | Keyword =>
    Some({
      light: Foreground("#CD6600"),
      dark: Foreground("#FF9932"),
    })
  | String =>
    Some({
      light: Foreground("#B22222"),
      dark: Foreground("#DD4D4D"),
    })
  | Number =>
    Some({
      light: Foreground("#800080"),
      dark: Foreground("#9010E0"),
    })
  | Symbol =>
    Some({
      light: Foreground("#404040"),
      dark: Foreground("#BFBFBF"),
    })
  | PrimitiveType =>
    Some({
      light: Foreground("#0000CD"),
      dark: Foreground("#8080FF"),
    })
  | Pragma => None
  | Background => None
  | Markup => None
  | Error =>
    Some({
      light: Foreground("#FF0000"),
      dark: Foreground("#FF0000"),
    })
  | DottedPattern => None
  | UnsolvedMeta =>
    Some({
      light: Background("#FFFF00"),
      dark: Background("#806B00"),
    })
  | UnsolvedConstraint =>
    Some({
      light: Background("#FFFF00"),
      dark: Background("#806B00"),
    })
  | TerminationProblem =>
    Some({
      light: Background("#FFA07A"),
      dark: Background("#802400"),
    })
  | PositivityProblem =>
    Some({
      light: Background("#CD853F"),
      dark: Background("#803F00"),
    })
  | Deadcode =>
    Some({
      light: Background("#A9A9A9"),
      dark: Background("#808080"),
    })
  | CoverageProblem =>
    Some({
      light: Background("#F5DEB3"),
      dark: Background("#805300"),
    })
  | IncompletePattern =>
    Some({
      light: Background("#800080"),
      dark: Background("#800080"),
    })
  | TypeChecks => None
  | CatchallClause =>
    Some({
      light: Background("#F5F5F5"),
      dark: Background("#404040"),
    })
  | ConfluenceProblem =>
    Some({
      light: Background("#FFC0CB"),
      dark: Background("#800080"),
    })
  | Bound => None
  | Generalizable => None
  | ConstructorInductive =>
    Some({
      light: Foreground("#008B00"),
      dark: Foreground("#29CC29"),
    })
  | ConstructorCoInductive =>
    Some({
      light: Foreground("#996600"),
      dark: Foreground("#FFEA75"),
    })
  | Datatype =>
    Some({
      light: Foreground("#0000CD"),
      dark: Foreground("#8080FF"),
    })
  | Field =>
    Some({
      light: Foreground("#EE1289"),
      dark: Foreground("#F570B7"),
    })
  | Function =>
    Some({
      light: Foreground("#0000CD"),
      dark: Foreground("#8080FF"),
    })
  | Module =>
    Some({
      light: Foreground("#800080"),
      dark: Foreground("#CD80FF"),
    })
  | Postulate =>
    Some({
      light: Foreground("#0000CD"),
      dark: Foreground("#8080FF"),
    })
  | Primitive =>
    Some({
      light: Foreground("#0000CD"),
      dark: Foreground("#8080FF"),
    })
  | Record =>
    Some({
      light: Foreground("#0000CD"),
      dark: Foreground("#8080FF"),
    })
  | Argument => None
  | Macro =>
    Some({
      light: Foreground("#458B74"),
      dark: Foreground("#73BAA2"),
    })
  | Operator => None
  }

let toTokenTypeAndModifiersAndDecoration: t => (
  (
    option<Highlighting__SemanticToken.TokenType.t>,
    array<Highlighting__SemanticToken.TokenModifier.t>,
  ), // Type of Semantic Token // Modifiers of Semantic Token
  option<Highlighting__Decoration.t>, // background decoration colors of light/dark themes
) = x => {
  // helper constructor
  let typeOnly = (t: Highlighting__SemanticToken.TokenType.t) => ((Some(t), []), None)
  let nothing = ((None, []), None)
  let backgroundOnly = (light, dark) => (
    (None, []),
    Some({Highlighting__Decoration.light: Background(light), dark: Background(dark)}),
  )

  switch x {
  // the Aspect part
  | Comment => typeOnly(Comment)
  | Keyword => typeOnly(Keyword)
  | String => typeOnly(String)
  | Number => typeOnly(Number)
  | Symbol => nothing // we choose not to color Symbols for aesthetic reasons
  | PrimitiveType => typeOnly(Type)
  | Pragma => nothing
  | Background => nothing
  | Markup => nothing
  // the OtherAspect part
  | Error => ((None, [Deprecated]), None)
  | DottedPattern => nothing
  | UnsolvedMeta => backgroundOnly("#FFFF00", "#806B00")
  | UnsolvedConstraint => backgroundOnly("#FFA07A", "#802400")
  | TerminationProblem => backgroundOnly("#FFA07A", "#802400")
  | PositivityProblem => backgroundOnly("#CD853F", "#803F00")
  | Deadcode => backgroundOnly("#A9A9A9", "#808080")
  | CoverageProblem => backgroundOnly("#F5DEB3", "#805300")
  | IncompletePattern => backgroundOnly("#800080", "#800080")
  | TypeChecks => nothing
  | CatchallClause => backgroundOnly("#F5F5F5", "#404040")
  | ConfluenceProblem => backgroundOnly("#FFC0CB", "#800080")
  // the NameKind part
  | Bound => typeOnly(Variable)
  | Generalizable => typeOnly(Variable)
  | ConstructorInductive => typeOnly(EnumMember)
  | ConstructorCoInductive => typeOnly(EnumMember)
  | Datatype => typeOnly(Type)
  | Field => typeOnly(Member)
  | Function => typeOnly(Function)
  | Module => typeOnly(Namespace)
  | Postulate => typeOnly(Function)
  | Primitive => typeOnly(String)
  | Record => typeOnly(Struct)
  | Argument => typeOnly(Parameter)
  | Macro => typeOnly(Macro)
  // when the second field of Aspect.Name is True
  | Operator => typeOnly(Operator)
  }
}
