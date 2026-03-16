// https://github.com/agda/agda/blob/master/src/full/Agda/Interaction/Highlighting/Precise.hs
// a mix of Aspect, OtherAspect and NameKind
type t =
  | Hole
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
  | Hole => "Hole"
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
  | "hole" => Hole
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
  | _others => Operator
  }

let toTokenTypeAndModifiersAndDecoration: t => (
  (
    option<Highlighting__SemanticToken.TokenType.t>,
    array<Highlighting__SemanticToken.TokenModifier.t>,
  ), // Type of Semantic Token // Modifiers of Semantic Token
  option<Highlighting__Decoration.t>, // background decoration colors of light/dark themes
) = x => {
  // helper constructor
  let typeOnly = (t: Highlighting__SemanticToken.TokenType.t) =>
    ((Some(t), [Highlighting__SemanticToken.TokenModifier.Agda]), None)
  let typeWithModifier = (t: Highlighting__SemanticToken.TokenType.t, m: Highlighting__SemanticToken.TokenModifier.t) =>
    ((Some(t), [m, Agda]), None)
  let nothing = ((None, []), None)
  let backgroundOnly = (light, dark) => (
    (None, []),
    Some({Highlighting__Decoration.light: Background(light), dark: Background(dark)}),
  )

  switch x {
  | Hole => nothing
  // the Aspect part
  | Comment => typeOnly(Comment)
  | Keyword => typeOnly(Keyword)
  | String => typeOnly(String)
  | Number => typeOnly(Number)
  | Symbol => nothing // we choose not to color Symbols for aesthetic reasons
  | PrimitiveType => typeWithModifier(Type, Primitive)
  | Pragma => nothing
  | Background => nothing
  | Markup => nothing
  // the OtherAspect part
  | Error => ((None, [Agda]), None)
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
  | ConstructorCoInductive => typeWithModifier(EnumMember, CoInductive)
  | Datatype => typeOnly(Type)
  | Field => typeOnly(Property)
  | Function => typeOnly(Function)
  | Module => typeOnly(Namespace)
  | Postulate => typeWithModifier(Function, Abstract)
  | Primitive => typeWithModifier(String, Primitive)
  | Record => typeOnly(Struct)
  | Argument => typeOnly(Parameter)
  | Macro => typeOnly(Macro)
  // when the second field of Aspect.Name is True
  | Operator => typeOnly(Operator)
  }
}
