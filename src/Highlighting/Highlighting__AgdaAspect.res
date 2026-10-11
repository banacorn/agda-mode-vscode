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
  | ErrorWarning
  | DottedPattern
  | UnsolvedMeta
  | UnsolvedConstraint
  | TerminationProblem
  | PositivityProblem
  | Deadcode
  | ShadowingInTelescope
  | CoverageProblem
  | IncompletePattern
  | TypeChecks
  | MissingDefinition
  | InstanceProblem
  | CosmeticProblem
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
  | ErrorWarning => "ErrorWarning"
  | DottedPattern => "DottedPattern"
  | UnsolvedMeta => "UnsolvedMeta"
  | UnsolvedConstraint => "UnsolvedConstraint"
  | TerminationProblem => "TerminationProblem"
  | PositivityProblem => "PositivityProblem"
  | Deadcode => "Deadcode"
  | ShadowingInTelescope => "ShadowingInTelescope"
  | CoverageProblem => "CoverageProblem"
  | IncompletePattern => "IncompletePattern"
  | TypeChecks => "TypeChecks"
  | MissingDefinition => "MissingDefinition"
  | InstanceProblem => "InstanceProblem"
  | CosmeticProblem => "CosmeticProblem"
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
  | "errorwarning" => ErrorWarning
  | "dottedpattern" => DottedPattern
  | "unsolvedmeta" => UnsolvedMeta
  | "unsolvedconstraint" => UnsolvedConstraint
  | "terminationproblem" => TerminationProblem
  | "positivityproblem" => PositivityProblem
  | "deadcode" => Deadcode
  | "shadowingintelescope" => ShadowingInTelescope
  | "coverageproblem" => CoverageProblem
  | "incompletepattern" => IncompletePattern
  | "typechecks" => TypeChecks
  | "missingdefinition" => MissingDefinition
  | "instanceproblem" => InstanceProblem
  | "cosmeticproblem" => CosmeticProblem
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
  option<Highlighting__Decoration.t>, // decoration colored by a theme color id
) = x => {
  // helper constructor
  let typeOnly = (t: Highlighting__SemanticToken.TokenType.t) =>
    ((Some(t), [Highlighting__SemanticToken.TokenModifier.Agda]), None)
  let typeWithModifier = (t: Highlighting__SemanticToken.TokenType.t, m: Highlighting__SemanticToken.TokenModifier.t) =>
    ((Some(t), [m, Agda]), None)
  let nothing = ((None, []), None)
  // the ids are theme colors contributed in package.json ("contributes.colors")
  let underlinedForeground = id => (
    (None, [Highlighting__SemanticToken.TokenModifier.Agda]),
    Some(Highlighting__Decoration.ForegroundUnderlined(id)),
  )
  let underlinedBackground = id => ((None, []), Some(Highlighting__Decoration.BackgroundUnderlined(id)))
  let backgroundOnly = id => ((None, []), Some(Highlighting__Decoration.Background(id)))

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
  | Error => underlinedForeground("agdaMode.error")
  | ErrorWarning => underlinedBackground("agdaMode.errorWarning")
  | DottedPattern => nothing
  | UnsolvedMeta => backgroundOnly("agdaMode.unsolvedMeta")
  | UnsolvedConstraint => backgroundOnly("agdaMode.unsolvedConstraint")
  | TerminationProblem => backgroundOnly("agdaMode.terminationProblem")
  | PositivityProblem => backgroundOnly("agdaMode.positivityProblem")
  | Deadcode => backgroundOnly("agdaMode.deadcode")
  | ShadowingInTelescope => backgroundOnly("agdaMode.shadowingInTelescope")
  | CoverageProblem => backgroundOnly("agdaMode.coverageProblem")
  | IncompletePattern => backgroundOnly("agdaMode.incompletePattern")
  | TypeChecks => nothing
  | MissingDefinition => backgroundOnly("agdaMode.missingDefinition")
  | InstanceProblem => backgroundOnly("agdaMode.instanceProblem")
  | CosmeticProblem => backgroundOnly("agdaMode.cosmeticProblem")
  | CatchallClause => backgroundOnly("agdaMode.catchallClause")
  | ConfluenceProblem => backgroundOnly("agdaMode.confluenceProblem")
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
  | Primitive => typeWithModifier(Type, Primitive)
  | Record => typeOnly(Class)
  | Argument => typeOnly(Parameter)
  | Macro => typeOnly(Macro)
  // when the second field of Aspect.Name is True
  | Operator => ((Some(Operator), []), None)
  }
}
