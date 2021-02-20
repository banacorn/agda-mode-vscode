open Belt

type filepath = string
type index = int

module Token = Parser.SExpression

module GiveAction = {
  type t =
    | GiveParen
    | GiveNoParen
    | GiveString(string)

  open Json.Decode
  open Util.Decode
  let decode: decoder<t> = sum(x =>
    switch x {
    | "GiveString" => Contents(string |> map(s => GiveString(s)))
    | "GiveParen" => TagOnly(GiveParen)
    | "GiveNoParen" => TagOnly(GiveNoParen)
    | tag => raise(DecodeError("[Response.GiveAction] Unknown constructor: " ++ tag))
    }
  )
}

type makeCaseType =
  | Function
  | ExtendedLambda

module DisplayInfo = {
  type t =
    | CompilationOk(string)
    | Constraints(option<string>)
    | AllGoalsWarnings(string, string)
    | Time(string)
    | Error(string)
    | Intro(string)
    | Auto(string)
    | ModuleContents(string)
    | SearchAbout(string)
    | WhyInScope(string)
    | NormalForm(string)
    | GoalType(string)
    | CurrentGoal(string)
    | InferredType(string)
    | Context(string)
    | HelperFunction(string)
    | Version(string)
  let toString = x =>
    switch x {
    | CompilationOk(string) => "CompilationOk " ++ string
    | Constraints(None) => "Constraints"
    | Constraints(Some(string)) => "Constraints " ++ string
    | AllGoalsWarnings(title, _body) => "AllGoalsWarnings " ++ title
    | Time(string) => "Time " ++ string
    | Error(string) => "Error " ++ string
    | Intro(string) => "Intro " ++ string
    | Auto(string) => "Auto " ++ string
    | ModuleContents(string) => "ModuleContents " ++ string
    | SearchAbout(string) => "SearchAbout " ++ string
    | WhyInScope(string) => "WhyInScope " ++ string
    | NormalForm(string) => "NormalForm " ++ string
    | GoalType(string) => "GoalType " ++ string
    | CurrentGoal(string) => "CurrentGoal " ++ string
    | InferredType(string) => "InferredType " ++ string
    | Context(string) => "Context " ++ string
    | HelperFunction(string) => "HelperFunction " ++ string
    | Version(string) => "Version " ++ string
    }

  let parse = (xs: array<Token.t>): option<t> =>
    switch xs[1] {
    | Some(A(rawPayload)) =>
      let payload = rawPayload |> Js.String.replaceByRe(%re("/\\\\r\\\\n|\\\\n/g"), "\n")
      switch xs[0] {
      | Some(A("*Compilation result*")) => Some(CompilationOk(payload))
      | Some(A("*Constraints*")) =>
        switch payload {
        | "nil" => Some(Constraints(None))
        | _ => Some(Constraints(Some(payload)))
        }
      | Some(A("*Helper function*")) => Some(HelperFunction(payload))
      | Some(A("*Error*")) => Some(Error(payload))
      | Some(A("*Auto*")) => Some(Auto(payload))
      | Some(A("*Time*")) => Some(Time(payload))
      | Some(A("*Normal Form*")) => Some(NormalForm(payload))
      | Some(A("*Search About*")) => Some(SearchAbout(payload))
      | Some(A("*Inferred Type*")) => Some(InferredType(payload))
      | Some(A("*Current Goal*")) => Some(CurrentGoal(payload))
      | Some(A("*Goal type etc.*")) => Some(GoalType(payload))
      | Some(A("*Module contents*")) => Some(ModuleContents(payload))
      | Some(A("*Scope Info*")) => Some(WhyInScope(payload))
      | Some(A("*Context*")) => Some(Context(payload))
      | Some(A("*Intro*")) => Some(Intro(payload))
      | Some(A("*Agda Version*")) => Some(Version(payload))
      | Some(A(title)) => Some(AllGoalsWarnings(title, payload))
      | _ => None
      }
    | _ => None
    }
  // NOTE: TEMP
  let parseFromString = (raw: string): option<t> => {
    let tokens =
      raw
      ->Parser.SExpression.parse
      ->Array.keepMap(result =>
        switch result {
        | Error(_) => None
        | Ok(s) => Some(s)
        }
      )
    switch tokens[0] {
    | None => None
    | Some(Parser.SExpression.L(xs)) => parse(Js.Array.sliceFrom(1, xs))
    | Some(A(_)) => None
    }
  }
}

// Here's the corresponding datatype in Haskell:
// https://github.com/agda/agda/blob/master/src/full/Agda/Interaction/Response.hs
// Note that, we are not trying to replicate that datatype,
// just the portion conveyed by the Emacs Lisp protocol

type t =
  // agda2-highlight-add-annotations
  | HighlightingInfoDirect(bool, array<Highlighting.t>)
  // agda2-highlight-load-and-delete-action
  | HighlightingInfoIndirect(filepath)
  // agda2-status-action
  | Status(
      // Has the module been successfully type checked?
      bool,
      // Are implicit arguments displayed?
      bool,
    )
  // agda2-maybe-goto
  | JumpToError(filepath, int)
  // agda2-goals-action
  | InteractionPoints(array<index>)
  // agda2-give-action
  | GiveAction(index, GiveAction.t)
  // agda2-make-case-action
  // agda2-make-case-action-extendlam
  | MakeCase(makeCaseType, array<string>)
  // agda2-solveAll-action
  | SolveAll(array<(index, string)>)
  // agda2-info-action
  // agda2-info-action-and-copy
  | DisplayInfo(DisplayInfo.t)
  | ClearRunningInfo
  // agda2-verbose
  | RunningInfo(int, string)
  | // agda2-highlight-clear
  ClearHighlighting
  | // agda2-abort-done
  DoneAborting
  | // agda2-exit-done
  DoneExiting
  | // CompleteHighlightingAndMakePromptReappear is not sent by Agda, but inserted by ourselves to indicate that
  // all "non-last commands" have been handled, and the highlighting should be completed and prompt should reappear
  // so that we can proceed to handle "last commands"
  CompleteHighlightingAndMakePromptReappear

let toString = x =>
  switch x {
  | HighlightingInfoDirect(keepHighlighting, annotations) =>
    "HighlightingInfoDirect " ++
    ((keepHighlighting ? "Keep " : "Remove ") ++
    (" " ++ (annotations->Array.length->string_of_int ++ " annotations")))
  | HighlightingInfoIndirect(filepath) => "HighlightingInfoIndirect " ++ filepath
  | Status(displayed, checked) =>
    "Status: implicit arguments " ++
    ((displayed ? "displayed, " : "not displayed, ") ++
    ("module " ++ (checked ? "type checked" : "not type checked")))
  | JumpToError(filepath, n) => "JumpToError " ++ (filepath ++ (" " ++ string_of_int(n)))
  | InteractionPoints(points) =>
    "InteractionPoints " ++ points->Array.map(string_of_int)->Util.Pretty.array
  | GiveAction(index, GiveParen) => "GiveAction " ++ (string_of_int(index) ++ " Paren")
  | GiveAction(index, GiveNoParen) => "GiveAction " ++ (string_of_int(index) ++ " NoParen")
  | GiveAction(index, GiveString(string)) =>
    "GiveAction " ++ (string_of_int(index) ++ (" String " ++ string))
  | MakeCase(Function, payload) => "MakeCase Function " ++ Util.Pretty.array(payload)
  | MakeCase(ExtendedLambda, payload) => "MakeCase ExtendedLambda " ++ Util.Pretty.array(payload)
  | SolveAll(solutions) =>
    "SolveAll " ++
    solutions->Array.map(((i, s)) => string_of_int(i) ++ (" " ++ s))->Util.Pretty.array
  | DisplayInfo(info) => "DisplayInfo " ++ DisplayInfo.toString(info)
  | ClearRunningInfo => "ClearRunningInfo"
  | RunningInfo(int, string) => "RunningInfo " ++ (string_of_int(int) ++ (" " ++ string))
  | ClearHighlighting => "ClearHighlighting"
  | DoneAborting => "DoneAborting"
  | DoneExiting => "DoneExiting"
  | CompleteHighlightingAndMakePromptReappear => "CompleteHighlightingAndMakePromptReappear"
  }

let parse = (tokens: Token.t): result<t, Parser.Error.t> => {
  let err = n => Error(Parser.Error.Response(n, tokens))
  switch tokens {
  | A(_) => err(0)
  | L(xs) =>
    switch xs[0] {
    | Some(A("agda2-highlight-add-annotations")) =>
      let annotations = Highlighting.parseDirectHighlightings(xs)
      switch xs[1] {
      | Some(A("remove")) => Ok(HighlightingInfoDirect(false, annotations))
      | Some(A("nil")) => Ok(HighlightingInfoDirect(true, annotations))
      | _ => Ok(HighlightingInfoDirect(true, []))
      }
    | Some(A("agda2-highlight-load-and-delete-action")) =>
      switch xs[1] {
      | Some(A(filepath)) => Ok(HighlightingInfoIndirect(filepath))
      | _ => err(2)
      }
    | Some(A("agda2-status-action")) =>
      switch xs[1] {
      | Some(A(status)) =>
        let pulp = status |> Js.String.split(",")
        Ok(Status(pulp |> Js.Array.includes("ShowImplicit"), pulp |> Js.Array.includes("Checked")))
      | _ => Ok(Status(false, false))
      }
    | Some(A("agda2-maybe-goto")) =>
      switch xs[1] {
      | Some(L([A(filepath), _, A(index')])) =>
        int_of_string_opt(index')
        ->Option.flatMap(index => Some(JumpToError(filepath, index)))
        ->Option.mapWithDefault(err(3), x => Ok(x))
      | _ => err(4)
      }
    | Some(A("agda2-goals-action")) =>
      switch xs[1] {
      | Some(xs) => Ok(InteractionPoints(xs->Token.flatten->Array.keepMap(int_of_string_opt)))
      | _ => err(5)
      }
    | Some(A("agda2-give-action")) =>
      switch xs[1] {
      | Some(A(index')) =>
        int_of_string_opt(index')
        ->Option.flatMap(i =>
          switch xs[2] {
          | Some(A("paren")) => Some(GiveAction(i, GiveParen))
          | Some(A("no-paren")) => Some(GiveAction(i, GiveNoParen))
          | Some(A(result)) => Some(GiveAction(i, GiveString(result)))
          | Some(L(_)) => None
          | _ => None
          }
        )
        ->Option.mapWithDefault(err(6), x => Ok(x))
      | _ => err(7)
      }
    | Some(A("agda2-make-case-action")) =>
      switch xs[1] {
      | Some(xs) => Ok(MakeCase(Function, Token.flatten(xs)))
      | _ => err(8)
      }
    | Some(A("agda2-make-case-action-extendlam")) =>
      switch xs[1] {
      | Some(xs) => Ok(MakeCase(ExtendedLambda, Token.flatten(xs)))
      | _ => err(9)
      }
    | Some(A("agda2-solveAll-action")) =>
      switch xs[1] {
      | Some(xs) =>
        let tokens = Token.flatten(xs)

        let isEven = i => Int32.rem(Int32.of_int(i), Int32.of_int(2)) == Int32.of_int(0)

        let i = ref(0)
        let solutions = tokens->Array.keepMap(token => {
          let solution = if isEven(i.contents) {
            int_of_string_opt(token)->Option.flatMap(index =>
              tokens[i.contents + 1]->Option.map(s => (index, s))
            )
          } else {
            None
          }
          i := i.contents + 1
          solution
        })
        Ok(SolveAll(solutions))
      | _ => err(10)
      }
    | Some(A("agda2-info-action"))
    | Some(A("agda2-info-action-and-copy")) =>
      switch xs[1] {
      | Some(A("*Type-checking*")) =>
        switch xs[3] {
        | Some(A("t")) =>
          switch xs[2] {
          | Some(A(message)) => Ok(RunningInfo(1, message))
          | _ => err(11)
          }
        | _ => Ok(ClearRunningInfo)
        }
      | _ =>
        switch DisplayInfo.parse(xs |> Js.Array.sliceFrom(1)) {
        | Some(info) => Ok(DisplayInfo(info))
        | None => err(12)
        }
      }
    | Some(A("agda2-verbose")) =>
      switch xs[1] {
      | Some(A(message)) => Ok(RunningInfo(2, message))
      | _ => err(13)
      }
    // NOTE: now there are 2 kinds of "agda2-highlight-clear", "TokenBased" and "NotOnlyTokenBased"
    // We should do something about this
    | Some(A("agda2-highlight-clear")) => Ok(ClearHighlighting)
    | Some(A("agda2-abort-done")) => Ok(DoneAborting)
    | Some(A("agda2-exit-done")) => Ok(DoneExiting)
    | _ => err(14)
    }
  }
}

module Prioritized = {
  type response = t
  type t =
    | NonLast(response)
    | Last(int, response)

  let toString = x =>
    switch x {
    | NonLast(response) => "NonLast " ++ toString(response)
    | Last(n, response) => "Last(" ++ (string_of_int(n) ++ (") " ++ toString(response)))
    }

  let parse = (tokens: Token.t): result<t, Parser.Error.t> =>
    switch //        the following text from `agda-mode.el` explains what are those
    //        "last . n" prefixes for:
    //            Every command is run by this function, unless it has the form
    //            "(('last . priority) . cmd)", in which case it is run by
    //            `agda2-run-last-commands' at the end, after the Agda2 prompt
    //            has reappeared, after all non-last commands, and after all
    //            interactive highlighting is complete. The last commands can have
    //            different integer priorities; those with the lowest priority are
    //            executed first.
    // Read the priorities of expressions like this:
    //  [["last", ".", "1"], ".", ["agda2-goals-action", []]]
    // Expressions without the prefix have priority `0` (gets executed first)
    tokens {
    | L([L([A("last"), A("."), A(priority)]), A("."), xs]) =>
      parse(xs)->Result.map(response => Last(int_of_string(priority), response))
    | _ => parse(tokens)->Result.map(response => NonLast(response))
    }
}
