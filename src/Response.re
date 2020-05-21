open Belt;

/* https://github.com/agda/agda/blob/master/src/full/Agda/Interaction/Response.hs */

type filepath = string;
type index = int;

module Token = Parser.SExpression;

type giveResult =
  | Paren
  | NoParen
  | String(string);

type makeCaseType =
  | Function
  | ExtendedLambda;

module Info = {
  type t =
    | CompilationOk
    | Constraints(option(string))
    | AllGoalsWarnings
    // | AllGoalsWarnings(Emacs.AllGoalsWarnings.t)
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
    | Version(string);
  let toString =
    fun
    | CompilationOk => "CompilationOk"
    | Constraints(None) => "Constraints"
    | Constraints(Some(string)) => "Constraints " ++ string
    | AllGoalsWarnings => "AllGoalsWarnings "
    // | AllGoalsWarnings(warnings) =>
    //   "AllGoalsWarnings " ++ Emacs.AllGoalsWarnings.toString(warnings)
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
    | Version(string) => "Version " ++ string;

  let parse = (xs: array(Token.t)): option(t) => {
    switch (xs[1]) {
    | Some(A(rawPayload)) =>
      let payload =
        rawPayload |> Js.String.replaceByRe([%re "/\\\\r\\\\n|\\\\n/g"], "\n");
      switch (xs[0]) {
      | Some(A("*Compilation result*")) => Some(CompilationOk)
      | Some(A("*Constraints*")) =>
        switch (payload) {
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
      | Some(A(title)) => Some(AllGoalsWarnings)

      // Some(
      //   AllGoalsWarnings(Emacs__AllGoalsWarnings.parse(title, payload)),
      // )
      | _ => None
      };
    | _ => None
    };
  };
};

type t =
  /* agda2-highlight-add-annotations */
  | HighlightingInfoDirect
  /* agda2-highlight-load-and-delete-action */
  | HighlightingInfoIndirect(filepath)
  /* agda2-status-action */
  | NoStatus
  | Status(
      bool, /*  Are implicit arguments displayed? */
      /* Has the module been successfully type checked? */
      bool,
    )
  /* agda2-maybe-goto */
  | JumpToError(filepath, int)
  /* agda2-goals-action */
  | InteractionPoints(array(index))
  /* agda2-give-action */
  | GiveAction(index, giveResult)
  /* agda2-make-case-action */
  /* agda2-make-case-action-extendlam */
  | MakeCase(makeCaseType, array(string))
  /* agda2-solveAll-action */
  | SolveAll(array((index, string)))
  /* agda2-info-action */
  /* agda2-info-action-and-copy */
  | DisplayInfo(Info.t)
  | ClearRunningInfo
  /* agda2-verbose */
  | RunningInfo(int, string)
  /* agda2-highlight-clear */
  | ClearHighlighting
  /* agda2-abort-done */
  | DoneAborting;

let toString =
  fun
  | HighlightingInfoDirect => "HighlightingInfoDirect "
  | HighlightingInfoIndirect(filepath) =>
    "HighlightingInfoIndirect " ++ filepath
  | NoStatus => "NoStatus"
  | Status(displayed, checked) =>
    "Status: implicit arguments "
    ++ (displayed ? "displayed, " : "not displayed, ")
    ++ "module "
    ++ (checked ? "type checked" : "not type checked")
  | JumpToError(filepath, n) =>
    "JumpToError " ++ filepath ++ " " ++ string_of_int(n)
  | InteractionPoints(points) =>
    "InteractionPoints "
    ++ points->Array.map(string_of_int)->Util.Pretty.array
  | GiveAction(index, Paren) =>
    "GiveAction " ++ string_of_int(index) ++ " Paren"
  | GiveAction(index, NoParen) =>
    "GiveAction " ++ string_of_int(index) ++ " NoParen"
  | GiveAction(index, String(string)) =>
    "GiveAction " ++ string_of_int(index) ++ " String " ++ string
  | MakeCase(Function, payload) =>
    "MakeCase Function " ++ Util.Pretty.array(payload)
  | MakeCase(ExtendedLambda, payload) =>
    "MakeCase ExtendedLambda " ++ Util.Pretty.array(payload)
  | SolveAll(solutions) =>
    "SolveAll "
    ++ solutions
       ->Array.map(((i, s)) => string_of_int(i) ++ " " ++ s)
       ->Util.Pretty.array
  | DisplayInfo(info) => "DisplayInfo " ++ Info.toString(info)
  | ClearRunningInfo => "ClearRunningInfo"
  | RunningInfo(int, string) =>
    "RunningInfo " ++ string_of_int(int) ++ " " ++ string
  | ClearHighlighting => "ClearHighlighting"
  | DoneAborting => "DoneAborting";

// TODO: execute responses with priority
let parseWithPriority =
    (_priority: int, tokens: Token.t): result(t, Parser.Error.t) => {
  let err = n => Error(Parser.Error.Response(n, tokens));
  switch (tokens) {
  | A(_) => err(0)
  | L(xs) =>
    switch (xs[0]) {
    | Some(A("agda2-highlight-add-annotations")) =>
      switch (xs[1]) {
      | Some(A("remove")) => Ok(HighlightingInfoDirect)
      | Some(A("nil")) => Ok(HighlightingInfoDirect)
      | _ => Ok(HighlightingInfoDirect)
      }
    | Some(A("agda2-highlight-load-and-delete-action")) =>
      switch (xs[1]) {
      | Some(A(filepath)) => Ok(HighlightingInfoIndirect(filepath))
      | _ => err(2)
      }
    | Some(A("agda2-status-action")) =>
      switch (xs[1]) {
      | Some(A(status)) =>
        let pulp = status |> Js.String.split(",");
        Ok(
          Status(
            pulp |> Js.Array.includes("ShowImplicit"),
            pulp |> Js.Array.includes("Checked"),
          ),
        );
      | _ => Ok(NoStatus)
      }
    | Some(A("agda2-maybe-goto")) =>
      switch (xs[1]) {
      | Some(L([|A(filepath), _, A(index')|])) =>
        Parser.int(index')
        ->Option.flatMap(index => Some(JumpToError(filepath, index)))
        ->Option.mapWithDefault(err(3), x => Ok(x))
      | _ => err(4)
      }
    | Some(A("agda2-goals-action")) =>
      switch (xs[1]) {
      | Some(xs) =>
        Ok(InteractionPoints(xs->Token.flatten->Array.keepMap(Parser.int)))
      | _ => err(5)
      }
    | Some(A("agda2-give-action")) =>
      switch (xs[1]) {
      | Some(A(index')) =>
        Parser.int(index')
        ->Option.flatMap(i =>
            switch (xs[2]) {
            | Some(A("paren")) => Some(GiveAction(i, Paren))
            | Some(A("no-paren")) => Some(GiveAction(i, NoParen))
            | Some(A(result)) => Some(GiveAction(i, String(result)))
            | Some(L(_)) => None
            | _ => None
            }
          )
        ->Option.mapWithDefault(err(6), x => Ok(x))
      | _ => err(7)
      }
    | Some(A("agda2-make-case-action")) =>
      switch (xs[1]) {
      | Some(xs) => Ok(MakeCase(Function, Token.flatten(xs)))
      | _ => err(8)
      }
    | Some(A("agda2-make-case-action-extendlam")) =>
      switch (xs[1]) {
      | Some(xs) => Ok(MakeCase(ExtendedLambda, Token.flatten(xs)))
      | _ => err(9)
      }
    | Some(A("agda2-solveAll-action")) =>
      switch (xs[1]) {
      | Some(xs) =>
        let tokens = Token.flatten(xs);

        let isEven = i =>
          Int32.rem(Int32.of_int(i), Int32.of_int(2)) == Int32.of_int(0);

        let i = ref(0);
        let solutions =
          tokens->Array.keepMap(token => {
            let solution =
              if (isEven(i^)) {
                Parser.int(token)
                ->Option.flatMap(index =>
                    tokens[i^ + 1]->Option.map(s => (index, s))
                  );
              } else {
                None;
              };
            /* loop index */
            i := i^ + 1;
            /* return the solution */
            solution;
          });
        Ok(SolveAll(solutions));
      | _ => err(10)
      }
    | Some(A("agda2-info-action"))
    | Some(A("agda2-info-action-and-copy")) =>
      switch (xs[1]) {
      | Some(A("*Type-checking*")) =>
        /* Resp_ClearRunningInfo & Resp_RunningInfo may run into this case */
        switch (xs[3]) {
        /* t: append */
        | Some(A("t")) =>
          switch (xs[2]) {
          | Some(A(message)) => Ok(RunningInfo(1, message))
          | _ => err(11)
          }
        | _ => Ok(ClearRunningInfo)
        }
      | _ =>
        switch (Info.parse(xs |> Js.Array.sliceFrom(1))) {
        | Some(info) => Ok(DisplayInfo(info))
        | None => err(12)
        }
      }
    | Some(A("agda2-verbose")) =>
      switch (xs[1]) {
      | Some(A(message)) => Ok(RunningInfo(2, message))
      | _ => err(13)
      }
    | Some(A("agda2-highlight-clear")) => Ok(ClearHighlighting)
    | Some(A("agda2-abort-done")) => Ok(DoneAborting)
    | _ => err(14)
    }
  };
};

let parse = (tokens: Token.t): result(t, Parser.Error.t) => {
  //        the following text from `agda-mode.el` explains what are those
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
  switch (tokens) {
  // with prefix
  | L([|L([|A("last"), A("."), A(priority)|]), A("."), xs|]) =>
    parseWithPriority(int_of_string(priority), xs)
  // without prefix
  | _ => parseWithPriority(0, tokens)
  };
};