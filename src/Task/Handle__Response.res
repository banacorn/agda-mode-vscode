// from Agda Response to Tasks
open Belt
open VSCode
open! Task
open Response
module DisplayInfo = {
  let handle = x =>
    switch x {
    | Response.DisplayInfo.CompilationOk => list{display(Success("Compilation Done!"), Nothing)}
    | Constraints(None) => list{display(Plain("No Constraints"), Nothing)}
    | Constraints(Some(body)) => list{displayEmacs(Outputs, Plain("Constraints"), body)}
    | AllGoalsWarnings(header, "nil") => list{display(Success(header), Nothing)}
    | AllGoalsWarnings(header, body) => list{displayEmacs(AllGoalsWarnings, Plain(header), body)}
    | Time(body) => list{displayEmacs(Text, Plain("Time"), body)}
    | Error(body) => list{displayEmacs(Error, Error("Error!"), body)}
    | Intro(body) => list{displayEmacs(Text, Plain("Intro"), body)}
    | Auto(body) => list{displayEmacs(Text, Plain("Auto"), body)}
    | ModuleContents(body) => list{displayEmacs(Text, Plain("Module Contents"), body)}
    | SearchAbout(body) => list{displayEmacs(SearchAbout, Plain("Search About"), body)}
    | WhyInScope(body) => list{displayEmacs(Text, Plain("Scope info"), body)}
    | NormalForm(body) => list{displayEmacs(Text, Plain("Normal form"), body)}
    | GoalType(body) => list{displayEmacs(GoalType, Plain("Goal and Context"), body)}
    | CurrentGoal(payload) => list{display(Plain("Current goal"), Plain(payload))}
    | InferredType(payload) => list{display(Plain("Inferred type"), Plain(payload))}
    | Context(body) => list{displayEmacs(Outputs, Plain("Context"), body)}
    | HelperFunction(payload) => list{
        WithStateP(_ => Env.clipboard->Clipboard.writeText(payload)->Promise.map(() => list{})),
        display(Plain("Helper function (copied to clipboard)"), Plain(payload)),
      }
    | Version(payload) => list{display(Plain("Version"), Plain(payload))}
    }
}

let handle = response =>
  switch response {
  | HighlightingInfoDirect(_remove, annotations) => list{Decoration(AddViaPipe(annotations))}
  | HighlightingInfoIndirect(filepath) => list{Decoration(AddViaFile(filepath))}
  | ClearHighlighting => list{Decoration(Clear)}
  | Status(_displayImplicit, _checked) => // display(
    //   "Status",
    //   Some(
    //     "Typechecked: "
    //     ++ string_of_bool(checked)
    //     ++ "\nDisplay implicit arguments: "
    //     ++ string_of_bool(displayImplicit),
    //   ),
    // ),
    list{}
  // if (displayImplicit || checked) {
  //   [
  //     display(
  //       "Status",
  //       Some(
  //         "Typechecked: "
  //         ++ string_of_bool(checked)
  //         ++ "\nDisplay implicit arguments: "
  //         ++ string_of_bool(displayImplicit),
  //       ),
  //     ),
  //   ];
  // } else {
  //   [];
  // }
  | JumpToError(filepath, offset) => list{
      WithStateP(
        state => {
          // only jump to site of error
          // when it's on the same file
          let path = state.editor->TextEditor.document->TextDocument.fileName->Parser.filepath
          if path == filepath {
            Promise.resolved(list{Goal(SetCursor(offset - 1))})
          } else {
            Promise.resolved(list{})
          }
        },
      ),
    }
  | InteractionPoints(indices) => list{Goal(Instantiate(indices))}
  | GiveAction(index, give) => list{
      WithStateP(
        state => {
          let found = state.goals->Array.keep(goal => goal.index == index)
          switch found[0] {
          | None =>
            Promise.resolved(list{
              display(
                Error("Error: Give failed"),
                Plain("Cannot find goal #" ++ string_of_int(index)),
              ),
            })
          | Some(goal) =>
            Promise.resolved(
              switch give {
              | Paren => list{
                  Goal(Modify(goal, content => "(" ++ (content ++ ")"))),
                  Goal(RemoveBoundaryAndDestroy(goal)),
                }
              | NoParen => // do nothing
                list{Goal(RemoveBoundaryAndDestroy(goal))}
              | String(content) => list{
                  Goal(Modify(goal, _ => Js.String.replaceByRe(%re("/\\\\n/g"), "\n", content))),
                  Goal(RemoveBoundaryAndDestroy(goal)),
                }
              },
            )
          }
        },
      ),
    }
  | MakeCase(makeCaseType, lines) => list{
      Goal(
        LocalOrGlobal(
          goal =>
            switch makeCaseType {
            | Function => list{Goal(ReplaceWithLines(goal, lines)), DispatchCommand(Load)}
            | ExtendedLambda => list{Goal(ReplaceWithLambda(goal, lines)), DispatchCommand(Load)}
            },
          list{displayOutOfGoalError},
        ),
      ),
    }
  | SolveAll(solutions) => list{
      WithStateP(
        state => {
          let solveOne = ((index, solution)) => {
            let goals = state.goals->Array.keep(goal => goal.index == index)
            switch goals[0] {
            | None => list{}
            | Some(goal) => list{Goal(Modify(goal, _ => solution)), AgdaRequest(Give(goal))}
            }
          }

          // solve them one by one
          let tasks = solutions->Array.map(solveOne)->List.concatMany
          let size = Array.length(solutions)
          let after = if size == 0 {
            list{display(Error("No solutions found"), Nothing)}
          } else {
            list{display(Success(string_of_int(size) ++ " goals solved"), Nothing)}
          }
          Promise.resolved(List.concat(tasks, after))
        },
      ),
    }
  | DisplayInfo(info) => DisplayInfo.handle(info)
  | RunningInfo(_verbosity, message) => list{display(Plain("Type-checking"), Plain(message))}
  | _ => list{}
  // | others => [Debug(Response.toString(others))];
  }
