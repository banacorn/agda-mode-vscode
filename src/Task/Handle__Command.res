open Belt
open Command

open! Task
// from Editor Command to Tasks
let handle = command => {
  let header = View.Header.Plain(Command.toString(command))
  switch command {
  | Load => list{
      BenchStart("$$$ Load"),
      display(Plain("Loading ..."), Nothing),
      Task.WithStateP(
        state => {
          let document = VSCode.TextEditor.document(state.editor)
          let options = Some(VSCode.TextDocumentShowOptions.make(~preview=false, ()))
          // save the document before loading
          VSCode.TextDocument.save(document)
          // Issue #26 - don't load the document in preview mode
          ->Promise.flatMap(_ => VSCode.Window.showTextDocumentWithShowOptions(document, options))
          ->Promise.map(_ => list{})
        },
      ),
      AgdaRequest(Load),
      BenchEnd("$$$ Load"),
    }
  | Quit => list{}
  | Restart => list{DispatchCommand(Load)}
  | Refresh => list{Goal(UpdateRange), Decoration(Refresh)}
  | Compile => list{AgdaRequest(Compile)}
  | ToggleDisplayOfImplicitArguments => list{AgdaRequest(ToggleDisplayOfImplicitArguments)}
  | ShowConstraints => list{AgdaRequest(ShowConstraints)}
  | SolveConstraints(normalization) => list{
      Goal(
        LocalOrGlobal(
          goal => list{AgdaRequest(SolveConstraints(normalization, goal))},
          list{AgdaRequest(SolveConstraintsGlobal(normalization))},
        ),
      ),
    }
  | ShowGoals => list{AgdaRequest(ShowGoals)}
  | NextGoal => list{Goal(Next)}
  | PreviousGoal => list{Goal(Previous)}
  | SearchAbout(normalization) =>
    prompt(header, {body: None, placeholder: Some("name:"), value: None}, expr => list{
      AgdaRequest(SearchAbout(normalization, expr)),
    })
  | Give => list{
      Goal(LocalOrGlobal2((goal, _) => list{AgdaRequest(Give(goal))}, goal => prompt(header, {
              body: None,
              placeholder: Some("expression to give:"),
              value: None,
            }, expr => list{
              Goal(Modify(goal, _ => expr)),
              AgdaRequest(Give(goal)),
            }), list{Error(OutOfGoal)})),
    }
  | Refine => list{
      Goal(LocalOrGlobal(goal => list{AgdaRequest(Refine(goal))}, list{Error(OutOfGoal)})),
    }
  | ElaborateAndGive(normalization) =>
    let placeholder = Some("expression to elaborate and give:")
    list{
      Goal(
        LocalOrGlobal2(
          (goal, expr) => list{AgdaRequest(ElaborateAndGive(normalization, expr, goal))},
          goal =>
            prompt(header, {body: None, placeholder: placeholder, value: None}, expr => list{
              AgdaRequest(ElaborateAndGive(normalization, expr, goal)),
            }),
          list{Error(OutOfGoal)},
        ),
      ),
    }
  | Auto => list{
      WithStateP(
        _ =>
          Promise.resolved(list{
            Goal(LocalOrGlobal(goal => list{AgdaRequest(Auto(goal))}, list{Error(OutOfGoal)})),
          }),
      ),
    }
  | Case =>
    let placeholder = Some("variable to case split:")
    list{Goal(LocalOrGlobal2((goal, _) => list{AgdaRequest(Case(goal))}, goal => prompt(header, {
              body: Some("Please specify which variable you wish to split"),
              placeholder: placeholder,
              value: None,
            }, expr => list{
              Goal(Modify(goal, _ => expr)), // place the queried expression in the goal
              AgdaRequest(Case(goal)),
            }), list{Error(OutOfGoal)}))}
  | HelperFunctionType(normalization) =>
    let placeholder = Some("expression:")
    list{
      Goal(
        LocalOrGlobal2(
          (goal, expr) => list{AgdaRequest(HelperFunctionType(normalization, expr, goal))},
          goal =>
            prompt(header, {body: None, placeholder: placeholder, value: None}, expr => list{
              AgdaRequest(HelperFunctionType(normalization, expr, goal)),
            }),
          list{Error(OutOfGoal)},
        ),
      ),
    }
  | InferType(normalization) =>
    let placeholder = Some("expression to infer:")
    list{
      Goal(
        LocalOrGlobal2(
          (goal, expr) => list{AgdaRequest(InferType(normalization, expr, goal))},
          goal =>
            prompt(header, {body: None, placeholder: placeholder, value: None}, expr => list{
              AgdaRequest(InferType(normalization, expr, goal)),
            }),
          prompt(header, {body: None, placeholder: placeholder, value: None}, expr => list{
            AgdaRequest(InferTypeGlobal(normalization, expr)),
          }),
        ),
      ),
    }
  | Context(normalization) => list{
      Goal(
        LocalOrGlobal(
          goal => list{AgdaRequest(Context(normalization, goal))},
          list{Error(OutOfGoal)},
        ),
      ),
    }
  | GoalType(normalization) => list{
      Goal(
        LocalOrGlobal(
          goal => list{AgdaRequest(GoalType(normalization, goal))},
          list{Error(OutOfGoal)},
        ),
      ),
    }
  | GoalTypeAndContext(normalization) => list{
      Goal(
        LocalOrGlobal(
          goal => list{AgdaRequest(GoalTypeAndContext(normalization, goal))},
          list{Error(OutOfGoal)},
        ),
      ),
    }
  | GoalTypeContextAndInferredType(normalization) => list{
      Goal(
        LocalOrGlobal2(
          (goal, expr) => list{
            AgdaRequest(GoalTypeContextAndInferredType(normalization, expr, goal)),
          },
          // fallback to `GoalTypeAndContext` when there's no content
          goal => list{AgdaRequest(GoalTypeAndContext(normalization, goal))},
          list{Error(OutOfGoal)},
        ),
      ),
    }
  | GoalTypeContextAndCheckedType(normalization) =>
    let placeholder = Some("expression to type:")
    list{
      Goal(
        LocalOrGlobal2(
          (goal, expr) => list{
            AgdaRequest(GoalTypeContextAndCheckedType(normalization, expr, goal)),
          },
          goal =>
            prompt(header, {body: None, placeholder: placeholder, value: None}, expr => list{
              AgdaRequest(GoalTypeContextAndCheckedType(normalization, expr, goal)),
            }),
          list{Error(OutOfGoal)},
        ),
      ),
    }
  | ModuleContents(normalization) =>
    let placeholder = Some("module name:")
    list{
      Goal(
        LocalOrGlobal2(
          (goal, expr) => list{AgdaRequest(ModuleContents(normalization, expr, goal))},
          goal =>
            prompt(header, {body: None, placeholder: placeholder, value: None}, expr => list{
              AgdaRequest(ModuleContents(normalization, expr, goal)),
            }),
          prompt(header, {body: None, placeholder: placeholder, value: None}, expr => list{
            AgdaRequest(ModuleContentsGlobal(normalization, expr)),
          }),
        ),
      ),
    }
  | ComputeNormalForm(computeMode) =>
    let placeholder = Some("expression to normalize:")
    list{
      Goal(
        LocalOrGlobal2(
          (goal, expr) => list{AgdaRequest(ComputeNormalForm(computeMode, expr, goal))},
          goal =>
            prompt(header, {body: None, placeholder: placeholder, value: None}, expr => list{
              AgdaRequest(ComputeNormalForm(computeMode, expr, goal)),
            }),
          prompt(header, {body: None, placeholder: placeholder, value: None}, expr => list{
            AgdaRequest(ComputeNormalFormGlobal(computeMode, expr)),
          }),
        ),
      ),
    }
  | WhyInScope =>
    let placeholder = Some("name:")
    list{
      Goal(
        LocalOrGlobal2(
          (goal, expr) => list{AgdaRequest(WhyInScope(expr, goal))},
          goal =>
            prompt(header, {body: None, placeholder: placeholder, value: None}, expr => list{
              AgdaRequest(WhyInScope(expr, goal)),
            }),
          prompt(header, {body: None, placeholder: placeholder, value: None}, expr => list{
            AgdaRequest(WhyInScopeGlobal(expr)),
          }),
        ),
      ),
    }
  | EventFromView(event) =>
    switch event {
    | Initialized => list{}
    | Destroyed => list{Destroy}
    | InputMethod(InsertChar(char)) => list{DispatchCommand(InputMethod(InsertChar(char)))}
    | InputMethod(ChooseSymbol(symbol)) => list{
        WithStateP(state => Handle__InputMethod.chooseSymbol(state, symbol)),
      }
    | PromptChange(Select(interval)) => list{
        WithStateP(state => Handle__InputMethod.select(state, [interval])),
      }
    | PromptChange(Change(input)) => list{
        WithStateP(state => Handle__InputMethod.activatePromptIM(state, input)),
      }
    | PromptChange(BrowseUp) => list{DispatchCommand(InputMethod(BrowseUp))}
    | PromptChange(BrowseDown) => list{DispatchCommand(InputMethod(BrowseDown))}
    | PromptChange(BrowseLeft) => list{DispatchCommand(InputMethod(BrowseLeft))}
    | PromptChange(BrowseRight) => list{DispatchCommand(InputMethod(BrowseRight))}
    | JumpToTarget(link) => list{
        WithState(
          state => {
            let document = VSCode.TextEditor.document(state.editor)
            Editor.focus(document)
            let path = document->VSCode.TextDocument.fileName->Parser.filepath
            switch link {
            | ToRange(NoRange) => ()
            | ToRange(Range(None, _intervals)) => ()
            | ToRange(Range(Some(filePath), intervals)) =>
              // only select the intervals when it's on the same file
              if path == filePath {
                let ranges = intervals->Array.map(View__Controller.fromInterval)
                Editor.Selection.setMany(state.editor, ranges)
              }
            | ToHole(index) =>
              let goal = Js.Array.find((goal: Goal.t) => goal.index == index, state.goals)
              switch goal {
              | None => ()
              | Some(goal) => Goal.setCursor(goal, state.editor)
              }
            }
          },
        ),
      }
    | MouseOver(_) => list{Debug("MouseOver")}
    | MouseOut(_) => list{Debug("MouseOut")}
    }
  | Escape => list{
      WithStateP(
        state => {
          if state.editorIM->IM.isActivated || state.promptIM->IM.isActivated {
            Handle__InputMethod.deactivate(state)
          } else {
            Promise.resolved(list{ViewEvent(PromptInterrupt)})
          }
        },
      ),
    }
  | InputMethod(action) => Handle__InputMethod.handle(action)
  }
}
