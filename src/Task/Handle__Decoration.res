open! Task
open Belt

// from Decoration to Tasks
let handle = x =>
  switch x {
  | Decoration.AddViaPipe(highlightings) => list{
      WithState(state => state.decorations->Decoration.addDirectly(highlightings)),
    }
  | AddViaFile(filepath) => list{
      WithState(state => state.decorations->Decoration.addIndirectly(filepath)),
    }
  | Clear => list{WithState(state => Decoration.removeAppliedDecorations(state.decorations))}
  | Apply => list{
      BenchStart("$$$ Decoration"),
      WithStateP(state => Decoration.readTempFiles(state.decorations)->Promise.map(() => {
            Decoration.applyHighlightings(state.decorations, state.editor)
            list{}
          })),
      BenchEnd("$$$ Decoration"),
    }
  | ApplyExperimental => list{
      BenchStart("$$$ Decoration (experimental)"),
      WithStateP(
        _state => {
          Promise.resolved(list{})
        },
      ),
      BenchEnd("$$$ Decoration (experimental)"),
    }
  | Refresh => list{
      BenchStart("$$$ Refreshing decorations"),
      WithState(
        state => {
          // highlightings
          Decoration.refresh(state.editor, state.decorations)
          // goal decorations
          state.goals->Array.forEach(goal => goal->Goal.refreshDecoration(state.editor))
        },
      ),
      BenchEnd("$$$ Refreshing decorations"),
    }
  }
