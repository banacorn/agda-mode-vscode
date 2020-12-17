open! Task
open Belt

let addViaPipe = (state: State.t, highlightings) =>
  state.decorations->Decoration.addDirectly(highlightings)

let addViaFile = (state: State.t, filepath) => state.decorations->Decoration.addIndirectly(filepath)

let clear = (state: State.t) => Decoration.removeAppliedDecorations(state.decorations)

let apply = (state: State.t) => Decoration.readTempFiles(state.decorations)->Promise.map(() => {
    Decoration.applyHighlightings(state.decorations, state.editor)
  })

let refresh = (state: State.t) => {
  // highlightings
  Decoration.refresh(state.editor, state.decorations)
  // goal decorations
  state.goals->Array.forEach(goal => goal->Goal.refreshDecoration(state.editor))
}
