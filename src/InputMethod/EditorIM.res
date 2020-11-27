open Belt
// Validates cursor position
module type CursorValidator = {
  type t

  let make: unit => t
  let isBusy: t => bool
  let lock: t => unit
  let unlock: (t, array<VSCode.Position.t> => bool) => unit
  let validate: (t, array<VSCode.Position.t> => bool, array<VSCode.Position.t>) => Promise.t<bool>
}
module CursorValidator: CursorValidator = {
  type t = {
    // the semaphore
    mutable isBusy: bool,
    // cursor positions waiting to be validated + resolver
    // only accumulates when `isBusy` is flipped to `true`
    mutable backlog: array<(array<VSCode.Position.t>, bool => unit)>,
  }

  let make = () => {
    isBusy: false,
    backlog: [],
  }

  let isBusy = self => self.isBusy

  let lock = self => self.isBusy = true

  let unlock = (self, callback) => {
    self.isBusy = false

    self.backlog->Array.forEach(((positions, resolver)) => callback(positions)->resolver)
    self.backlog = []
  }

  let validate = (self, callback, positions) => {
    if self.isBusy {
      let (promise, resolver) = Promise.pending()
      Js.Array2.push(self.backlog, (positions, resolver))->ignore
      promise
    } else {
      callback(positions)->Promise.resolved
    }
  }
}

module type Module = {
  type t
  type event =
    | Change
    | Activate
    | Deactivate

  let make: Chan.t<event> => t
  let activate: (t, VSCode.TextEditor.t, array<(int, int)>) => unit
  let deactivate: t => unit
  let isActivated: t => bool

  // event => input
  let handleTextEditorSelectionChangeEvent: VSCode.TextEditorSelectionChangeEvent.t => array<
    VSCode.Position.t,
  >
  let handleTextDocumentChangeEvent: (
    VSCode.TextEditor.t,
    VSCode.TextDocumentChangeEvent.t,
  ) => array<Buffer.change>

  // input
  let changeSelection: (t, VSCode.TextEditor.t, array<VSCode.Position.t>) => Promise.t<bool>
  let changeDocument: (t, VSCode.TextEditor.t, array<Buffer.change>) => unit

  let onCommand: (t, Command.InputMethod.t => unit, unit) => unit
  // methods
  let chooseSymbol: (t, VSCode.TextEditor.t, string) => unit
  let moveUp: (t, VSCode.TextEditor.t) => unit
  let moveRight: (t, VSCode.TextEditor.t) => unit
  let moveDown: (t, VSCode.TextEditor.t) => unit
  let moveLeft: (t, VSCode.TextEditor.t) => unit
  let insertBackslash: VSCode.TextEditor.t => unit
  let insertChar: (VSCode.TextEditor.t, string) => unit
}
module Module: Module = {
  type event =
    | Change
    | Activate
    | Deactivate

  let printLog = false
  let log = if printLog {
    Js.log
  } else {
    _ => ()
  }

  // module type Instance = {
  //   type t

  //   // constructor/destructor
  //   let make: (VSCode.TextEditor.t, (int, int)) => t
  //   let destroy: t => unit
  //   // getters/setters
  //   let getRange: t => (int, int)
  //   let setRange: t => (int, int)

  //   // returns its range, for debugging
  //   let toString: t => string
  //   // see if an offset is with in its range
  //   let withIn: (t, int) => bool
  // }
  module Instance = {
    type t = {
      mutable range: (int, int),
      mutable decoration: array<Editor.Decoration.t>,
      mutable buffer: Buffer.t,
    }

    let toString = self =>
      "(" ++ string_of_int(fst(self.range)) ++ ", " ++ (string_of_int(snd(self.range)) ++ ")")

    let make = (editor, range) => {
      let document = VSCode.TextEditor.document(editor)
      let start = document->VSCode.TextDocument.positionAt(fst(range))
      let end_ = document->VSCode.TextDocument.positionAt(snd(range))
      {
        range: range,
        decoration: [Editor.Decoration.underlineText(editor, VSCode.Range.make(start, end_))],
        buffer: Buffer.make(),
      }
    }

    let withIn = (instance, offset) => {
      let (start, end_) = instance.range
      start <= offset && offset <= end_
    }

    let redocorate = (instance, editor) => {
      instance.decoration->Array.forEach(Editor.Decoration.destroy)
      instance.decoration = []

      let document = VSCode.TextEditor.document(editor)
      let (start, end_) = instance.range
      let start = document->VSCode.TextDocument.positionAt(start)
      let end_ = document->VSCode.TextDocument.positionAt(end_)
      let range = VSCode.Range.make(start, end_)

      instance.decoration = [Editor.Decoration.underlineText(editor, range)]
    }

    let destroy = instance => {
      instance.decoration->Array.forEach(Editor.Decoration.destroy)
      instance.decoration = []
    }
  }

  type t = {
    mutable instances: array<Instance.t>,
    mutable activated: bool,
    // mutable busy: bool,
    cursorValidator: CursorValidator.t,
    // cursor positions will NOT be validated until the semaphore `busy` is flipped false
    // cursor positions waiting to be validated will be queued here and resolved afterwards
    // mutable cursorPositionsToBeValidated: array<(array<VSCode.Position.t>, bool => unit)>,
    // for notifying the Task Dispatcher
    chan: Chan.t<Command.InputMethod.t>,
    // for reporting when some task has be done
    chanTest: Chan.t<event>,
  }

  // datatype for representing a rewrite to be made to the text editor
  type rewrite = {
    rangeBefore: (int, int),
    rangeAfter: (int, int),
    text: string,
    instance: option<Instance.t>, // update range offsets after rewrite
  }

  // kill the Instances that are not are not pointed by cursors
  // returns `true` when the system should be Deactivate
  let validateCursorPositions = (self, document, points: array<VSCode.Position.t>) => {
    let offsets = points->Array.map(VSCode.TextDocument.offsetAt(document))
    log(
      "\n### Cursors  : " ++
      (Js.Array.sortInPlaceWith(compare, offsets)->Array.map(string_of_int)->Util.Pretty.array ++
      ("\n### Instances: " ++ self.instances->Array.map(Instance.toString)->Util.Pretty.array)),
    )

    // store the surviving instances
    self.instances = self.instances->Array.keep((instance: Instance.t) => {
        // if any cursor falls into the range of the instance, the instance survives
        let survived = offsets->Array.some(Instance.withIn(instance))
        // if not, the instance gets destroyed
        if !survived {
          Instance.destroy(instance)
        }
        survived
      })

    // return `true` and emit `Deactivate` if all instances have been destroyed
    if Array.length(self.instances) == 0 {
      self.chanTest->Chan.emit(Deactivate)
      true
    } else {
      false
    }
  }

  let updateView = self =>
    // update the view
    self.instances[0]->Option.forEach(instance =>
      self.chan->Chan.emit(
        Update(
          Buffer.toSequence(instance.buffer),
          instance.buffer.translation,
          instance.buffer.candidateIndex,
        ),
      )
    )

  let toRewrites = (instances: array<Instance.t>, f: Instance.t => option<string>): array<
    rewrite,
  > => {
    let accum = ref(0)

    instances->Array.keepMap(instance => {
      let (start, end_) = instance.range

      // update the range
      instance.range = (start + accum.contents, end_ + accum.contents)

      f(instance)->Option.map(replacement => {
        let delta = String.length(replacement) - (end_ - start)
        // update `accum`
        accum := accum.contents + delta
        // returns a `rewrite`
        {
          rangeBefore: instance.range,
          rangeAfter: (fst(instance.range), snd(instance.range) + delta),
          text: replacement,
          instance: Some(instance),
        }
      })
    })
  }

  // iterate through a list of rewrites and apply them to the text editor
  let applyRewrites = (self, editor, rewrites) => {
    let document = VSCode.TextEditor.document(editor)

    // lock the CursorValidator before applying edits to the text editor
    self.cursorValidator->CursorValidator.lock
    // iterate though the list of rewrites
    rewrites->Array.map(({rangeBefore, rangeAfter, text, instance}, ()) => {
      let editorRange = VSCode.Range.make(
        document->VSCode.TextDocument.positionAt(fst(rangeBefore)),
        document->VSCode.TextDocument.positionAt(snd(rangeBefore)),
      )
      // update the text buffer
      Editor.Text.delete(document, editorRange)
      ->Promise.flatMap(_ => Editor.Text.insert(document, VSCode.Range.start(editorRange), text))
      ->Promise.map(_ =>
        switch // update range offsets and redecorate the Instance
        // if it still exist after this rewrite
        instance {
        | None => ()
        | Some(instance) =>
          instance.range = rangeAfter
          Instance.redocorate(instance, editor)
        }
      )
    })->Util.oneByOne->Promise.get(_ => {
      // emit CHANGE event after applied rewriting
      self.chanTest->Chan.emit(Change)

      // all offsets updated and rewrites have been applied
      // unlock the cursor validator
      self.cursorValidator->CursorValidator.unlock(
        validateCursorPositions(self, editor->VSCode.TextEditor.document),
      )
    })
  }

  // update offsets of Instances base on changes
  let updateInstanceOffsets = (instances: array<Instance.t>, changes: array<Buffer.change>): (
    array<Instance.t>,
    array<rewrite>,
  ) => {
    // sort the changes base on their offsets in the ascending order
    let changes = Js.Array.sortInPlaceWith(
      (x: Buffer.change, y: Buffer.change) => compare(x.offset, y.offset),
      changes,
    )

    // iterate through Instances and changes
    // returns a list of Instances along with the changeEvent event that occurred inside that Instance
    let rec go: (
      int,
      (list<Buffer.change>, list<Instance.t>),
    ) => list<(Instance.t, option<Buffer.change>)> = (accum, x) =>
      switch x {
      | (list{change, ...cs}, list{instance, ...is}) =>
        let (start, end_) = instance.range
        let delta = String.length(change.insertedText) - change.replacedTextLength
        if Instance.withIn(instance, change.offset) {
          // `change` appears inside the `instance`
          instance.range = (accum + start, accum + end_ + delta)
          list{
            (instance, Some({...change, offset: change.offset + accum})),
            ...go(accum + delta, (cs, is)),
          }
        } else if change.offset < fst(instance.range) {
          // `change` appears before the `instance`
          go(accum + delta, (cs, list{instance, ...is})) // update only `accum`
        } else {
          // `change` appears after the `instance`
          instance.range = (accum + start, accum + end_)
          list{(instance, None), ...go(accum, (list{change, ...cs}, is))}
        }
      | (list{}, list{instance, ...is}) => list{instance, ...is}->List.map(i => (i, None))
      | (_, list{}) => list{}
      }
    let instancesWithChanges =
      go(0, (List.fromArray(changes), List.fromArray(instances)))->List.toArray

    let rewrites = []

    // push rewrites to the `rewrites` queue
    let instances = {
      let accum = ref(0)
      instancesWithChanges->Array.keepMap(((instance, change)) =>
        switch change {
        | None => Some(instance)
        | Some(change) =>
          let (buffer, shouldRewrite) = Buffer.update(instance.buffer, fst(instance.range), change)
          // issue rewrites
          shouldRewrite->Option.forEach(text => {
            let (start, end_) = instance.range
            let delta = String.length(text) - (end_ - start)
            Js.Array.push(
              {
                rangeBefore: (start + accum.contents, end_ + accum.contents),
                rangeAfter: (start + accum.contents, end_ + accum.contents + delta),
                text: text,
                instance: buffer.translation.further ? Some(instance) : None,
              },
              rewrites,
            )->ignore
            accum := accum.contents + delta
          })

          // destroy the instance if there's no further possible transition
          if buffer.translation.further {
            instance.buffer = buffer
            Some(instance)
          } else {
            Instance.destroy(instance)
            None
          }
        }
      )
    }

    (instances, rewrites)
  }

  let activate = (self, editor, cursors: array<(int, int)>) => {
    self.activated = true

    // emit ACTIVATE event after applied rewriting
    self.chanTest->Chan.emit(Activate)
    // setContext
    VSCode.Commands.setContext("agdaModeTyping", true)->ignore

    // instantiate from an array of offsets
    self.instances =
      Js.Array.sortInPlaceWith((x, y) => compare(fst(x), fst(y)), cursors)->Array.map(
        Instance.make(editor),
      )
  }

  // TextEditorSelectionChangeEvent.t => array<Position.t>
  let handleTextEditorSelectionChangeEvent = event =>
    event->VSCode.TextEditorSelectionChangeEvent.selections->Array.map(VSCode.Selection.anchor)

  let handleTextDocumentChangeEvent = (editor, event) => {
    // see if the change event happened in this TextEditor
    let fileName = editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName
    let eventFileName = event->VSCode.TextDocumentChangeEvent.document->VSCode.TextDocument.fileName
    if fileName == eventFileName {
      // TextDocumentContentChangeEvent.t => Buffer.change
      event->VSCode.TextDocumentChangeEvent.contentChanges->Array.map(change => {
        Buffer.offset: change->VSCode.TextDocumentContentChangeEvent.rangeOffset,
        insertedText: change->VSCode.TextDocumentContentChangeEvent.text,
        replacedTextLength: change->VSCode.TextDocumentContentChangeEvent.rangeLength,
      })
    } else {
      []
    }
  }

  let changeSelection = (self, editor, positions) => {
    if self.activated {
      self.cursorValidator->CursorValidator.validate(
        validateCursorPositions(self, editor->VSCode.TextEditor.document),
        positions,
      )
    } else {
      Promise.resolved(false)
    }
  }

  let changeDocument = (self, editor, changes) => {
    if self.activated && !CursorValidator.isBusy(self.cursorValidator) {
      // update the offsets to reflect the changes
      let (instances, rewrites) = updateInstanceOffsets(self.instances, changes)
      self.instances = instances
      // apply rewrites onto the text editor
      applyRewrites(self, editor, rewrites)
      // update the view
      updateView(self)
    }
  }

  let deactivate = self => {
    // setContext
    VSCode.Commands.setContext("agdaModeTyping", false)->ignore

    self.chanTest->Chan.emit(Deactivate)

    self.instances->Array.forEach(Instance.destroy)
    self.instances = []
    self.activated = false
  }

  let make = chanTest => {
    instances: [],
    activated: false,
    cursorValidator: CursorValidator.make(),
    chan: Chan.make(),
    chanTest: chanTest,
  }
  ////////////////////////////////////////////////////////////////////////////////////////////

  let isActivated = self => self.activated

  let onCommand = (self, callback) => self.chan->Chan.on(callback)

  let moveUp = (self, editor) => {
    let rewrites = toRewrites(self.instances, instance => {
      instance.buffer = Buffer.moveUp(instance.buffer)
      instance.buffer.translation.candidateSymbols[instance.buffer.candidateIndex]
    })
    // apply rewrites onto the text editor
    applyRewrites(self, editor, rewrites)

    // update the view
    updateView(self)
  }

  let moveRight = (self, editor) => {
    let rewrites = toRewrites(self.instances, instance => {
      instance.buffer = Buffer.moveRight(instance.buffer)
      instance.buffer.translation.candidateSymbols[instance.buffer.candidateIndex]
    })
    // apply rewrites onto the text editor
    applyRewrites(self, editor, rewrites)
    // update the view
    updateView(self)
  }

  let moveDown = (self, editor) => {
    let rewrites = toRewrites(self.instances, instance => {
      instance.buffer = Buffer.moveDown(instance.buffer)
      instance.buffer.translation.candidateSymbols[instance.buffer.candidateIndex]
    })

    // apply rewrites onto the text editor
    applyRewrites(self, editor, rewrites)
    // update the view
    updateView(self)
  }

  let moveLeft = (self, editor) => {
    let rewrites = toRewrites(self.instances, instance => {
      instance.buffer = Buffer.moveLeft(instance.buffer)
      instance.buffer.translation.candidateSymbols[instance.buffer.candidateIndex]
    })

    // apply rewrites onto the text editor
    applyRewrites(self, editor, rewrites)
    // update the view
    updateView(self)
  }

  let chooseSymbol = (self, editor, symbol) => {
    let rewrites = toRewrites(self.instances, _ => Some(symbol))
    // apply rewrites onto the text editor
    applyRewrites(self, editor, rewrites)
    // update the view
    updateView(self)
  }

  let insertBackslash = editor =>
    Editor.Cursor.getMany(editor)->Array.forEach(point =>
      Editor.Text.insert(VSCode.TextEditor.document(editor), point, "\\")->ignore
    )
  let insertChar = (editor, char) => {
    let char = Js.String.charAt(0, char)
    Editor.Cursor.getMany(editor)->Array.forEach(point =>
      Editor.Text.insert(VSCode.TextEditor.document(editor), point, char)->ignore
    )
  }
}
include Module
