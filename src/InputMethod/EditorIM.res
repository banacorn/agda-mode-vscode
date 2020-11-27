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
  open Belt

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

  module Instance = {
    type t = {
      mutable range: (int, int),
      mutable decoration: array<Editor.Decoration.t>,
      mutable buffer: Buffer.t,
    }

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
    mutable cursorsToBeChecked: option<array<VSCode.Position.t>>,
    mutable busy: bool,
    mutable handles: array<VSCode.Disposable.t>,
    // for notifying the Task Dispatcher
    chan: Chan.t<Command.InputMethod.t>,
    // for reporting when some task has be done
    chanTest: Chan.t<event>,
  }

  let fromContentChangeEvent = (change: VSCode.TextDocumentContentChangeEvent.t): Buffer.change => {
    offset: change->VSCode.TextDocumentContentChangeEvent.rangeOffset,
    insertedText: change->VSCode.TextDocumentContentChangeEvent.text,
    replacedTextLength: change->VSCode.TextDocumentContentChangeEvent.rangeLength,
  }

  // datatype for representing a rewrite to be made to the text editor
  type rewrite = {
    rangeBefore: (int, int),
    rangeAfter: (int, int),
    text: string,
    instance: option<Instance.t>, // update range offsets after rewrite
  }

  // kill the Instances that are not are not pointed by cursors
  let checkCursorPositions = (self, document, points: array<VSCode.Position.t>) => {
    let offsets = points->Array.map(VSCode.TextDocument.offsetAt(document))
    log(
      "\n### Cursors  : " ++
      (Js.Array.sortInPlaceWith(compare, offsets)->Array.map(string_of_int)->Util.Pretty.array ++
      ("\n### Instances: " ++
      self.instances
      ->Array.map(i =>
        "(" ++ (string_of_int(fst(i.range)) ++ (", " ++ (string_of_int(snd(i.range)) ++ ")")))
      )
      ->Util.Pretty.array)),
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

    self.cursorsToBeChecked = None

    // emit "Deactivate" if all instances have been destroyed
    if Array.length(self.instances) == 0 {
      self.chan->Chan.emit(Deactivate)
      self.chanTest->Chan.emit(Deactivate)
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

    // before applying edits to the text editor, flip the semaphore
    self.busy = true
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

      // all offsets updated and rewrites applied, reset the semaphore
      self.busy = false
      // see if there are any pending cursor positions to be checked
      switch self.cursorsToBeChecked {
      | None => ()
      | Some(points) => checkCursorPositions(self, document, points)
      }
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

  let activate = (self, editor, ranges: array<(int, int)>) => {
    self.activated = true

    // emit ACTIVATE event after applied rewriting
    self.chanTest->Chan.emit(Activate)
    // setContext
    VSCode.Commands.setContext("agdaModeTyping", true)->ignore

    // instantiate from an array of offsets
    self.instances =
      Js.Array.sortInPlaceWith((x, y) => compare(fst(x), fst(y)), ranges)->Array.map(
        Instance.make(editor),
      )

    // initiate listeners

    VSCode.Window.onDidChangeTextEditorSelection(.event => {
      let points =
        event->VSCode.TextEditorSelectionChangeEvent.selections->Array.map(VSCode.Selection.anchor)
      if self.busy {
        // cannot check cursor positions at this moment
        // store cursor positions and wait until the system is not busy
        self.cursorsToBeChecked = Some(points)
      } else {
        checkCursorPositions(self, VSCode.TextEditor.document(editor), points)
      }
    })->Js.Array.push(self.handles)->ignore

    // listens to changes from the text editor
    VSCode.Workspace.onDidChangeTextDocument(.event => {
      let changes = event->VSCode.TextDocumentChangeEvent.contentChanges

      if !self.busy && Array.length(changes) > 0 {
        let changes = changes->Array.map(fromContentChangeEvent)
        // update the offsets to reflect the changes
        let (instances, rewrites) = updateInstanceOffsets(self.instances, changes)
        self.instances = instances
        // apply rewrites onto the text editor
        applyRewrites(self, editor, rewrites)
        // update the view
        updateView(self)
      }
    })->Js.Array.push(self.handles)->ignore
  }

  let deactivate = self => {
    // setContext
    VSCode.Commands.setContext("agdaModeTyping", false)->ignore

    self.chanTest->Chan.emit(Deactivate)

    self.instances->Array.forEach(Instance.destroy)
    self.instances = []
    self.activated = false
    self.cursorsToBeChecked = None
    self.busy = false
    self.handles->Array.forEach(VSCode.Disposable.dispose)
    self.handles = []
  }

  let make = chanTest => {
    instances: [],
    activated: false,
    cursorsToBeChecked: None,
    busy: false,
    handles: [],
    chan: Chan.make(),
    chanTest: chanTest,
  }
  ////////////////////////////////////////////////////////////////////////////////////////////

  let isActivated = self => self.activated == true

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
