open Belt

type offset = int
type interval = (offset, offset)

module type Semaphore = {
  type t<'a>

  let make: unit => t<'a>
  let isLocked: t<'a> => bool
  let lock: t<'a> => unit
  let unlock: (t<'a>, 'a) => unit
  let acquire: t<'a> => Promise.t<option<'a>>
}
module Semaphore: Semaphore = {
  type t<'a> = ref<option<(Promise.t<option<'a>>, option<'a> => unit)>>

  let make = () => ref(None)

  let isLocked = self => self.contents->Option.isSome

  let lock = self => {
    Js.log(">>>>> LOCK")
    let (promise, resolver) = Promise.pending()
    self.contents = Some(promise, resolver)
  }

  let unlock = (self, value) =>
    switch self.contents {
    | None => ()
    | Some((_, resolver)) =>
      Js.log(">>>>> UNLOCK")
      self.contents = None
      resolver(Some(value))
    }

  let acquire = self =>
    switch self.contents {
    | None => Promise.resolved(None)
    | Some((promise, _)) => promise
    }
}

module Input = {
  type candidateInput =
    | ChooseSymbol(string)
    | BrowseUp
    | BrowseDown
    | BrowseLeft
    | BrowseRight
  type t =
    | Change(array<Buffer.change>)
    | Select(array<offset>)
    | Candidate(candidateInput)

  let fromTextDocumentChangeEvent = (editor, event) => {
    // see if the change event happened in this TextEditor
    let fileName = editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName
    let eventFileName = event->VSCode.TextDocumentChangeEvent.document->VSCode.TextDocument.fileName
    if fileName == eventFileName {
      // TextDocumentContentChangeEvent.t => Buffer.change
      Change(event->VSCode.TextDocumentChangeEvent.contentChanges->Array.map(change => {
          Buffer.offset: change->VSCode.TextDocumentContentChangeEvent.rangeOffset,
          insertedText: change->VSCode.TextDocumentContentChangeEvent.text,
          replacedTextLength: change->VSCode.TextDocumentContentChangeEvent.rangeLength,
        }))
    } else {
      Change([])
    }
  }
}

module Output = {
  type kind =
    | UpdateView(string, Translator.translation, int)
    | Rewrite(array<(interval, string)>, unit => unit)
    | Activate
    | Deactivate

  type t = array<kind>
}

module type Module = {
  type t

  let make: Chan.t<Output.kind> => t
  let activate: (t, option<VSCode.TextEditor.t>, array<interval>) => unit
  let deactivate: t => unit
  let isActivated: t => bool

  let run: (t, option<VSCode.TextEditor.t>, Input.t) => Promise.t<Output.t>
  let deviseChange: (t, string, string) => option<Input.t>
}
module Module: Module = {
  let printLog = false
  let log = if printLog {
    Js.log
  } else {
    _ => ()
  }

  module Instance = {
    type t = {
      mutable interval: interval,
      mutable decoration: option<Editor.Decoration.t>,
      mutable buffer: Buffer.t,
    }

    let toString = self =>
      "(" ++ string_of_int(fst(self.interval)) ++ ", " ++ (string_of_int(snd(self.interval)) ++ ")")

    let make = (editor, interval) =>
      switch editor {
      | None => {
          interval: interval,
          decoration: None,
          buffer: Buffer.make(),
        }
      | Some(editor) =>
        let document = VSCode.TextEditor.document(editor)
        let (start, end_) = interval
        let start = document->VSCode.TextDocument.positionAt(start)
        let end_ = document->VSCode.TextDocument.positionAt(end_)
        let range = VSCode.Range.make(start, end_)

        {
          interval: interval,
          decoration: Some(Editor.Decoration.underlineText(editor, range)),
          buffer: Buffer.make(),
        }
      }

    let withIn = (instance, offset) => {
      let (start, end_) = instance.interval
      start <= offset && offset <= end_
    }

    let redecorate = (instance, editor) => {
      instance.decoration->Option.forEach(Editor.Decoration.destroy)

      let document = VSCode.TextEditor.document(editor)
      let (start, end_) = instance.interval
      let start = document->VSCode.TextDocument.positionAt(start)
      let end_ = document->VSCode.TextDocument.positionAt(end_)
      let range = VSCode.Range.make(start, end_)

      instance.decoration = Some(Editor.Decoration.underlineText(editor, range))
    }

    let destroy = instance => {
      instance.decoration->Option.forEach(Editor.Decoration.destroy)
    }
  }

  type t = {
    mutable instances: array<Instance.t>,
    mutable activated: bool,
    // cursor positions will NOT be validated until the semaphore `busy` is flipped false
    // cursor positions waiting to be validated will be queued here and resolved afterwards
    semaphore: Semaphore.t<array<(interval, string)>>,
    // for reporting when some task has be done
    chanLog: Chan.t<Output.kind>,
  }

  // datatype for representing a rewrite to be made to the text editor
  type rewrite = {
    interval: interval,
    text: string,
    // `instance` has been destroyed if is None
    instance: option<Instance.t>,
  }

  // kill the Instances that are not are not pointed by cursors
  // returns `true` when the system should be Deactivate
  let validateCursorPositions = (self, offsets) => {
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
  }

  //
  let toRewrites = (instances: array<Instance.t>, modify: Instance.t => option<string>): array<
    rewrite,
  > => {
    let accum = ref(0)

    instances->Array.keepMap(instance => {
      let (start, end_) = instance.interval

      // update the interval with `accum`
      instance.interval = (start + accum.contents, end_ + accum.contents)

      modify(instance)->Option.map(replacement => {
        let delta = String.length(replacement) - (end_ - start)
        // update `accum`
        accum := accum.contents + delta

        // update the interval with the change `delta`
        instance.interval = (fst(instance.interval), snd(instance.interval) + delta)

        // returns a `rewrite`
        {
          interval: instance.interval,
          text: replacement,
          instance: Some(instance),
        }
      })
    })
  }

  // iterate through a list of rewrites and apply them to the text editor
  let applyRewrites = (self, editor, rewrites): Promise.t<Output.t> => {
    // lock before applying edits to the text editor
    self.semaphore->Semaphore.lock

    // calculate the replacements to be made to the editor
    let replacements = rewrites->Array.map(({interval, text}) => (interval, text))

    let (promise, resolve) = Promise.pending()

    promise->Promise.get(() => {
      // redecorate and update intervals of each Instance
      rewrites->Array.forEach(rewrite => {
        rewrite.instance->Option.forEach(instance => {
          editor->Option.forEach(Instance.redecorate(instance))
        })
      })

      // all offsets updated and rewrites have been applied
      // unlock the semaphore
      self.semaphore->Semaphore.unlock(replacements)

      // // for testing
      // self.instances[0]->Option.forEach(instance => {
      //   self.chanLog->Chan.emit(
      //     UpdateView(
      //       Buffer.toSequence(instance.buffer),
      //       instance.buffer.translation,
      //       instance.buffer.candidateIndex,
      //     ),
      //   )
      // })
    })

    // update the view
    switch self.instances[0] {
    | None => Promise.resolved([Output.Rewrite(replacements, resolve), Deactivate])
    | Some(instance) =>
      // real output
      Promise.resolved([
        Output.Rewrite(replacements, resolve),
        UpdateView(
          Buffer.toSequence(instance.buffer),
          instance.buffer.translation,
          instance.buffer.candidateIndex,
        ),
      ])
    }
  }

  let groupChangeWithInstances = (
    instances: array<Instance.t>,
    changes: array<Buffer.change>,
  ): array<(Instance.t, option<Buffer.change>)> => {
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
        let (start, end_) = instance.interval
        let delta = String.length(change.insertedText) - change.replacedTextLength
        if Instance.withIn(instance, change.offset) {
          // `change` appears inside the `instance`
          instance.interval = (accum + start, accum + end_ + delta)
          list{
            (instance, Some({...change, offset: change.offset + accum})),
            ...go(accum + delta, (cs, is)),
          }
        } else if change.offset < fst(instance.interval) {
          // `change` appears before the `instance`
          go(accum + delta, (cs, list{instance, ...is})) // update only `accum`
        } else {
          // `change` appears after the `instance`
          instance.interval = (accum + start, accum + end_)
          list{(instance, None), ...go(accum, (list{change, ...cs}, is))}
        }
      | (list{}, list{instance, ...is}) => list{instance, ...is}->List.map(i => (i, None))
      | (_, list{}) => list{}
      }
    go(0, (List.fromArray(changes), List.fromArray(instances)))->List.toArray
  }

  // update offsets of Instances base on changes
  let updateInstances = (instances: array<Instance.t>, changes: array<Buffer.change>): (
    array<Instance.t>,
    array<rewrite>,
  ) => {
    let instancesWithChanges = groupChangeWithInstances(instances, changes)

    let rewrites = []

    // push rewrites to the `rewrites` queue
    let instances = {
      let accum = ref(0)
      instancesWithChanges->Array.keepMap(((instance, change)) =>
        switch change {
        | None => Some(instance)
        | Some(change) =>
          let (buffer, shouldRewrite) = Buffer.update(
            instance.buffer,
            fst(instance.interval),
            change,
          )
          // Js.log4("Buffer.update ", instance.buffer, fst(instance.interval), change)
          // Js.log3("Buffer.update2 ", buffer, shouldRewrite)

          // issue rewrites
          shouldRewrite->Option.forEach(text => {
            let (start, end_) = instance.interval
            let delta = String.length(text) - (end_ - start)

            // update the interval
            instance.interval = (start + accum.contents, end_ + accum.contents + delta)
            Js.Array.push(
              {
                interval: (start + accum.contents, end_ + accum.contents),
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

  let activate = (self, editor, cursors: array<interval>) => {
    self.activated = true

    // emit ACTIVATE event after applied rewriting
    // self.chanLog->Chan.emit(Activate)
    // setContext
    VSCode.Commands.setContext("agdaModeTyping", true)->ignore

    // instantiate from an array of offsets
    self.instances =
      Js.Array.sortInPlaceWith((x, y) => compare(fst(x), fst(y)), cursors)->Array.map(
        Instance.make(editor),
      )
  }

  let deactivate = self => {
    // setContext
    VSCode.Commands.setContext("agdaModeTyping", false)->ignore

    // self.chanLog->Chan.emit(Deactivate)

    self.instances->Array.forEach(Instance.destroy)
    self.instances = []
    self.activated = false
  }

  let make = chanLog => {
    {
      instances: [],
      activated: false,
      semaphore: Semaphore.make(),
      chanLog: chanLog,
    }
  }
  ////////////////////////////////////////////////////////////////////////////////////////////

  let isActivated = self => self.activated

  let run = (self, editor, input) =>
    switch input {
    | Input.Select(offsets) =>
      if self.activated && !Semaphore.isLocked(self.semaphore) {
        validateCursorPositions(self, offsets)
        if Js.Array.length(self.instances) == 0 {
          deactivate(self)
          Promise.resolved([Output.Deactivate])
        } else {
          Promise.resolved([])
        }
      } else {
        Promise.resolved([])
      }
    | Change(changes) =>
      if self.activated && !Semaphore.isLocked(self.semaphore) {
        // update the offsets to reflect the changes
        let (instances, rewrites) = updateInstances(self.instances, changes)
        self.instances = instances
        // apply rewrites onto the text editor
        applyRewrites(self, editor, rewrites)
      } else {
        Promise.resolved([])
      }
    | Candidate(action) =>
      let callback = switch action {
      | ChooseSymbol(symbol) => _ => Some(symbol)
      | BrowseUp =>
        instance => {
          instance.Instance.buffer = Buffer.moveUp(instance.Instance.buffer)
          instance.buffer.translation.candidateSymbols[instance.buffer.candidateIndex]
        }
      | BrowseDown =>
        instance => {
          instance.buffer = Buffer.moveDown(instance.buffer)
          instance.buffer.translation.candidateSymbols[instance.buffer.candidateIndex]
        }
      | BrowseLeft =>
        instance => {
          instance.buffer = Buffer.moveLeft(instance.buffer)
          instance.buffer.translation.candidateSymbols[instance.buffer.candidateIndex]
        }
      | BrowseRight =>
        instance => {
          instance.buffer = Buffer.moveRight(instance.buffer)
          instance.buffer.translation.candidateSymbols[instance.buffer.candidateIndex]
        }
      }
      let rewrites = toRewrites(self.instances, callback)
      applyRewrites(self, editor, rewrites)
    }

  // devise the "change" made to the input box
  let deviseChange = (self, previous, next): option<Input.t> =>
    self.instances[0]->Option.flatMap(instance => {
      let inputLength = String.length(next)
      let bufferSurface = Buffer.toSurface(instance.buffer)

      // helper funcion
      let init = s => Js.String.substring(~from=0, ~to_=String.length(s) - 1, s)
      let last = s => Js.String.substringToEnd(~from=String.length(s) - 1, s)

      if init(next) == previous ++ bufferSurface {
        // Insertion
        Some(
          Input.Change([
            {
              offset: inputLength - 1,
              insertedText: last(next),
              replacedTextLength: 0,
            },
          ]),
        )
      } else if next == previous || next == previous ++ init(bufferSurface) {
        // Backspacing
        Some(
          Input.Change([
            {
              offset: inputLength,
              insertedText: "",
              replacedTextLength: 1,
            },
          ]),
        )
      } else {
        None
      }
    })
}
include Module
