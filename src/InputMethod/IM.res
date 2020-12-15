open Belt

type offset = int
type interval = (offset, offset)

let fromOffset = (document, offset) => document->VSCode.TextDocument.positionAt(offset)
let toOffset = (document, position) => document->VSCode.TextDocument.offsetAt(position)

let fromInterval = (document, interval) =>
  VSCode.Range.make(document->fromOffset(fst(interval)), document->fromOffset(snd(interval)))
let toInterval = (document, range) => (
  document->toOffset(VSCode.Range.start(range)),
  document->toOffset(VSCode.Range.end_(range)),
)

module Input = {
  type candidateInput =
    | ChooseSymbol(string)
    | BrowseUp
    | BrowseDown
    | BrowseLeft
    | BrowseRight
  type t =
    | Activate(array<interval>)
    | Deactivate
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

// for testing
module Log = {
  type kind =
    | UpdateView
    | RewriteIssued(array<(interval, string)>)
    | RewriteApplied
    | Activate
    | Deactivate
  type t = array<kind>

  let fromOutput = (xs: Output.t) => xs->Array.map(x =>
      switch x {
      | UpdateView(_, _, _) => UpdateView
      | Rewrite(xs, _) => RewriteIssued(xs)
      | Activate => Activate
      | Deactivate => Deactivate
      }
    )
}

module type Module = {
  type t

  let make: Chan.t<Log.t> => t
  let isActivated: t => bool

  let run: (t, option<VSCode.TextEditor.t>, Input.t) => Output.t
  // let deviseChange: (t, string, string) => option<Input.t>
}

module Module: Module = {
  module Instance = {
    type t = {
      mutable interval: interval,
      mutable decoration: option<Editor.Decoration.t>,
      mutable buffer: Buffer.t,
    }

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

    let destroy = instance => instance.decoration->Option.forEach(Editor.Decoration.destroy)
  }

  type t = {
    mutable instances: array<Instance.t>,
    mutable activated: bool,
    mutable semaphore: bool,
    // for reporting when some task has be done
    chanLog: Chan.t<Log.t>,
  }

  let logOutput = (self, output) =>
    if Array.length(output) > 0 {
      Chan.emit(self.chanLog, Log.fromOutput(output))
    }
  let logRewriteApplied = self => Chan.emit(self.chanLog, [RewriteApplied])

  let make = chanLog => {
    {
      instances: [],
      activated: false,
      semaphore: false,
      chanLog: chanLog,
    }
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
  let validateCursorPositions = (instances, offsets): array<Instance.t> =>
    instances->Array.keep((instance: Instance.t) => {
      // if any cursor falls into the range of the instance, the instance survives
      let survived = offsets->Array.some(Instance.withIn(instance))
      // if not, the instance gets destroyed
      if !survived {
        Instance.destroy(instance)
      }
      survived
    })

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

          // issue rewrites
          shouldRewrite->Option.forEach(text => {
            let (start, end_) = instance.interval
            let delta = String.length(text) - (end_ - start)

            // update the interval
            instance.interval = (start + accum.contents, end_ + accum.contents + delta)
            Js.Array.push(
              {
                interval: (start, end_),
                text: text,
                instance: buffer.translation.further ? Some(instance) : None,
              },
              rewrites,
            )->ignore
            accum := accum.contents + delta
          })

          // destroy the instance when:
          //  1. interval length is 0
          //  2. no further possible transition
          if fst(instance.interval) == snd(instance.interval) {
            Instance.destroy(instance)
            None
          } else if buffer.translation.further {
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

  ////////////////////////////////////////////////////////////////////////////////////////////

  let isActivated = self => self.activated

  let deactivate = self => {
    // setContext
    VSCode.Commands.setContext("agdaModeTyping", false)->ignore

    self.instances->Array.forEach(Instance.destroy)
    self.instances = []
    self.activated = false
  }

  // iterate through a list of rewrites and apply them to the text editor
  let applyRewrites = (self, editor, rewrites): Output.t => {
    // lock before applying edits to the text editor
    self.semaphore = true

    // calculate the replacements to be made to the editor
    let replacements = rewrites->Array.map(({interval, text}) => (interval, text))

    let (promise, resolve) = Promise.pending()

    // this promise will be resolved, once the real edits have been made
    promise->Promise.get(() => {
      // redecorate instances
      rewrites->Array.forEach(rewrite => {
        rewrite.instance->Option.forEach(instance => {
          editor->Option.forEach(Instance.redecorate(instance))
        })
      })

      // unlock the semaphore
      self.semaphore = false

      // for testing
      logRewriteApplied(self)
    })

    // deactivate if there are no instances left
    switch self.instances[0] {
    | None =>
      deactivate(self)
      [Output.Rewrite(replacements, resolve), Deactivate]
    | Some(instance) => [
        Output.Rewrite(replacements, resolve),
        UpdateView(
          Buffer.toSequence(instance.buffer),
          instance.buffer.translation,
          instance.buffer.candidateIndex,
        ),
      ]
    }
  }

  let rec run = (self, editor, input) => {
    let output = switch input {
    | Input.Activate(intervals) =>
      self.activated = true
      // setContext
      VSCode.Commands.setContext("agdaModeTyping", true)->ignore

      // instantiate from an array of offsets
      self.instances =
        Js.Array.sortInPlaceWith((x, y) => compare(fst(x), fst(y)), intervals)->Array.map(
          Instance.make(editor),
        )

      [Output.Activate]
    | Deactivate =>
      deactivate(self)
      [Output.Deactivate]
    | Select(offsets) =>
      if self.activated && !self.semaphore {
        self.instances = validateCursorPositions(self.instances, offsets)
        // deactivate if all instances have been destroyed
        if Js.Array.length(self.instances) == 0 {
          run(self, editor, Deactivate)
        } else {
          []
        }
      } else {
        []
      }
    | Change(changes) =>
      if Array.length(changes) !== 0 && self.activated && !self.semaphore {
        // update the offsets to reflect the changes
        let (instances, rewrites) = updateInstances(self.instances, changes)
        self.instances = instances
        // apply rewrites onto the text editor
        applyRewrites(self, editor, rewrites)
      } else {
        []
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
    logOutput(self, output)
    output
  }
}
include Module
