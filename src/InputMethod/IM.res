open Common

module Input = {
  type candidateInput =
    | ChooseSymbol(string)
    | BrowseUp
    | BrowseDown
    | BrowseLeft
    | BrowseRight
  type t =
    | Activate(array<Interval.t>)
    | Deactivate
    | KeyUpdate(array<Buffer.change>)
    | MouseSelect(array<Interval.t>)
    | Candidate(candidateInput)

  let fromTextDocumentChangeEvent = (editor, event) => {
    // see if the change event happened in this TextEditor
    let fileName = editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName->Parser.filepath
    let eventFileName =
      event->VSCode.TextDocumentChangeEvent.document->VSCode.TextDocument.fileName->Parser.filepath
    if fileName == eventFileName {
      // TextDocumentContentChangeEvent.t => Buffer.change
      event
      ->VSCode.TextDocumentChangeEvent.contentChanges
      ->Array.map(change => {
        Buffer.offset: change->VSCode.TextDocumentContentChangeEvent.rangeOffset,
        insertedText: change->VSCode.TextDocumentContentChangeEvent.text,
        replacedTextLength: change->VSCode.TextDocumentContentChangeEvent.rangeLength,
      })
    } else {
      []
    }
  }
}

module Output = {
  type kind =
    | UpdateView(string, Translator.translation, int)
    | Rewrite(array<(Interval.t, string)>, unit => unit)
    | Activate
    | Deactivate

  type t = array<kind>
}

// for testing
module Log = {
  type kind =
    | UpdateView
    | RewriteIssued(array<(Interval.t, string)>)
    | RewriteApplied
    | Activate
    | Deactivate
  type t = array<kind>

  let fromOutput = (xs: Output.t) =>
    xs->Array.map(x =>
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

  // To enable input '\' in editor,
  // we need to check whether buffer is empty when input method is activated
  // (Actually, we don't need this function, but I somehow can't get field instances from IM.t in State__InputMethod.res)
  let bufferIsEmpty: t => bool

  let run: (t, option<VSCode.TextEditor.t>, Input.t) => Output.t
  // let deviseChange: (t, string, string) => option<Input.t>
}

module Module: Module = {
  module Instance = {
    type t = {
      mutable interval: Interval.t,
      mutable decoration: option<Editor.Decoration.t>,
      mutable buffer: Buffer.t,
    }

    let make = (editor, interval) =>
      switch editor {
      | None => {
          interval,
          decoration: None,
          buffer: Buffer.make(),
        }
      | Some(editor) =>
        let document = VSCode.TextEditor.document(editor)
        let range = Editor.Range.fromInterval(document, interval)
        {
          interval,
          decoration: Some(Editor.Decoration.underlineText(editor, range)),
          buffer: Buffer.make(),
        }
      }

    let withIn = (instance, offset) => Interval.contains(instance.interval, offset)

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
      chanLog,
    }
  }

  // datatype for representing a rewrite to be made to the text editor
  type rewrite = {
    interval: Interval.t,
    text: string,
    // `instance` has been destroyed if is None
    instance: option<Instance.t>,
  }

  // kill the Instances that are not are not pointed by cursors
  // returns `true` when the system should be Deactivate
  let validateCursorPositions = (instances, intervals): array<Instance.t> =>
    instances->Array.filter((instance: Instance.t) => {
      // if any selection falls into the range of the instance, the instance survives
      let survived =
        intervals->Array.some(((start, end_)) =>
          Instance.withIn(instance, start) && Instance.withIn(instance, end_)
        )

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

    instances->Array.filterMap(instance => {
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
      instancesWithChanges->Array.filterMap(((instance, change)) =>
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
            rewrites
            ->Array.push({
              interval: (start, end_),
              text,
              instance: buffer.translation.further ? Some(instance) : None,
            })
            ->ignore
            accum := accum.contents + delta
          })

          // destroy the instance when:
          //  1. interval length is 0
          //  2. no further possible transition and there is 0 or 1 candidate symbol
          let intervalLengthIs0 = fst(instance.interval) == snd(instance.interval)
          let numberOfCandidateSymbols = Array.length(buffer.translation.candidateSymbols)

          let shouldDestroy =
            intervalLengthIs0 || (!buffer.translation.further && numberOfCandidateSymbols <= 1)
          if shouldDestroy {
            Instance.destroy(instance)
            None
          } else {
            instance.buffer = buffer
            Some(instance)
          }
        }
      )
    }

    (instances, rewrites)
  }

  ////////////////////////////////////////////////////////////////////////////////////////////

  let bufferIsEmpty = (im: t): bool => {
    open Buffer
    im.instances->Array.every(instance => instance.buffer->isEmpty)
  }

  let isActivated = self => self.activated

  let deactivate = self => {
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

    let (promise, resolve, _) = Util.Promise_.pending()

    // this promise will be resolved, once the real edits have been made
    promise
    ->Promise.finally(() => {
      // redecorate instances
      rewrites->Array.forEach(rewrite => {
        rewrite.instance->Option.forEach(
          instance => {
            editor->Option.forEach(Instance.redecorate(instance, ...))
          },
        )
      })

      // unlock the semaphore
      self.semaphore = false

      // for testing
      logRewriteApplied(self)
    })
    ->Promise.done

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

      // instantiate from an array of offsets
      self.instances = {
        intervals->Array.sort((x, y) => Int.compare(fst(x), fst(y)))
        intervals->Array.map(Instance.make(editor, ...))
      }

      [Output.Activate]
    | Deactivate =>
      deactivate(self)
      [Output.Deactivate]
    | MouseSelect(intervals) =>
      if self.activated && !self.semaphore {
        self.instances = validateCursorPositions(self.instances, intervals)

        // deactivate if all instances have been destroyed
        if Js.Array.length(self.instances) == 0 {
          run(self, editor, Deactivate)
        } else {
          []
        }
      } else {
        []
      }
    | KeyUpdate(changes) =>
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
