open Belt

// TODO: sort these errors out
module Error = {
  module LSP = {
    // Errors when sending Command to the server
    module CommandErr = {
      type t =
        | CannotDecodeJSON(string)
        | CannotParseCommand(string)

      let toString = x =>
        switch x {
        | CannotDecodeJSON(s) => "Cannot decode JSON: \n" ++ s
        | CannotParseCommand(s) => "Cannot read IOTCM: \n" ++ s
        }

      open Json.Decode
      open Util.Decode
      let decode: decoder<t> = sum(x =>
        switch x {
        | "CmdErrCannotDecodeJSON" => Contents(string |> map(version => CannotDecodeJSON(version)))
        | "CmdErrCannotParseCommand" =>
          Contents(string |> map(version => CannotParseCommand(version)))
        | tag => raise(DecodeError("[LSP.CommandErr] Unknown constructor: " ++ tag))
        }
      )
    }

    type t =
      // Errors originated from the LSP client within
      | Connection(Js.Exn.t)
      // Errors when sending Command to ther server
      | SendCommand(CommandErr.t)
      // Cannot initialize the connection
      | Initialize
      // Parsing / Decoding
      | CannotDecodeCommandRes(string, Js.Json.t)
      | CannotDecodeResponse(string, Js.Json.t)

    let toString = error =>
      switch error {
      | Connection(exn) =>
        let isECONNREFUSED =
          Js.Exn.message(exn)->Option.mapWithDefault(
            false,
            Js.String.startsWith("connect ECONNREFUSED"),
          )

        isECONNREFUSED
          ? ("[LSP] Connection Error", "Please enter \":main -d\" in ghci")
          : (
              "[LSP] Client Internal Connection Error",
              Js.Exn.message(exn)->Option.getWithDefault(""),
            )
      | SendCommand(e) => ("[LSP] Cannot Send Command", CommandErr.toString(e))
      | Initialize => ("[LSP] Cannot Initialize Connection", "")
      | CannotDecodeCommandRes(msg, json) => (
          "[LSP] Cannot Send Command",
          "Cannot decode the result after sending command" ++ msg ++ "\n" ++ Json.stringify(json),
        )
      | CannotDecodeResponse(msg, json) => (
          "[LSP] Cannot Parse Response",
          "Cannot decode responses from the server" ++ msg ++ "\n" ++ Json.stringify(json),
        )
      }
  }

  type t =
    // probing "agda" or "als"
    | CannotConnectViaStdIO(AgdaModeVscode.Process.PathSearch.Error.t)
    | CannotConnectViaTCP(Js.Exn.t)
    | PathSearch(Process.PathSearch.Error.t)
    | Validation(Process.Validation.Error.t)
    | Process(Process.Error.t)
    // LSP related
    | LSP(LSP.t)
    // Emacs S-expression parse error
    | ResponseParseError(Parser.Error.t)
    | NotConnectedYet
  let toString = x =>
    switch x {
    | CannotConnectViaStdIO(e) =>
      let (_header, body) = AgdaModeVscode.Process.PathSearch.Error.toString(e)
      ("Cannot locate \"als\"", body ++ "\nPlease make sure that the executable is in the path")
    | CannotConnectViaTCP(_) => (
        "Cannot connect with the server",
        "Please enter \":main -d\" in ghci",
      )
    | PathSearch(e) => Process.PathSearch.Error.toString(e)
    | Validation(e) => Process.Validation.Error.toString(e)
    | Process(e) => Process.Error.toString(e)
    | LSP(e) => LSP.toString(e)
    | ResponseParseError(e) => ("Internal Parse Error", Parser.Error.toString(e))
    | NotConnectedYet => ("Connection not established yet", "")
    }
}

type version = string

// This module makes sure that Last Responses are handled after NonLast Responses
//
// There are 2 kinds of Responses
//  NonLast Response :
//    * get handled first
//    * don't invoke `sendAgdaRequest`
//  Last Response :
//    * have priorities, those with the smallest priority number are executed first
//    * only get handled:
//        1. after prompt has reappeared
//        2. after all NonLast Responses
//        3. after all interactive highlighting is complete
//    * may invoke `sendAgdaRequest`
module Scheduler: {
  type t
  type handler = result<Response.t, Error.t> => Promise.t<unit>
  let make: unit => t
  let runNonLast: (t, handler, Response.t) => unit
  let addLast: (t, int, Response.t) => unit
  let runLast: (t, handler) => unit
} = {
  type t = {
    // keep the number of running NonLast Response
    mutable tally: int,
    allDone: Chan.t<unit>,
    deferredLastResponses: array<(int, Response.t)>,
  }
  type handler = result<Response.t, Error.t> => Promise.t<unit>

  let make = () => {
    tally: 0,
    allDone: Chan.make(),
    deferredLastResponses: [],
  }
  // NonLast Responses should fed here
  let runNonLast = (self, handler, response) => {
    // Js.log("[NonLast] " ++ Response.toString(response))
    self.tally = self.tally + 1
    handler(Ok(response))->Promise.get(_ => {
      self.tally = self.tally - 1
      if self.tally == 0 {
        self.allDone->Chan.emit()
      }
    })
  }
  // deferred (Last) Responses are queued here
  let addLast = (self, priority, response) => {
    // Js.log("[Add Last] " ++ string_of_int(priority) ++ " " ++ Response.toString(response))
    Js.Array.push((priority, response), self.deferredLastResponses)->ignore
  }
  // gets resolved once there's no NonLast Responses running
  let onceDone = self =>
    if self.tally == 0 {
      Promise.resolved()
    } else {
      self.allDone->Chan.once
    }
  // start handling Last Responses, after all NonLast Responses have been handled
  let runLast = (self, handler) =>
    self
    ->onceDone
    ->Promise.get(() => {
      // sort the deferred Responses by priority (ascending order)
      let deferredLastResponses =
        Js.Array.sortInPlaceWith(
          (x, y) => compare(fst(x), fst(y)),
          self.deferredLastResponses,
        )->Array.map(snd)

      // insert `CompleteHighlightingAndMakePromptReappear` handling Last Responses
      Js.Array.unshift(
        Response.CompleteHighlightingAndMakePromptReappear,
        deferredLastResponses,
      )->ignore

      deferredLastResponses->Array.map(res => handler(Ok(res)))->Util.oneByOne->ignore
    })
}

module type Emacs = {
  type t
  let make: unit => Promise.t<result<t, Error.t>>
  let destroy: t => Promise.t<unit>
  let onResponse: (t, result<Response.t, Error.t> => Promise.t<unit>) => Promise.t<unit>
  let sendRequest: (t, string) => unit
  let getVersion: t => string
}

module Emacs: Emacs = {
  @bs.module external untildify: string => string = "untildify"

  module Metadata = {
    type t = {
      path: string,
      args: array<string>,
      version: string,
    }

    // for making error report
    let _toString = self => {
      let path = "* path: " ++ self.path
      let args = "* args: " ++ Util.Pretty.array(self.args)
      let version = "* version: " ++ self.version
      let os = "* platform: " ++ N.OS.type_()

      "## Parse Log" ++
      ("\n" ++
      (path ++ ("\n" ++ (args ++ ("\n" ++ (version ++ ("\n" ++ (os ++ "\n"))))))))
    }

    // a more sophiscated "make"
    let make = (path, args): Promise.t<result<t, Error.t>> => {
      let validator = (output): result<string, string> =>
        switch Js.String.match_(%re("/Agda version (.*)/"), output) {
        | None => Error("Cannot read Agda version")
        | Some(match_) =>
          switch match_[1] {
          | None => Error("Cannot read Agda version")
          | Some(version) => Ok(version)
          }
        }
      // normailize the path by replacing the tild "~/" with the absolute path of home directory
      let path = untildify(path)
      Process.Validation.run("\"" ++ (path ++ "\" -V"), validator)
      ->Promise.mapOk(version => {
        path: path,
        args: args,
        version: version,
      })
      ->Promise.mapError(e => Error.Validation(e))
    }
  }

  type response = Parser.Incr.Gen.t<result<Response.Prioritized.t, Parser.Error.t>>

  type t = {
    metadata: Metadata.t,
    process: Process.t,
    chan: Chan.t<result<response, Error.t>>,
    mutable encountedFirstPrompt: bool,
  }

  let destroy = self => {
    self.chan->Chan.destroy
    self.encountedFirstPrompt = false
    self.process->Process.destroy
  }

  let wire = (self): unit => {
    // We use the prompt "Agda2>" as the delimiter of the end of a response
    // However, the prompt "Agda2>" also appears at the very start of the conversation
    // So this would be what it looks like:
    //    >>> request
    //      stop          <------- wierd stop
    //      yield
    //      yield
    //      stop
    //    >> request
    //      yield
    //      yield
    //      stop

    let toResponse = Parser.Incr.Gen.flatMap(x =>
      switch x {
      | Error(parseError) => Parser.Incr.Gen.Yield(Error(parseError))
      | Ok(Parser.SExpression.A("Agda2>")) => Parser.Incr.Gen.Stop
      | Ok(tokens) => Parser.Incr.Gen.Yield(Response.Prioritized.parse(tokens))
      }
    )

    // resolves the requests in the queue
    let handleResponse = (res: response) =>
      switch res {
      | Yield(x) => self.chan->Chan.emit(Ok(Yield(x)))
      | Stop =>
        if self.encountedFirstPrompt {
          self.chan->Chan.emit(Ok(Stop))
        } else {
          // do nothing when encountering the first Stop
          self.encountedFirstPrompt = true
        }
      }

    let mapError = x => Parser.Incr.Gen.map(x =>
        switch x {
        | Ok(x) => Ok(x)
        | Error((no, e)) => Error(Parser.Error.SExpression(no, e))
        }
      , x)

    let pipeline = Parser.SExpression.makeIncr(x => x->mapError->toResponse->handleResponse)

    // listens to the "data" event on the stdout
    // The chunk may contain various fractions of the Agda output
    // TODO: handle the destructor
    let _destructor = self.process->Process.onOutput(x =>
      switch x {
      | Stdout(rawText) =>
        // split the raw text into pieces and feed it to the parser
        rawText->Parser.split->Array.forEach(Parser.Incr.feed(pipeline))
      | Stderr(_) => ()
      | Error(e) => self.chan->Chan.emit(Error(Process(e)))
      }
    )
  }

  let make = () => {
    let getPath = (): Promise.t<result<string, Error.t>> => {
      // first, get the path from the config (stored in the Editor)
      let storedPath = Config.getAgdaPath()
      if storedPath == "" || storedPath == "." {
        // if there's no stored path, find one from the OS (with the specified name)
        let agdaVersion = Config.getAgdaVersion()
        Process.PathSearch.run(agdaVersion)
        ->Promise.mapOk(Js.String.trim)
        ->Promise.mapError(e => Error.PathSearch(e))
      } else {
        Promise.resolved(Ok(storedPath))
      }
    }

    // store the path in the editor config
    let setPath = (metadata: Metadata.t): Promise.t<result<Metadata.t, Error.t>> =>
      Config.setAgdaPath(metadata.path)->Promise.map(() => Ok(metadata))

    let args = ["--interaction"]

    getPath()
    ->Promise.flatMapOk(path => {
      Metadata.make(path, args)
    })
    ->Promise.flatMapOk(setPath)
    ->Promise.mapOk(metadata => {
      metadata: metadata,
      process: Process.make(metadata.path, metadata.args),
      chan: Chan.make(),
      encountedFirstPrompt: false,
    })
    ->Promise.tapOk(wire)
  }

  let sendRequest = (connection, encoded): unit => connection.process->Process.send(encoded)->ignore

  let onResponse = (connection, callback) => {
    let scheduler = Scheduler.make()
    // this promise get resolved after all Responses has been received from Agda
    let (promise, stopListener) = Promise.pending()

    // There are 2 kinds of Responses
    //  NonLast Response :
    //    * get handled first
    //    * don't invoke `sendAgdaRequest`
    //  Last Response :
    //    * have priorities, those with the smallest priority number are executed first
    //    * only get handled:
    //        1. after prompt has reappeared
    //        2. after all NonLast Responses
    //        3. after all interactive highlighting is complete
    //    * may invoke `sendAgdaRequest`
    let listener: result<response, Error.t> => unit = x =>
      switch x {
      | Error(error) => callback(Error(error))->ignore
      | Ok(Parser.Incr.Gen.Yield(Error(error))) =>
        callback(Error(ResponseParseError(error)))->ignore
      | Ok(Yield(Ok(NonLast(response)))) => scheduler->Scheduler.runNonLast(callback, response)
      | Ok(Yield(Ok(Last(priority, response)))) => scheduler->Scheduler.addLast(priority, response)
      | Ok(Stop) =>
        // stop the Agda Response listener
        stopListener()
        // start handling Last Responses, after all NonLast Responses have been handled
        scheduler->Scheduler.runLast(callback)
      }

    let listenerHandle = ref(None)
    // start listening for responses
    listenerHandle := Some(connection.chan->Chan.on(listener))
    // destroy the listener after all responses have been received
    promise->Promise.tap(() =>
      listenerHandle.contents->Option.forEach(destroyListener => destroyListener())
    )
  }

  let getVersion = conn => conn.metadata.version
}

module LSP = {
  type method = ViaStdIO(string, string) | ViaTCP(int)

  module CommandReq = {
    type t = SYN | Command(string)

    open! Json.Encode
    let encode: encoder<t> = x =>
      switch x {
      | SYN => object_(list{("tag", string("CmdReqSYN"))})
      | Command(raw) => object_(list{("tag", string("CmdReq")), ("contents", raw |> string)})
      }
  }

  module CommandRes = {
    type t =
      | ACK(version)
      | Result(option<Error.LSP.CommandErr.t>)

    let fromJsError = (error: 'a): string => %raw("function (e) {return e.toString()}")(error)

    open Json.Decode
    open Util.Decode
    let decode: decoder<t> = sum(x =>
      switch x {
      | "CmdResACK" => Contents(string |> map(version => ACK(version)))
      | "CmdRes" => Contents(optional(Error.LSP.CommandErr.decode) |> map(error => Result(error)))
      | tag => raise(DecodeError("[LSP.CommandRes] Unknown constructor: " ++ tag))
      }
    )
  }

  module LSPResponse = {
    module DisplayInfo = {
      type t =
        | Generic(string, array<Component.Item.t>)
        | CompilationOk(array<string>, array<string>)
        | AllGoalsWarnings(
            string,
            array<(RichText.t, string)>,
            array<(RichText.t, string, Common.AgdaRange.t)>,
            array<string>,
            array<string>,
          )
        | CurrentGoal(Component.Item.t)
        | InferredType(Component.Item.t)
        | Auto(string)
        | Error'(string)
        | Time(string)
        | NormalForm(string)

      open Json.Decode
      open Util.Decode
      let decode: decoder<t> = sum(x =>
        switch x {
        | "DisplayInfoGeneric" =>
          Contents(pair(string, array(Component.Item.decode)) |> map(((header, itmes)) => Generic(header, itmes)))
        | "DisplayInfoAllGoalsWarnings" =>
          Contents(
            tuple5(
              string,
              array(pair(RichText.decode, string)),
              array(tuple3(RichText.decode, string, Common.AgdaRange.decode)),
              array(string),
              array(string),
            ) |> map(((header, goals, metas, warnings, errors)) => AllGoalsWarnings(
              header,
              goals,
              metas,
              warnings,
              errors,
            )),
          )
        | "DisplayInfoCurrentGoal" =>
          Contents(Component.Item.decode |> map(body => CurrentGoal(body)))
        | "DisplayInfoInferredType" =>
          Contents(Component.Item.decode |> map(body => InferredType(body)))
        | "DisplayInfoCompilationOk" =>
          Contents(
            pair(array(string), array(string)) |> map(((warnings, errors)) => CompilationOk(
              warnings,
              errors,
            )),
          )
        | "DisplayInfoAuto" => Contents(string |> map(body => Auto(body)))
        | "DisplayInfoError" => Contents(string |> map(body => Error'(body)))
        | "DisplayInfoTime" => Contents(string |> map(body => Time(body)))
        | "DisplayInfoNormalForm" => Contents(string |> map(body => NormalForm(body)))
        | tag => raise(DecodeError("[LSP.DisplayInfo] Unknown constructor: " ++ tag))
        }
      )
    }

    type t =
      | ResponseNonLast(Response.t)
      | ResponseLast(int, Response.t)
      | ResponseParseError(Parser.Error.t)
      | ResponseEnd

    let toString = x =>
      switch x {
      | ResponseNonLast(s) => Response.toString(s)
      | ResponseLast(i, s) => "[Last " ++ string_of_int(i) ++ "] " ++ Response.toString(s)
      | ResponseParseError(e) => Parser.Error.toString(e)
      | ResponseEnd => "========"
      }

    open Json.Decode
    open Util.Decode
    let decode: decoder<t> = sum(x =>
      switch x {
      | "ResponseHighlightingInfoDirect" =>
        Contents(
          Highlighting.Infos.decode |> map((Highlighting.Infos.Infos(keepHighlighting, infos)) => {
            ResponseNonLast(Response.HighlightingInfoDirect(keepHighlighting, infos))
          }),
        )
      | "ResponseHighlightingInfoIndirect" =>
        Contents(
          string |> map(filePath => ResponseNonLast(
            Response.HighlightingInfoIndirectJSON(filePath),
          )),
        )
      | "ResponseDisplayInfo" =>
        open DisplayInfo
        Contents(
          DisplayInfo.decode |> map(info =>
            switch info {
            // | Generic("*Constraints*", "nil") => ResponseNonLast(DisplayInfo(Constraints(None)))
            | Generic(header, body) =>
              ResponseNonLast(DisplayInfo(Generic(header, body)))
            // | Generic("*Helper function*", body) =>
            //   ResponseNonLast(DisplayInfo(HelperFunction(body)))
            // | Generic("*Search About*", body) => ResponseNonLast(DisplayInfo(SearchAbout(body)))
            // | Generic("*Inferred Type*", body) => ResponseNonLast(DisplayInfo(InferredType(body)))
            // | Generic("*Goal type etc.*", body) => ResponseNonLast(DisplayInfo(GoalType(body)))
            // | Generic("*Module contents*", body) =>
              // ResponseNonLast(DisplayInfo(ModuleContents(body)))
            // | Generic("*Scope Info*", body) => ResponseNonLast(DisplayInfo(WhyInScope(body)))
            // | Generic("*Context*", body) => ResponseNonLast(DisplayInfo(Context(body)))
            // | Generic("*Intro*", body) => ResponseNonLast(DisplayInfo(Intro(body)))
            // | Generic("*Agda Version*", body) => ResponseNonLast(DisplayInfo(Version(body)))
            // | Generic(header, body) => ResponseNonLast(DisplayInfo(AllGoalsWarnings(header, body)))
            | AllGoalsWarnings(header, goals, metas, warnings, errors) =>
              ResponseNonLast(
                Response.DisplayInfo(AllGoalsWarningsLSP(header, goals, metas, warnings, errors)),
              )
            | CurrentGoal(item) => ResponseNonLast(Response.DisplayInfo(CurrentGoalLSP(item)))
            | InferredType(item) => ResponseNonLast(Response.DisplayInfo(InferredTypeLSP(item)))
            | CompilationOk(warnings, errors) =>
              ResponseNonLast(Response.DisplayInfo(CompilationOkLSP(warnings, errors)))
            | Auto(body) => ResponseNonLast(Response.DisplayInfo(Auto(body)))
            | Error'(body) => ResponseNonLast(Response.DisplayInfo(Error(body)))
            | Time(body) => ResponseNonLast(Response.DisplayInfo(Time(body)))
            | NormalForm(body) => ResponseNonLast(Response.DisplayInfo(NormalForm(body)))
            }
          ),
        )
      | "ResponseStatus" =>
        Contents(
          pair(bool, bool) |> map(((checked, displayImplicit)) => ResponseNonLast(
            Response.Status(checked, displayImplicit),
          )),
        )
      | "ResponseRunningInfo" =>
        Contents(
          pair(int, string) |> map(((verbosity, info)) => ResponseNonLast(
            Response.RunningInfo(verbosity, info),
          )),
        )
      | "ResponseClearHighlightingTokenBased" =>
        TagOnly(ResponseNonLast(Response.ClearHighlighting))
      | "ResponseClearHighlightingNotOnlyTokenBased" =>
        TagOnly(ResponseNonLast(Response.ClearHighlighting))
      | "ResponseClearRunningInfo" => TagOnly(ResponseNonLast(Response.ClearRunningInfo))
      | "ResponseDoneAborting" => TagOnly(ResponseNonLast(Response.DoneAborting))
      | "ResponseDoneExiting" => TagOnly(ResponseNonLast(Response.DoneExiting))
      | "ResponseGiveAction" =>
        Contents(
          pair(int, Response.GiveAction.decode) |> map(((id, giveAction)) => ResponseNonLast(
            Response.GiveAction(id, giveAction),
          )),
        )
      | "ResponseInteractionPoints" =>
        Contents(array(int) |> map(ids => ResponseLast(1, InteractionPoints(ids))))
      | "ResponseMakeCaseFunction" =>
        Contents(
          array(string) |> map(payload => ResponseLast(2, Response.MakeCase(Function, payload))),
        )
      | "ResponseMakeCaseExtendedLambda" =>
        Contents(
          array(string) |> map(payload => ResponseLast(
            2,
            Response.MakeCase(ExtendedLambda, payload),
          )),
        )
      | "ResponseSolveAll" =>
        Contents(
          array(pair(int, string)) |> map(payloads => ResponseLast(2, Response.SolveAll(payloads))),
        )
      | "ResponseJumpToError" =>
        Contents(
          pair(string, int) |> map(((filePath, offset)) => ResponseLast(
            3,
            Response.JumpToError(filePath, offset),
          )),
        )
      | "ResponseEnd" => TagOnly(ResponseEnd)
      | tag => raise(DecodeError("[LSP.Response] Unknown constructor: " ++ tag))
      }
    )
  }

  module type Module = {
    // lifecycle
    let start: bool => Promise.t<result<(version, method), Error.t>>
    let stop: unit => Promise.t<unit>
    // messaging
    let sendRequest: (string, Scheduler.handler) => Promise.t<result<unit, Error.t>>
    let getVersion: unit => option<version>
    let onError: (Error.t => unit) => VSCode.Disposable.t
    // predicate
    let isConnected: unit => bool
  }

  module Module: Module = {
    module Client = {
      open VSCode

      type t = {
        client: LSP.LanguageClient.t,
        subscription: VSCode.Disposable.t,
        method: method,
      }

      // for emitting errors
      let errorChan: Chan.t<Js.Exn.t> = Chan.make()
      // for emitting data
      let dataChan: Chan.t<Js.Json.t> = Chan.make()

      let onError = callback =>
        errorChan->Chan.on(e => callback(Error.LSP(Connection(e))))->VSCode.Disposable.make
      let onResponse = callback => dataChan->Chan.on(callback)->VSCode.Disposable.make

      let sendRequest = (self, data) =>
        self.client
        ->LSP.LanguageClient.onReady
        ->Promise.Js.toResult
        ->Promise.flatMapOk(() => {
          self.client->LSP.LanguageClient.sendRequest("agda", data)->Promise.Js.toResult
        })
        ->Promise.mapError(exn => Error.LSP(Connection(exn)))

      let destroy = self => {
        self.subscription->VSCode.Disposable.dispose->ignore
        self.client->LSP.LanguageClient.stop->Promise.Js.toResult->Promise.map(_ => ())
      }

      let make = method => {
        let serverOptions = switch method {
        | ViaTCP(port) => LSP.ServerOptions.makeWithStreamInfo(port)
        | ViaStdIO(name, _path) => LSP.ServerOptions.makeWithCommand(name)
        }

        let clientOptions = {
          // Register the server for plain text documents
          let documentSelector: DocumentSelector.t = [
            StringOr.others({
              open DocumentFilter
              {
                scheme: Some("file"),
                pattern: None,
                language: Some("agda"),
              }
            }),
          ]

          // Notify the server about file changes to '.clientrc files contained in the workspace
          let synchronize: FileSystemWatcher.t = Workspace.createFileSystemWatcher(
            %raw("'**/.clientrc'"),
            ~ignoreCreateEvents=false,
            ~ignoreChangeEvents=false,
            ~ignoreDeleteEvents=false,
          )

          let errorHandler: LSP.ErrorHandler.t = LSP.ErrorHandler.make(
            ~error=(exn, _msg, _count) => {
              errorChan->Chan.emit(exn)
              Shutdown
            },
            ~closed=() => {
              DoNotRestart
            },
          )
          LSP.LanguageClientOptions.make(documentSelector, synchronize, errorHandler)
        }

        // Create the language client
        let languageClient = LSP.LanguageClient.make(
          "agdaLanguageServer",
          "Agda Language Server",
          serverOptions,
          clientOptions,
        )

        let self = {
          client: languageClient,
          subscription: languageClient->LSP.LanguageClient.start,
          method: method,
        }

        // Let `LanguageClient.onReady` and `errorChan->Chan.once` race
        Promise.race(list{
          self.client->LSP.LanguageClient.onReady->Promise.Js.toResult,
          errorChan->Chan.once->Promise.map(err => Error(err)),
        })->Promise.map(result =>
          switch result {
          | Error(error) => Error(Error.LSP(Connection(error)))
          | Ok() =>
            self.client->LSP.LanguageClient.onRequest("agda", json => {
              dataChan->Chan.emit(json)
              Promise.resolved()
            })
            Ok(self)
          }
        )
      }
    }

    // for internal bookkeeping
    type state =
      | Disconnected
      | Connected(Client.t, version)
    // internal state singleton
    let singleton: ref<state> = ref(Disconnected)

    // stop the LSP client
    let stop = () =>
      switch singleton.contents {
      | Disconnected => Promise.resolved()
      | Connected(client, _version) =>
        // update the status
        singleton := Disconnected
        // destroy the client
        client->Client.destroy
      }

    // catches exceptions occured when decoding JSON values
    let decodeCommandRes = (json: Js.Json.t): result<CommandRes.t, Error.t> =>
      switch CommandRes.decode(json) {
      | response => Ok(response)
      | exception Json.Decode.DecodeError(msg) =>
        Error(Error.LSP(CannotDecodeCommandRes(msg, json)))
      }

    let decodeResponse = (json: Js.Json.t): result<LSPResponse.t, Error.t> =>
      switch LSPResponse.decode(json) {
      | reaction => Ok(reaction)
      | exception Json.Decode.DecodeError(msg) => Error(Error.LSP(CannotDecodeResponse(msg, json)))
      }

    let onError = Client.onError

    let sendRequestPrim = (client, request): Promise.t<result<CommandRes.t, Error.t>> => {
      client
      ->Client.sendRequest(CommandReq.encode(request))
      ->Promise.flatMapOk(json => Promise.resolved(decodeCommandRes(json)))
    }

    // see if the server is available
    let probe = (tryTCP, port, name) => {
      // see if "als" is available
      let probeStdIO = name => {
        AgdaModeVscode.Process.PathSearch.run(name)
        ->Promise.mapOk(path => ViaStdIO(name, Js.String.trim(path)))
        ->Promise.mapError(e => Error.CannotConnectViaStdIO(e))
      }
      // see if the TCP port is available
      let probeTCP = port => {
        let (promise, resolve) = Promise.pending()
        // connect and resolve `Ok()`` on success
        let socket = N.Net.connect(port, () => resolve(Ok()))
        // resolve `Error(CannotConnect(Js.Exn.t))` on error
        socket->N.Net.Socket.on(#error(exn => resolve(Error(exn))))->ignore
        // destroy the connection afterwards
        promise->Promise.mapOk(() => {
          N.Net.Socket.destroy(socket)->ignore
          ViaTCP(port)
        })
      }
      if tryTCP {
        probeTCP(port)->Promise.flatMapError(error => {
          Js.log2(
            "Got the following error when trying to connect to the Agda language server via TCP:",
            error,
          )
          probeStdIO(name)
        })
      } else {
        probeStdIO(name)
      }
    }

    // start the LSP client
    let start = tryTCP =>
      switch singleton.contents {
      | Disconnected =>
        probe(tryTCP, 4096, "als")
        ->Promise.flatMapOk(Client.make)
        ->Promise.flatMap(result =>
          switch result {
          | Error(error) =>
            singleton.contents = Disconnected
            Promise.resolved(Error(error))
          | Ok(client) =>
            // send `ReqInitialize` and wait for `ResInitialize` before doing anything else
            sendRequestPrim(client, SYN)->Promise.flatMapOk(response =>
              switch response {
              | Result(_) => Promise.resolved(Error(Error.LSP(Initialize)))
              | ACK(version) =>
                // update the status
                singleton.contents = Connected(client, version)
                Promise.resolved(Ok((version, client.method)))
              }
            )
          }
        )
      | Connected(client, version) => Promise.resolved(Ok((version, client.method)))
      }

    let getVersion = () =>
      switch singleton.contents {
      | Disconnected => None
      | Connected(_, version) => Some(version)
      }

    let isConnected = () =>
      switch singleton.contents {
      | Disconnected => false
      | Connected(_, _version) => true
      }

    let sendRequest = (request, handler) =>
      switch singleton.contents {
      | Connected(client, _version) =>
        let scheduler = Scheduler.make()
        // waits for `ResponseEnd`
        let (waitForResponseEnd, resolve) = Promise.pending()

        // listens for notifications
        let subscription = Client.onResponse(json => {
          switch decodeResponse(json) {
          | Ok(ResponseNonLast(responese)) => scheduler->Scheduler.runNonLast(handler, responese)
          | Ok(ResponseLast(priority, responese)) =>
            scheduler->Scheduler.addLast(priority, responese)
          | Ok(ResponseParseError(e)) => resolve(Error(Error.ResponseParseError(e)))
          | Ok(ResponseEnd) => resolve(Ok())
          | Error(error) => resolve(Error(error))
          }
        })

        let stopListeningForNotifications = () => subscription->VSCode.Disposable.dispose->ignore

        // sends `Command` and waits for `ResponseEnd`
        sendRequestPrim(client, Command(request))
        ->Promise.flatMapOk(result =>
          switch result {
          | ACK(_) => Promise.resolved(Error(Error.LSP(Initialize)))
          | Result(Some(error)) => Promise.resolved(Error(Error.LSP(SendCommand(error))))
          // waits for `ResponseEnd`
          | Result(None) => waitForResponseEnd
          }
        )
        // stop listening for notifications once the Command
        ->Promise.tap(_ => stopListeningForNotifications())
        ->Promise.tap(_ =>
          // start handling Last Responses, after all NonLast Responses have been handled
          scheduler->Scheduler.runLast(handler)
        )
      | Disconnected => Promise.resolved(Error(Error.NotConnectedYet))
      }
  }
  include Module
}
