module Scheduler = Connection__Scheduler
module Client = Connection__LSP__Client
module Error = Connection__LSP__Error

// type method = ViaStdIO(string, string) | ViaTCP(int)
type version = string

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
    | Result(option<Connection__LSP__Error.CommandErr.t>)

  let fromJsError = (error: 'a): string => %raw("function (e) {return e.toString()}")(error)

  open Json.Decode
  open Util.Decode
  let decode: decoder<t> = sum(x =>
    switch x {
    | "CmdResACK" => Contents(string |> map(version => ACK(version)))
    | "CmdRes" =>
      Contents(optional(Connection__LSP__Error.CommandErr.decode) |> map(error => Result(error)))
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
          array<Component.Item.t>,
          array<Component.Item.t>,
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
        Contents(
          pair(string, array(Component.Item.decode)) |> map(((header, itmes)) => Generic(
            header,
            itmes,
          )),
        )
      | "DisplayInfoAllGoalsWarnings" =>
        Contents(
          tuple5(
            string,
            array(Component.Item.decode),
            array(Component.Item.decode),
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
        string |> map(filePath => ResponseNonLast(Response.HighlightingInfoIndirectJSON(filePath))),
      )
    | "ResponseDisplayInfo" =>
      open DisplayInfo
      Contents(
        DisplayInfo.decode |> map(info =>
          switch info {
          | Generic(header, body) => ResponseNonLast(DisplayInfo(Generic(header, body)))
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
    | "ResponseClearHighlightingTokenBased" => TagOnly(ResponseNonLast(Response.ClearHighlighting))
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
  type t
  // lifecycle
  let make: bool => Promise.t<result<t, Error.t>>
  let destroy: t => Promise.t<unit>
  // messaging
  let sendRequest: (t, string, result<Response.t, Error.t> => Promise.t<unit>) => Promise.t<result<unit, Error.t>>
  let getInfo: t => (version, Client.method)
}

module Module: Module = {
  type t = {
    client: Client.t,
    version: version,
    method: Client.method,
  }

  // see if the server is available
  let probe = (tryTCP, name) => {
    // see if "als" is available
    let probeStdIO = name => {
      Connection__Process.PathSearch.run(
        name,
        "Please make sure that the language server is installed on the path",
      )
      ->Promise.mapOk(path => Client.ViaStdIO(name, Js.String.trim(path)))
      ->Promise.mapError(e => Error.PathSearch(e))
    }
    // see if the TCP port is available
    let probeTCP = port => {
      let (promise, resolve) = Promise.pending()
      // connect and resolve `Ok()`` on success
      let socket = N.Net.connect(port, () => resolve(Ok()))
      // resolve an error
      socket->N.Net.Socket.on(#error(exn => resolve(Error(Error.PortSearch(port, exn)))))->ignore
      // destroy the connection afterwards
      promise->Promise.mapOk(() => {
        N.Net.Socket.destroy(socket)->ignore
        Client.ViaTCP(port)
      })
    }
    if tryTCP {
      let portNumber = Config.Connection.getAgdaLanguageServerPort()
      probeTCP(portNumber)->Promise.flatMapError(error => {
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
  // catches exceptions occured when decoding JSON values
  let decodeCommandRes = (json: Js.Json.t): result<CommandRes.t, Error.t> =>
    switch CommandRes.decode(json) {
    | response => Ok(response)
    | exception Json.Decode.DecodeError(msg) => Error(CannotDecodeCommandRes(msg, json))
    }

  let decodeResponse = (json: Js.Json.t): result<LSPResponse.t, Error.t> =>
    switch LSPResponse.decode(json) {
    | reaction => Ok(reaction)
    | exception Json.Decode.DecodeError(msg) => Error(CannotDecodeResponse(msg, json))
    }

  let sendRequestPrim = (client, request): Promise.t<result<CommandRes.t, Error.t>> => {
    client
    ->Client.sendRequest(CommandReq.encode(request))
    ->Promise.flatMapOk(json => Promise.resolved(decodeCommandRes(json)))
  }

  // start the LSP client
  let make = tryTCP =>
    probe(tryTCP, "als")
    ->Promise.flatMapOk(Client.make)
    ->Promise.flatMapOk(client =>
      // send `ReqInitialize` and wait for `ResInitialize` before doing anything else
      sendRequestPrim(client, SYN)->Promise.flatMapOk(response =>
        switch response {
        | Result(_) => Promise.resolved(Error(Error.Initialize))
        | ACK(version) =>
          Promise.resolved(Ok({client: client, version: version, method: Client.getMethod(client)}))
        }
      )
    )

  // destroy the client
  let destroy = self => self.client->Client.destroy

  let getInfo = self => (self.version, self.method)

  let sendRequest = (self, request, handler) => {
    let handler = response => handler(Ok(response))
    
    let scheduler = Scheduler.make()
    // waits for `ResponseEnd`
    let (waitForResponseEnd, resolve) = Promise.pending()

    // listens for notifications
    let subscription = Client.onResponse(json => {
      
      switch decodeResponse(json) {
      | Ok(ResponseNonLast(responese)) => scheduler->Scheduler.runNonLast(handler, responese)
      | Ok(ResponseLast(priority, responese)) => scheduler->Scheduler.addLast(priority, responese)
      | Ok(ResponseParseError(e)) => resolve(Error(Error.ResponseParseError(e)))
      | Ok(ResponseEnd) => resolve(Ok())
      | Error(error) => resolve(Error(error))
      }
    })

    let stopListeningForNotifications = () => subscription->VSCode.Disposable.dispose->ignore

    // sends `Command` and waits for `ResponseEnd`
    sendRequestPrim(self.client, Command(request))
    ->Promise.flatMapOk(result =>
      switch result {
      | ACK(_) => Promise.resolved(Error(Error.Initialize))
      | Result(Some(error)) => Promise.resolved(Error(Error.SendCommand(error)))
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
  }
}
include Module
