module Scheduler = Connection__Scheduler
module Error = Connection__Error.CommWithALS
module LSP = Connection__Protocol__LSP

type version = string

module CommandReq = {
  type t = SYN | Command(string)

  let encode = {
    open JsonCombinators.Json.Encode
    Util.Encode.sum(x =>
      switch x {
      | SYN => TagOnly("CmdReqSYN")
      | Command(raw) => Payload("CmdReq", string(raw))
      }
    )
  }
}

module CommandRes = {
  type t =
    | ACK(version, option<version>) // agda version, language server version (ALS v6 and later)
    | Result(option<Connection__Error.CommWithALS.CommandErr.t>)

  let fromJsError = (error: 'a): string => %raw("function (e) {return e.toString()}")(error)

  let decode = {
    open JsonCombinators.Json.Decode
    field("tag", string)->flatMap(tag =>
      switch tag {
      | "CmdResACK" =>
        // ALS v5 and prior: agda version only
        // ALS v6 and later: agda version + language server version
        field("contents", 
          oneOf([
            pair(string, int)->map(((agdaVersion, alsVersion)) => ACK(agdaVersion, Some(alsVersion->string_of_int))),
            string->map(agdaVersion => ACK(agdaVersion, None))
          ])
        )
      | "CmdRes" =>
        field("contents", option(Connection__Error.CommWithALS.CommandErr.decode))->map(error => Result(error))
      | tag => 
        raise(DecodeError("[Connection.Target.ALS.CommandRes] Unknown constructor: " ++ tag))
      }
    )
  }
}

module ALSResponse = {
  module DisplayInfo = {
    type t =
      | Generic(string, array<Item.t>)
      | CompilationOk(array<string>, array<string>)
      | AllGoalsWarnings(string, array<Item.t>, array<Item.t>, array<string>, array<string>)
      | CurrentGoal(Item.t)
      | InferredType(Item.t)
      | Auto(string)
      | Error'(string)
      | Time(string)
      | NormalForm(string)

    let decode = {
      open JsonCombinators.Json.Decode
      Util.Decode.sum(x =>
        switch x {
        | "DisplayInfoGeneric" =>
          Payload(
            pair(string, array(Item.decode))->map(((header, items)) => Generic(header, items)),
          )
        | "DisplayInfoAllGoalsWarnings" =>
          Payload(
            Util.Decode.tuple5(
              string,
              array(Item.decode),
              array(Item.decode),
              array(string),
              array(string),
            )->map(((header, goals, metas, warnings, errors)) => AllGoalsWarnings(
              header,
              goals,
              metas,
              warnings,
              errors,
            )),
          )
        | "DisplayInfoCurrentGoal" => Payload(Item.decode->map(body => CurrentGoal(body)))
        | "DisplayInfoInferredType" => Payload(Item.decode->map(body => InferredType(body)))
        | "DisplayInfoCompilationOk" =>
          Payload(
            pair(array(string), array(string))->map(((warnings, errors)) => CompilationOk(
              warnings,
              errors,
            )),
          )
        | "DisplayInfoAuto" => Payload(string->map(body => Auto(body)))
        | "DisplayInfoError" => Payload(string->map(body => Error'(body)))
        | "DisplayInfoTime" => Payload(string->map(body => Time(body)))
        | "DisplayInfoNormalForm" => Payload(string->map(body => NormalForm(body)))
        | tag => raise(DecodeError("[ALS.DisplayInfo] Unknown constructor: " ++ tag))
        }
      )
    }
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

  let decode = {
    open JsonCombinators.Json.Decode
    Util.Decode.sum(x =>
      switch x {
      | "ResponseHighlightingInfoDirect" =>
        Payload(
          Token.decodeResponseHighlightingInfoDirect->map(((
            keepHighlighting,
            infos,
          )) => ResponseNonLast(Response.HighlightingInfoDirect(keepHighlighting, infos))),
        )
      | "ResponseHighlightingInfoIndirect" =>
        Payload(
          string->map(filePath => ResponseNonLast(Response.HighlightingInfoIndirectJSON(filePath))),
        )
      | "ResponseDisplayInfo" =>
        Payload(
          DisplayInfo.decode->map(info =>
            switch info {
            | Generic(header, body) => ResponseNonLast(DisplayInfo(Generic(header, body)))
            | AllGoalsWarnings(header, goals, metas, warnings, errors) =>
              ResponseNonLast(
                Response.DisplayInfo(AllGoalsWarningsALS(header, goals, metas, warnings, errors)),
              )
            | CurrentGoal(item) => ResponseNonLast(Response.DisplayInfo(CurrentGoalALS(item)))
            | InferredType(item) => ResponseNonLast(Response.DisplayInfo(InferredTypeALS(item)))
            | CompilationOk(warnings, errors) =>
              ResponseNonLast(Response.DisplayInfo(CompilationOkALS(warnings, errors)))
            | Auto(body) => ResponseNonLast(Response.DisplayInfo(Auto(body)))
            | Error'(body) => ResponseNonLast(Response.DisplayInfo(Error(body)))
            | Time(body) => ResponseNonLast(Response.DisplayInfo(Time(body)))
            | NormalForm(body) => ResponseNonLast(Response.DisplayInfo(NormalForm(body)))
            }
          ),
        )
      | "ResponseStatus" =>
        Payload(
          pair(bool, bool)->map(((checked, displayImplicit)) => ResponseNonLast(
            Response.Status(checked, displayImplicit),
          )),
        )
      | "ResponseRunningInfo" =>
        Payload(
          pair(int, string)->map(((verbosity, info)) => ResponseNonLast(
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
        Payload(
          pair(int, Response.GiveAction.decode)->map(((id, giveAction)) => ResponseNonLast(
            Response.GiveAction(id, giveAction),
          )),
        )
      | "ResponseInteractionPoints" =>
        Payload(array(int)->map(ids => ResponseLast(1, InteractionPoints(ids))))
      | "ResponseMakeCaseFunction" =>
        Payload(
          array(string)->map(payload => ResponseLast(2, Response.MakeCase(Function, payload))),
        )
      | "ResponseMakeCaseExtendedLambda" =>
        Payload(
          array(string)->map(payload => ResponseLast(
            2,
            Response.MakeCase(ExtendedLambda, payload),
          )),
        )
      | "ResponseSolveAll" =>
        Payload(
          array(pair(int, string))->map(payloads => ResponseLast(2, Response.SolveAll(payloads))),
        )
      | "ResponseJumpToError" =>
        Payload(
          pair(string, int)->map(((filePath, offset)) => ResponseLast(
            3,
            Response.JumpToError(filePath, offset),
          )),
        )
      | "ResponseEnd" => TagOnly(ResponseEnd)
      | tag => raise(DecodeError("[ALS.Response] Unknown constructor: " ++ tag))
      }
    )
  }
}

module type Module = {
  type t = {
    client: LSP.t,
    agdaVersion: version,
    alsVersion: option<version>,
    method: Connection__Transport.t,
  }
  // lifecycle
  let make: (
    Connection__Transport.t,
    option<Connection__Protocol__LSP__Binding.executableOptions>,
    JSON.t,
  ) => promise<result<t, Error.t>>
  let destroy: t => promise<result<unit, Error.t>>
  // messaging
  let sendRequest: (t, string, Response.t => promise<unit>) => promise<result<unit, Error.t>>
  // properties
  let getIPCMethod: t => Connection__Transport.t
}

module Module: Module = {
  type t = {
    client: LSP.t,
    agdaVersion: version,
    alsVersion: option<version>,
    method: Connection__Transport.t,
  }

  // catches exceptions occurred when decoding JSON values
  let decodeCommandRes = (json: JSON.t): result<CommandRes.t, Error.t> =>
    switch JsonCombinators.Json.decode(json, CommandRes.decode) {
    | Ok(response) => Ok(response)
    | Error(msg) => Error(CannotDecodeCommandRes(msg, json))
    }

  let decodeResponse = (json: JSON.t): result<ALSResponse.t, Error.t> =>
    switch JsonCombinators.Json.decode(json, ALSResponse.decode) {
    | Ok(reaction) => Ok(reaction)
    | Error(msg) => Error(CannotDecodeResponse(msg, json))
    }

  let sendRequestPrim = async (client, request): result<CommandRes.t, Error.t> => {
    switch await client->LSP.sendRequest(CommandReq.encode(request)) {
    | Ok(json) => decodeCommandRes(json)
    | Error(error) => Error(Error.ConnectionError(error))
    | exception Exn.Error(exn) => Error(Error.ConnectionError(exn))
    }
  }

  // start the ALS client
  let make = async (method, lspOptions, options) => {
    switch await LSP.make("agda", "Agda Language Server", method, lspOptions, options) {
    | Error(error) => Error(Error.ConnectionError(error))
    | exception Exn.Error(error) => Error(Error.ConnectionError(error))
    | Ok(client) =>
      // send `ReqInitialize` and wait for `ResInitialize` before doing anything else
      switch await sendRequestPrim(client, SYN) {
      | Error(error) => Error(error)
      | Ok(Result(_)) => Error(Error.Initialize)
      | Ok(ACK(agdaVersion, alsVersion)) => Ok({client, agdaVersion, alsVersion, method})
      }
    }
  }

  // destroy the client
  let destroy = async self =>
    switch await self.client->LSP.destroy {
    | Ok(result) => Ok(result)
    | Error(error) => Error(Error.ConnectionError(error))
    | exception Exn.Error(exn) => Error(Error.ConnectionError(exn))
    }

  let sendRequest = async (self, request, handler) => {
    let handler = response => handler(response)

    let scheduler = Scheduler.make()
    // waits for `ResponseEnd`
    let (waitForResponseEnd, resolve, _) = Util.Promise_.pending()

    // listens for responses from Agda
    let stopListeningForNotifications = self.client->LSP.onRequest(async json => {
      switch decodeResponse(json) {
      | Ok(ResponseNonLast(responses)) => scheduler->Scheduler.runNonLast(handler, responses)
      | Ok(ResponseLast(priority, responses)) => scheduler->Scheduler.addLast(priority, responses)
      | Ok(ResponseParseError(e)) => resolve(Error(Error.ResponseParseError(e)))
      | Ok(ResponseEnd) => resolve(Ok())
      | Error(error) => resolve(Error(error))
      }
      Ok(Js_json.null)
    })

    // sends `Command` and waits for `ResponseEnd`
    let result = switch await sendRequestPrim(self.client, Command(request)) {
    | Error(error) => Error(error)
    | Ok(ACK(_)) => Error(Error.Initialize)
    | Ok(Result(Some(error))) => Error(Error.SendCommand(error))
    // waits for `ResponseEnd`
    | Ok(Result(None)) => await waitForResponseEnd
    }
    // stop listening for requests from server once `ResponseEnd` arrived
    stopListeningForNotifications->VSCode.Disposable.dispose->ignore
    // start handling Last Responses, after all NonLast Responses have been handled
    let _ = await scheduler->Scheduler.runLast(handler)
    result
  }

  let getIPCMethod = conn => conn.method
}
include Module
