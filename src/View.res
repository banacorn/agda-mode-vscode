open Common

module Header = {
  type t =
    | Plain(string)
    | Success(string)
    | Warning(string)
    | Error(string)

  let toString = x =>
    switch x {
    | Plain(string) => string
    | Success(string) => string
    | Warning(string) => string
    | Error(string) => string
    }

  open Json.Decode
  open Util.Decode

  let decode: decoder<t> = sum(x =>
    switch x {
    | "Plain" => Contents(string |> map(text => Plain(text)))
    | "Success" => Contents(string |> map(text => Success(text)))
    | "Warning" => Contents(string |> map(text => Warning(text)))
    | "Error" => Contents(string |> map(text => Error(text)))
    | tag => raise(DecodeError("[Header] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Plain(text) => object_(list{("tag", string("Plain")), ("contents", text |> string)})
    | Success(text) => object_(list{("tag", string("Success")), ("contents", text |> string)})
    | Warning(text) => object_(list{("tag", string("Warning")), ("contents", text |> string)})
    | Error(text) => object_(list{("tag", string("Error")), ("contents", text |> string)})
    }
}

module Prompt = {
  type t = {
    body: option<string>,
    placeholder: option<string>,
    value: option<string>,
  }

  // JSON encode/decode

  open Json.Decode
  open Util.Decode

  let decode: decoder<t> = sum(x =>
    switch x {
    | "Prompt" =>
      Contents(
        tuple3(optional(string), optional(string), optional(string)) |> map(((
          body,
          placeholder,
          value,
        )) => {
          body: body,
          placeholder: placeholder,
          value: value,
        }),
      )
    | tag => raise(DecodeError("[Prompt] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | {body, placeholder, value} =>
      object_(list{
        ("tag", string("Prompt")),
        (
          "contents",
          (body, placeholder, value) |> tuple3(
            nullable(string),
            nullable(string),
            nullable(string),
          ),
        ),
      })
    }
}
module Body = {
  module Emacs = {
    type t =
      | Outputs
      | AllGoalsWarnings
      | GoalType
      | SearchAbout
      | Error
      | Text

    open Json.Decode
    open Util.Decode

    let decode: decoder<t> = sum(x =>
      switch x {
      | "Outputs" => TagOnly(Outputs)
      | "AllGoalsWarnings" => TagOnly(AllGoalsWarnings)
      | "GoalType" => TagOnly(GoalType)
      | "SearchAbout" => TagOnly(SearchAbout)
      | "Error" => TagOnly(Error)
      | "Text" => TagOnly(Text)
      | tag => raise(DecodeError("[Body.Emacs] Unknown constructor: " ++ tag))
      }
    )

    open! Json.Encode
    let encode: encoder<t> = x =>
      switch x {
      | Outputs => object_(list{("tag", string("Outputs"))})
      | AllGoalsWarnings => object_(list{("tag", string("AllGoalsWarnings"))})
      | GoalType => object_(list{("tag", string("GoalType"))})
      | SearchAbout => object_(list{("tag", string("SearchAbout"))})
      | Error => object_(list{("tag", string("Error"))})
      | Text => object_(list{("tag", string("Text"))})
      }
  }

  type t =
    | Nothing
    | Plain(string)
    | Emacs(Emacs.t, string, string)

  open Json.Decode
  open Util.Decode

  let decode: decoder<t> = sum(x =>
    switch x {
    | "Nothing" => TagOnly(Nothing)
    | "Plain" => Contents(string |> map(text => Plain(text)))
    | "Emacs" =>
      Contents(
        tuple3(Emacs.decode, string, string) |> map(((kind, header, body)) => Emacs(
          kind,
          header,
          body,
        )),
      )
    | tag => raise(DecodeError("[Body] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Nothing => object_(list{("tag", string("Nothing"))})
    | Plain(text) => object_(list{("tag", string("Plain")), ("contents", text |> string)})
    | Emacs(kind, header, body) =>
      object_(list{
        ("tag", string("Emacs")),
        ("contents", (kind, header, body) |> tuple3(Emacs.encode, string, string)),
      })
    }
}

module Link = {
  type t =
    | ToLocation(Agda.Location.t)
    | ToHole(int)

  open Json.Decode
  open Util.Decode

  let decode: decoder<t> = sum(x =>
    switch x {
    | "ToLocation" => Contents(Agda.Location.decode |> map(range => ToLocation(range)))
    | "ToHole" => Contents(int |> map(index => ToHole(index)))
    | tag => raise(DecodeError("[View.Link] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | ToLocation(range) =>
      object_(list{("tag", string("ToLocation")), ("contents", range |> Agda.Location.encode)})
    | ToHole(index) => object_(list{("tag", string("ToHole")), ("contents", index |> int)})
    }
}

module EventToView = {
  module InputMethod = {
    type t =
      | Activate
      | Deactivate
      | Update(string, Translator.translation, int)
      | BrowseUp
      | BrowseRight
      | BrowseDown
      | BrowseLeft

    open Json.Decode
    open Util.Decode

    let decode: decoder<t> = sum(x =>
      switch x {
      | "Activate" => TagOnly(Activate)
      | "Deactivate" => TagOnly(Deactivate)
      | "Update" =>
        Contents(
          tuple3(string, Translator.decode, int) |> map(((sequence, translation, index)) => Update(
            sequence,
            translation,
            index,
          )),
        )
      | "BrowseUp" => TagOnly(BrowseUp)
      | "BrowseRight" => TagOnly(BrowseRight)
      | "BrowseDown" => TagOnly(BrowseDown)
      | "BrowseLeft" => TagOnly(BrowseLeft)
      | tag => raise(DecodeError("[EventToView.InputMethod] Unknown constructor: " ++ tag))
      }
    )

    open! Json.Encode
    let encode: encoder<t> = x =>
      switch x {
      | Activate => object_(list{("tag", string("Activate"))})
      | Deactivate => object_(list{("tag", string("Deactivate"))})
      | Update(sequence, translation, index) =>
        object_(list{
          ("tag", string("Update")),
          ("contents", (sequence, translation, index) |> tuple3(string, Translator.encode, int)),
        })
      | BrowseUp => object_(list{("tag", string("BrowseUp"))})
      | BrowseRight => object_(list{("tag", string("BrowseRight"))})
      | BrowseDown => object_(list{("tag", string("BrowseDown"))})
      | BrowseLeft => object_(list{("tag", string("BrowseLeft"))})
      }
  }

  type t =
    | Display(Header.t, Body.t)
    | PromptInterrupt
    | PromptIMUpdate(string)
    | InputMethod(InputMethod.t)

  // JSON encode/decode

  open Json.Decode
  open Util.Decode

  let decode: decoder<t> = sum(x =>
    switch x {
    | "Display" =>
      Contents(pair(Header.decode, Body.decode) |> map(((header, body)) => Display(header, body)))
    | "PromptInterrupt" => TagOnly(PromptInterrupt)
    | "PromptIMUpdate" => Contents(string |> map(text => PromptIMUpdate(text)))
    | "InputMethod" => Contents(InputMethod.decode |> map(x => InputMethod(x)))
    | tag => raise(DecodeError("[EventToView] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Display(header, body) =>
      object_(list{
        ("tag", string("Display")),
        ("contents", (header, body) |> pair(Header.encode, Body.encode)),
      })
    | PromptInterrupt => object_(list{("tag", string("PromptInterrupt"))})
    | PromptIMUpdate(text) =>
      object_(list{("tag", string("PromptIMUpdate")), ("contents", text |> string)})
    | InputMethod(payload) =>
      object_(list{("tag", string("InputMethod")), ("contents", payload |> InputMethod.encode)})
    }
}

module Request = {
  type t = Prompt(Header.t, Prompt.t)

  // JSON encode/decode

  open Json.Decode
  open Util.Decode

  let decode: decoder<t> = sum(x =>
    switch x {
    | "Prompt" =>
      Contents(
        pair(Header.decode, Prompt.decode) |> map(((header, prompt)) => Prompt(header, prompt)),
      )
    | tag => raise(DecodeError("[Request] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Prompt(header, prompt) =>
      object_(list{
        ("tag", string("Prompt")),
        ("contents", (header, prompt) |> pair(Header.encode, Prompt.encode)),
      })
    }
}

module RequestOrEventToView = {
  type t =
    | Request(Request.t)
    | Event(EventToView.t)

  // JSON encode/decode

  open Json.Decode
  open Util.Decode

  let decode: decoder<t> = sum(x =>
    switch x {
    | "Request" => Contents(Request.decode |> map(x => Request(x)))
    | "Event" => Contents(EventToView.decode |> map(x => Event(x)))
    | tag => raise(DecodeError("[RequestOrEventToView] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Request(payload) =>
      object_(list{("tag", string("Request")), ("contents", payload |> Request.encode)})
    | Event(payload) =>
      object_(list{("tag", string("Event")), ("contents", payload |> EventToView.encode)})
    }
}

module Response = {
  type t =
    | PromptSuccess(string)
    | PromptInterrupted

  open Json.Decode
  open Util.Decode

  let decode: decoder<t> = sum(x =>
    switch x {
    | "PromptSuccess" => Contents(string |> map(result => PromptSuccess(result)))
    | "PromptInterrupted" => TagOnly(PromptInterrupted)
    | tag => raise(DecodeError("[Response] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | PromptSuccess(result) =>
      object_(list{("tag", string("PromptSuccess")), ("contents", result |> string)})
    | PromptInterrupted => object_(list{("tag", string("PromptInterrupted"))})
    }
}

module EventFromView = {
  module InputMethod = {
    type t =
      | InsertChar(string)
      | ChooseSymbol(string)

    open Json.Decode
    open Util.Decode

    let decode: decoder<t> = sum(x =>
      switch x {
      | "InsertChar" => Contents(string |> map(char => InsertChar(char)))
      | "ChooseSymbol" => Contents(string |> map(char => ChooseSymbol(char)))
      | tag => raise(DecodeError("[EventFromView.InputMethod] Unknown constructor: " ++ tag))
      }
    )

    open! Json.Encode
    let encode: encoder<t> = x =>
      switch x {
      | InsertChar(char) =>
        object_(list{("tag", string("InsertChar")), ("contents", char |> string)})
      | ChooseSymbol(symbol) =>
        object_(list{("tag", string("ChooseSymbol")), ("contents", symbol |> string)})
      }
  }

  module PromptIMUpdate = {
    type t =
      | MouseSelect(Interval.t)
      | KeyUpdate(string)
      | BrowseUp
      | BrowseDown
      | BrowseLeft
      | BrowseRight
      | Escape

    open Json.Decode
    open Util.Decode

    let decode: decoder<t> = sum(x =>
      switch x {
      | "MouseSelect" => Contents(Interval.decode |> map(interval => MouseSelect(interval)))
      | "KeyUpdate" => Contents(string |> map(char => KeyUpdate(char)))
      | "BrowseUp" => TagOnly(BrowseUp)
      | "BrowseDown" => TagOnly(BrowseDown)
      | "BrowseLeft" => TagOnly(BrowseLeft)
      | "BrowseRight" => TagOnly(BrowseRight)
      | "Escape" => TagOnly(Escape)
      | tag => raise(DecodeError("[EventFromView.Prompt] Unknown constructor: " ++ tag))
      }
    )

    open! Json.Encode
    let encode: encoder<t> = x =>
      switch x {
      | MouseSelect(interval) =>
        object_(list{("tag", string("MouseSelect")), ("contents", interval |> Interval.encode)})
      | KeyUpdate(char) => object_(list{("tag", string("KeyUpdate")), ("contents", char |> string)})
      | BrowseUp => object_(list{("tag", string("BrowseUp"))})
      | BrowseDown => object_(list{("tag", string("BrowseDown"))})
      | BrowseLeft => object_(list{("tag", string("BrowseLeft"))})
      | BrowseRight => object_(list{("tag", string("BrowseRight"))})
      | Escape => object_(list{("tag", string("Escape"))})
      }
  }

  type t =
    | Initialized
    | Destroyed
    | InputMethod(InputMethod.t)
    | PromptIMUpdate(PromptIMUpdate.t)
    | JumpToTarget(Link.t)
    | MouseOver(Link.t)
    | MouseOut(Link.t)

  open Json.Decode
  open Util.Decode

  let decode: decoder<t> = sum(x =>
    switch x {
    | "Initialized" => TagOnly(Initialized)
    | "Destroyed" => TagOnly(Destroyed)
    | "InputMethod" => Contents(InputMethod.decode |> map(action => InputMethod(action)))
    | "PromptIMUpdate" => Contents(PromptIMUpdate.decode |> map(action => PromptIMUpdate(action)))
    | "JumpToTarget" => Contents(Link.decode |> map(link => JumpToTarget(link)))
    | "MouseOver" => Contents(Link.decode |> map(link => MouseOver(link)))
    | "MouseOut" => Contents(Link.decode |> map(link => MouseOut(link)))
    | tag => raise(DecodeError("[Response.EventFromView] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Initialized => object_(list{("tag", string("Initialized"))})
    | Destroyed => object_(list{("tag", string("Destroyed"))})
    | InputMethod(action) =>
      object_(list{("tag", string("InputMethod")), ("contents", action |> InputMethod.encode)})
    | PromptIMUpdate(action) =>
      object_(list{
        ("tag", string("PromptIMUpdate")),
        ("contents", action |> PromptIMUpdate.encode),
      })
    | JumpToTarget(link) =>
      object_(list{("tag", string("JumpToTarget")), ("contents", link |> Link.encode)})
    | MouseOver(link) =>
      object_(list{("tag", string("MouseOver")), ("contents", link |> Link.encode)})
    | MouseOut(link) =>
      object_(list{("tag", string("MouseOut")), ("contents", link |> Link.encode)})
    }
}

module ResponseOrEventFromView = {
  type t =
    | Response(Response.t)
    | Event(EventFromView.t)

  // JSON encode/decode

  open Json.Decode
  open Util.Decode

  let decode: decoder<t> = sum(x =>
    switch x {
    | "Response" => Contents(Response.decode |> map(x => Response(x)))
    | "Event" => Contents(EventFromView.decode |> map(x => Event(x)))
    | tag => raise(DecodeError("[ResponseOrEventFromView] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Response(payload) =>
      object_(list{("tag", string("Response")), ("contents", payload |> Response.encode)})
    | Event(payload) =>
      object_(list{("tag", string("Event")), ("contents", payload |> EventFromView.encode)})
    }
}
