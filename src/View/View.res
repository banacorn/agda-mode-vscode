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

  // let decode: Json.Decode.decoder<t> = Util.Decode.sum(x => {
  //   open Json.Decode
  //   switch x {
  //   | "Plain" => Contents(string |> map(text => Plain(text)))
  //   | "Success" => Contents(string |> map(text => Success(text)))
  //   | "Warning" => Contents(string |> map(text => Warning(text)))
  //   | "Error" => Contents(string |> map(text => Error(text)))
  //   | tag => raise(DecodeError("[Header] Unknown constructor: " ++ tag))
  //   }
  // })

  let decode = {
    open JsonCombinators.Json.Decode
    Util.Decode.sum_(x => {
      switch x {
      | "Plain" => Payload(string->map((. text) => Plain(text)))
      | "Success" => Payload(string->map((. text) => Success(text)))
      | "Warning" => Payload(string->map((. text) => Warning(text)))
      | "Error" => Payload(string->map((. text) => Error(text)))
      | tag => raise(DecodeError("[Header] Unknown constructor: " ++ tag))
      }
    })
  }

  let encode = {
    open JsonCombinators.Json.Encode
    Util.Encode.sum(x =>
      switch x {
      | Plain(text) => Payload("Plain", string(text))
      | Success(text) => Payload("Success", string(text))
      | Warning(text) => Payload("Warning", string(text))
      | Error(text) => Payload("Error", string(text))
      }
    )
  }
}

module Prompt = {
  type t = {
    body: option<string>,
    placeholder: option<string>,
    value: option<string>,
  }

  // JSON encode/decode
  let decode: Json.Decode.decoder<t> = Util.Decode.sum(x => {
    open Json.Decode
    switch x {
    | "Prompt" =>
      Contents(
        tuple3(optional(string), optional(string), optional(string)) |> map(((
          body,
          placeholder,
          value,
        )) => {
          body,
          placeholder,
          value,
        }),
      )
    | tag => raise(DecodeError("[Prompt] Unknown constructor: " ++ tag))
    }
  })

  let encode: Json.Encode.encoder<t> = x => {
    open Json.Encode
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
}
module Body = {
  type t = array<Item.t>
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

    let decode = {
      open JsonCombinators.Json.Decode
      Util.Decode.sum_(x => {
        switch x {
        | "Activate" => TagOnly(Activate)
        | "Deactivate" => TagOnly(Deactivate)
        | "Update" =>
          Payload(
            tuple3(string, Translator.decode, int)->map((. (
              sequence,
              translation,
              index,
            )) => Update(sequence, translation, index)),
          )
        | "BrowseUp" => TagOnly(BrowseUp)
        | "BrowseRight" => TagOnly(BrowseRight)
        | "BrowseDown" => TagOnly(BrowseDown)
        | "BrowseLeft" => TagOnly(BrowseLeft)
        | tag => raise(DecodeError("[EventToView.InputMethod] Unknown constructor: " ++ tag))
        }
      })
    }

    let encode = {
      open JsonCombinators.Json.Encode
      Util.Encode.sum(x =>
        switch x {
        | Activate => TagOnly("Activate")
        | Deactivate => TagOnly("Deactivate")
        | Update(sequence, translation, index) =>
          Payload("Update", tuple3(string, Translator.encode, int, (sequence, translation, index)))
        | BrowseUp => TagOnly("BrowseUp")
        | BrowseRight => TagOnly("BrowseRight")
        | BrowseDown => TagOnly("BrowseDown")
        | BrowseLeft => TagOnly("BrowseLeft")
        }
      )
    }
  }

  type t =
    | Display(Header.t, Body.t)
    | Append(Header.t, Body.t) // append instead of flushing the old Body.t
    | SetStatus(string)
    | PromptInterrupt
    | PromptIMUpdate(string)
    | InputMethod(InputMethod.t)

  let toString = x =>
    switch x {
    | Display(header, _body) => "Display " ++ Header.toString(header)
    | Append(header, _body) => "Append " ++ Header.toString(header)
    | SetStatus(status) => "SetStatus " ++ status
    | PromptInterrupt => "PromptInterrupt"
    | PromptIMUpdate(s) => "PromptIMUpdate " ++ s
    | InputMethod(_) => "InputMethod"
    }

  // JSON encode/decode
  let decode: Json.Decode.decoder<t> = Util.Decode.sum(x => {
    open Json.Decode
    switch x {
    | "Display" =>
      Contents(
        pair(Util.Decode.convert(Header.decode), array(Item.decode)) |> map(((
          header,
          body,
        )) => Display(header, body)),
      )
    | "Append" =>
      Contents(
        pair(Util.Decode.convert(Header.decode), array(Item.decode)) |> map(((
          header,
          body,
        )) => Append(header, body)),
      )
    | "SetStatus" => Contents(string |> map(text => SetStatus(text)))
    | "PromptInterrupt" => TagOnly(PromptInterrupt)
    | "PromptIMUpdate" => Contents(string |> map(text => PromptIMUpdate(text)))
    | "InputMethod" => Contents(Util.Decode.convert(InputMethod.decode) |> map(x => InputMethod(x)))
    | tag => raise(DecodeError("[EventToView] Unknown constructor: " ++ tag))
    }
  })

  let encode = {
    open JsonCombinators.Json.Encode
    Util.Encode.sum(x =>
      switch x {
      | Display(header, body) =>
        Payload("Display", pair(Header.encode, array(Item.encode), (header, body)))
      | Append(header, body) =>
        Payload("Append", pair(Header.encode, array(Item.encode), (header, body)))
      | SetStatus(text) => Payload("SetStatus", string(text))
      | PromptInterrupt => TagOnly("PromptInterrupt")
      | PromptIMUpdate(text) => Payload("PromptIMUpdate", string(text))
      | InputMethod(payload) => Payload("InputMethod", InputMethod.encode(payload))
      }
    )
  }
}

module Request = {
  type t = Prompt(Header.t, Prompt.t)

  let toString = x =>
    switch x {
    | Prompt(header, _) => "Prompt " ++ Header.toString(header)
    }

  // JSON encode/decode
  let decode: Json.Decode.decoder<t> = Util.Decode.sum(x => {
    open Json.Decode
    switch x {
    | "Prompt" =>
      Contents(
        pair(Util.Decode.convert(Header.decode), Prompt.decode) |> map(((header, prompt)) => Prompt(
          header,
          prompt,
        )),
      )
    | tag => raise(DecodeError("[Request] Unknown constructor: " ++ tag))
    }
  })

  let encode: Json.Encode.encoder<t> = x => {
    open Json.Encode
    switch x {
    | Prompt(header, prompt) =>
      object_(list{
        ("tag", string("Prompt")),
        ("contents", (header, prompt) |> pair(Header.encode, Prompt.encode)),
      })
    }
  }
}

module RequestOrEventToView = {
  type t =
    | Request(Request.t)
    | Event(EventToView.t)

  let toString = x =>
    switch x {
    | Request(req) => Request.toString(req)
    | Event(ev) => EventToView.toString(ev)
    }

  // JSON encode/decode
  let decode: Json.Decode.decoder<t> = Util.Decode.sum(x => {
    open Json.Decode
    switch x {
    | "Request" => Contents(Request.decode |> map(x => Request(x)))
    | "Event" => Contents(EventToView.decode |> map(x => Event(x)))
    | tag => raise(DecodeError("[RequestOrEventToView] Unknown constructor: " ++ tag))
    }
  })

  let encode: Json.Encode.encoder<t> = x => {
    open Json.Encode
    switch x {
    | Request(payload) =>
      object_(list{("tag", string("Request")), ("contents", payload |> Request.encode)})
    | Event(payload) =>
      object_(list{("tag", string("Event")), ("contents", payload |> EventToView.encode)})
    }
  }
}

module Response = {
  type t =
    | PromptSuccess(string)
    | PromptInterrupted

  let decode: Json.Decode.decoder<t> = Util.Decode.sum(x => {
    open Json.Decode
    switch x {
    | "PromptSuccess" => Contents(string |> map(result => PromptSuccess(result)))
    | "PromptInterrupted" => TagOnly(PromptInterrupted)
    | tag => raise(DecodeError("[Response] Unknown constructor: " ++ tag))
    }
  })

  let encode: Json.Encode.encoder<t> = x => {
    open Json.Encode
    switch x {
    | PromptSuccess(result) =>
      object_(list{("tag", string("PromptSuccess")), ("contents", result |> string)})
    | PromptInterrupted => object_(list{("tag", string("PromptInterrupted"))})
    }
  }
}

module EventFromView = {
  module InputMethod = {
    type t =
      | InsertChar(string)
      | ChooseSymbol(string)

    let decode: Json.Decode.decoder<t> = Util.Decode.sum(x => {
      open Json.Decode
      switch x {
      | "InsertChar" => Contents(string |> map(char => InsertChar(char)))
      | "ChooseSymbol" => Contents(string |> map(char => ChooseSymbol(char)))
      | tag => raise(DecodeError("[EventFromView.InputMethod] Unknown constructor: " ++ tag))
      }
    })

    open! Json.Encode
    let encode: Json.Encode.encoder<t> = x =>
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

    let decode: Json.Decode.decoder<t> = Util.Decode.sum(x =>
      switch x {
      | "MouseSelect" =>
        Contents(Util.Decode.convert(Interval.decode) |> map(interval => MouseSelect(interval)))
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
    let encode: Json.Encode.encoder<t> = x =>
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

  let chan: Chan.t<t> = Chan.make()
  let eventContext = React.createContext(chan)

  module Provider = {
    let makeProps = (~value, ~children, ()) =>
      {
        "value": value,
        "children": children,
      }

    let make = React.Context.provider(eventContext)
  }

  let decode: Json.Decode.decoder<t> = Util.Decode.sum(x => {
    open Json.Decode
    switch x {
    | "Initialized" => TagOnly(Initialized)
    | "Destroyed" => TagOnly(Destroyed)
    | "InputMethod" => Contents(InputMethod.decode |> map(action => InputMethod(action)))
    | "PromptIMUpdate" => Contents(PromptIMUpdate.decode |> map(action => PromptIMUpdate(action)))
    | "JumpToTarget" => Contents(Link.decode |> map(link => JumpToTarget(link)))
    | tag => raise(DecodeError("[EventFromView] Unknown constructor: " ++ tag))
    }
  })

  let encode: Json.Encode.encoder<t> = x => {
    open Json.Encode
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
    }
  }
}

module ResponseOrEventFromView = {
  type t =
    | Response(Response.t)
    | Event(EventFromView.t)

  // JSON encode/decode
  let decode: Json.Decode.decoder<t> = Util.Decode.sum(x => {
    open Json.Decode
    switch x {
    | "Response" => Contents(Response.decode |> map(x => Response(x)))
    | "Event" => Contents(EventFromView.decode |> map(x => Event(x)))
    | tag => raise(DecodeError("[ResponseOrEventFromView] Unknown constructor: " ++ tag))
    }
  })

  let encode: Json.Encode.encoder<t> = x => {
    open Json.Encode
    switch x {
    | Response(payload) =>
      object_(list{("tag", string("Response")), ("contents", payload |> Response.encode)})
    | Event(payload) =>
      object_(list{("tag", string("Event")), ("contents", payload |> EventFromView.encode)})
    }
  }
}
