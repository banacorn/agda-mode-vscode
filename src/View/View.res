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

  let decode = {
    open JsonCombinators.Json.Decode
    Util.Decode.sum(x => {
      switch x {
      | "Plain" => Payload(string->map(text => Plain(text)))
      | "Success" => Payload(string->map(text => Success(text)))
      | "Warning" => Payload(string->map(text => Warning(text)))
      | "Error" => Payload(string->map(text => Error(text)))
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
    , ...)
  }
}

module Prompt = {
  type t = {
    body: option<string>,
    placeholder: option<string>,
    value: option<string>,
  }

  // JSON encode/decode
  let decode = {
    // TODO: replace `field.required(. "body", option(string))` with `field.optional(. "body", string)`
    open JsonCombinators.Json.Decode
    object(field => {
      body: field.required("body", option(string)),
      placeholder: field.required("placeholder", option(string)),
      value: field.required("value", option(string)),
    })
  }

  let encode = ({body, placeholder, value}) => {
    open JsonCombinators.Json.Encode
    Unsafe.object({
      "body": option(string)(body),
      "placeholder": option(string)(placeholder),
      "value": option(string)(value),
    })
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
      Util.Decode.sum(x => {
        switch x {
        | "Activate" => TagOnly(Activate)
        | "Deactivate" => TagOnly(Deactivate)
        | "Update" =>
          Payload(
            tuple3(string, Translator.decode, int)->map(((sequence, translation, index)) => Update(
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
      })
    }

    let encode = {
      open JsonCombinators.Json.Encode
      Util.Encode.sum(x =>
        switch x {
        | Activate => TagOnly("Activate")
        | Deactivate => TagOnly("Deactivate")
        | Update(sequence, translation, index) =>
          Payload("Update", tuple3(string, Translator.encode, int)((sequence, translation, index)))
        | BrowseUp => TagOnly("BrowseUp")
        | BrowseRight => TagOnly("BrowseRight")
        | BrowseDown => TagOnly("BrowseDown")
        | BrowseLeft => TagOnly("BrowseLeft")
        }
      , ...)
    }
  }

  type t =
    | Display(Header.t, Body.t)
    | Append(Header.t, Body.t) // append instead of flushing the old Body.t
    | SetConnectionStatus(string)
    | PromptInterrupt
    | PromptIMUpdate(string)
    | InputMethod(InputMethod.t)
    | ConfigurationChange(string)

  let toString = x =>
    switch x {
    | Display(header, _body) => "Display " ++ Header.toString(header)
    | Append(header, _body) => "Append " ++ Header.toString(header)
    | SetConnectionStatus(status) => "SetConnectionStatus " ++ status
    | PromptInterrupt => "PromptInterrupt"
    | PromptIMUpdate(s) => "PromptIMUpdate " ++ s
    | InputMethod(_) => "InputMethod"
    | ConfigurationChange(_) => "ConfigurationChange"
    }

  let decode = {
    open JsonCombinators.Json.Decode
    Util.Decode.sum(x => {
      switch x {
      | "Display" =>
        Payload(
          tuple2(Header.decode, array(Item.decode))->map(((header, body)) => Display(header, body)),
        )
      | "Append" =>
        Payload(
          tuple2(Header.decode, array(Item.decode))->map(((header, body)) => Append(header, body)),
        )
      | "SetConnectionStatus" => Payload(string->map(text => SetConnectionStatus(text)))
      | "PromptInterrupt" => TagOnly(PromptInterrupt)
      | "PromptIMUpdate" => Payload(string->map(text => PromptIMUpdate(text)))
      | "InputMethod" => Payload(InputMethod.decode->map(payload => InputMethod(payload)))
      | "ConfigurationChange" => Payload(string->map(text => ConfigurationChange(text)))
      | tag => raise(DecodeError("[EventToView] Unknown constructor: " ++ tag))
      }
    })
  }

  let encode = {
    open JsonCombinators.Json.Encode
    Util.Encode.sum(x =>
      switch x {
      | Display(header, body) =>
        Payload("Display", tuple2(Header.encode, array(Item.encode))((header, body)))
      | Append(header, body) =>
        Payload("Append", tuple2(Header.encode, array(Item.encode))((header, body)))
      | SetConnectionStatus(text) => Payload("SetConnectionStatus", string(text))
      | PromptInterrupt => TagOnly("PromptInterrupt")
      | PromptIMUpdate(text) => Payload("PromptIMUpdate", string(text))
      | InputMethod(payload) => Payload("InputMethod", InputMethod.encode(payload))
      | ConfigurationChange(text) => Payload("ConfigurationChange", string(text))
      }
    , ...)
  }
}

module Request = {
  type t = Prompt(Header.t, Prompt.t)

  let toString = x =>
    switch x {
    | Prompt(header, _) => "Prompt " ++ Header.toString(header)
    }

  // JSON encode/decode
  let decode = {
    open JsonCombinators.Json.Decode
    tuple2(Header.decode, Prompt.decode)->map(((header, prompt)) => Prompt(header, prompt))
  }

  let encode = request => {
    open JsonCombinators.Json.Encode
    switch request {
    | Prompt(header, prompt) => tuple2(Header.encode, Prompt.encode)((header, prompt))
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
  let decode = {
    open JsonCombinators.Json.Decode
    Util.Decode.sum(x => {
      switch x {
      | "Request" => Payload(Request.decode->map(payload => Request(payload)))
      | "Event" => Payload(EventToView.decode->map(payload => Event(payload)))
      | tag => raise(DecodeError("[RequestOrEventToView] Unknown constructor: " ++ tag))
      }
    })
  }

  let encode = {
    Util.Encode.sum(x =>
      switch x {
      | Request(payload) => Payload("Request", Request.encode(payload))
      | Event(payload) => Payload("Event", EventToView.encode(payload))
      }
    , ...)
  }
}

module Response = {
  type t =
    | PromptSuccess(string)
    | PromptInterrupted

  let decode = {
    open JsonCombinators.Json.Decode
    Util.Decode.sum(x => {
      switch x {
      | "PromptSuccess" => Payload(string->map(result => PromptSuccess(result)))
      | "PromptInterrupted" => TagOnly(PromptInterrupted)
      | tag => raise(DecodeError("[Response] Unknown constructor: " ++ tag))
      }
    })
  }

  let encode = {
    open JsonCombinators.Json.Encode
    Util.Encode.sum(x =>
      switch x {
      | PromptSuccess(result) => Payload("PromptSuccess", string(result))
      | PromptInterrupted => TagOnly("PromptInterrupted")
      }
    , ...)
  }
}

module EventFromView = {
  module InputMethod = {
    type t =
      | InsertChar(string)
      | ChooseSymbol(string)

    let decode = {
      open JsonCombinators.Json.Decode
      Util.Decode.sum(x => {
        switch x {
        | "InsertChar" => Payload(string->map(char => InsertChar(char)))
        | "ChooseSymbol" => Payload(string->map(symbol => ChooseSymbol(symbol)))
        | tag => raise(DecodeError("[EventFromView.InputMethod] Unknown constructor: " ++ tag))
        }
      })
    }

    let encode = {
      open JsonCombinators.Json.Encode
      Util.Encode.sum(x =>
        switch x {
        | InsertChar(char) => Payload("InsertChar", string(char))
        | ChooseSymbol(symbol) => Payload("ChooseSymbol", string(symbol))
        }
      , ...)
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

    let decode = {
      open JsonCombinators.Json.Decode
      Util.Decode.sum(x => {
        switch x {
        | "MouseSelect" => Payload(Interval.decode->map(interval => MouseSelect(interval)))
        | "KeyUpdate" => Payload(string->map(char => KeyUpdate(char)))
        | "BrowseUp" => TagOnly(BrowseUp)
        | "BrowseDown" => TagOnly(BrowseDown)
        | "BrowseLeft" => TagOnly(BrowseLeft)
        | "BrowseRight" => TagOnly(BrowseRight)
        | "Escape" => TagOnly(Escape)
        | tag => raise(DecodeError("[EventFromView.PromptIMUpdate] Unknown constructor: " ++ tag))
        }
      })
    }

    let encode = {
      open JsonCombinators.Json.Encode
      Util.Encode.sum(x =>
        switch x {
        | MouseSelect(interval) => Payload("MouseSelect", Interval.encode(interval))
        | KeyUpdate(char) => Payload("KeyUpdate", string(char))
        | BrowseUp => TagOnly("BrowseUp")
        | BrowseDown => TagOnly("BrowseDown")
        | BrowseLeft => TagOnly("BrowseLeft")
        | BrowseRight => TagOnly("BrowseRight")
        | Escape => TagOnly("Escape")
        }
      , ...)
    }
  }

  type t =
    | Initialized
    | Destroyed
    | InputMethod(InputMethod.t)
    | PromptIMUpdate(PromptIMUpdate.t)
    | JumpToTarget(Link.t)
    | ConnectionStatusClicked

  let toString = x =>
    switch x {
    | Initialized => "Initialized"
    | Destroyed => "Destroyed"
    | InputMethod(_) => "InputMethod"
    | PromptIMUpdate(_) => "PromptIMUpdate"
    | JumpToTarget(_) => "JumpToTarget"
    | ConnectionStatusClicked => "ConnectionStatusClicked"
    }

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

  let decode = {
    open JsonCombinators.Json.Decode
    Util.Decode.sum(x => {
      switch x {
      | "Initialized" => TagOnly(Initialized)
      | "Destroyed" => TagOnly(Destroyed)
      | "InputMethod" => Payload(InputMethod.decode->map(payload => InputMethod(payload)))
      | "PromptIMUpdate" => Payload(PromptIMUpdate.decode->map(payload => PromptIMUpdate(payload)))
      | "JumpToTarget" => Payload(Link.decode->map(link => JumpToTarget(link)))
      | "ConnectionStatusClicked" => TagOnly(ConnectionStatusClicked)
      | tag => raise(DecodeError("[EventFromView] Unknown constructor: " ++ tag))
      }
    })
  }

  let encode = {
    Util.Encode.sum(x =>
      switch x {
      | Initialized => TagOnly("Initialized")
      | Destroyed => TagOnly("Destroyed")
      | InputMethod(action) => Payload("InputMethod", InputMethod.encode(action))
      | PromptIMUpdate(action) => Payload("PromptIMUpdate", PromptIMUpdate.encode(action))
      | JumpToTarget(link) => Payload("JumpToTarget", Link.encode(link))
      | ConnectionStatusClicked => TagOnly("ConnectionStatusClicked")
      }
    , ...)
  }
}

module ResponseOrEventFromView = {
  type t =
    | Response(Response.t)
    | Event(EventFromView.t)

  // JSON encode/decode
  let decode = {
    open JsonCombinators.Json.Decode
    Util.Decode.sum(x => {
      switch x {
      | "Response" => Payload(Response.decode->map(payload => Response(payload)))
      | "Event" => Payload(EventFromView.decode->map(payload => Event(payload)))
      | tag => raise(DecodeError("[ResponseOrEventFromView] Unknown constructor: " ++ tag))
      }
    })
  }

  let encode = Util.Encode.sum(x =>
    switch x {
    | Response(payload) => Payload("Response", Response.encode(payload))
    | Event(payload) => Payload("Event", EventFromView.encode(payload))
    }
  )
}
