module Request = {
  module Header = {
    type t =
      | Plain(string)
      | Success(string)
      | Warning(string)
      | Error(string);

    open Json.Decode;
    open Util.Decode;

    let decode: decoder(t) =
      sum(
        fun
        | "Plain" => Contents(string |> map(text => Plain(text)))
        | "Success" => Contents(string |> map(text => Success(text)))
        | "Warning" => Contents(string |> map(text => Warning(text)))
        | "Error" => Contents(string |> map(text => Error(text)))
        | tag =>
          raise(
            DecodeError("[Request.Header] Unknown constructor: " ++ tag),
          ),
      );

    open! Json.Encode;
    let encode: encoder(t) =
      fun
      | Plain(text) =>
        object_([("tag", string("Plain")), ("contents", text |> string)])
      | Success(text) =>
        object_([("tag", string("Success")), ("contents", text |> string)])
      | Warning(text) =>
        object_([("tag", string("Warning")), ("contents", text |> string)])
      | Error(text) =>
        object_([("tag", string("Error")), ("contents", text |> string)]);
  };

  module Body = {
    type t =
      | Nothing
      | Plain(string)
      | Inquire(option(string), option(string));

    open Json.Decode;
    open Util.Decode;

    let decode: decoder(t) =
      sum(
        fun
        | "Nothing" => TagOnly(_ => Nothing)
        | "Plain" => Contents(string |> map(text => Plain(text)))
        | "Inquire" =>
          Contents(
            pair(optional(string), optional(string))
            |> map(((placeholder, value)) => Inquire(placeholder, value)),
          )
        | tag =>
          raise(DecodeError("[Request.Body] Unknown constructor: " ++ tag)),
      );

    open! Json.Encode;
    let encode: encoder(t) =
      fun
      | Nothing => object_([("tag", string("Nothing"))])
      | Plain(text) =>
        object_([("tag", string("Plain")), ("contents", text |> string)])
      | Inquire(placeholder, value) =>
        object_([
          ("tag", string("Inquire")),
          (
            "contents",
            (placeholder, value)
            |> pair(nullable(string), nullable(string)),
          ),
        ]);
  };

  type t =
    | Show
    | Hide
    | Plain(Header.t, Body.t);

  // JSON encode/decode

  open Json.Decode;
  open Util.Decode;

  let decode: decoder(t) =
    sum(
      fun
      | "Show" => TagOnly(_ => Show)
      | "Hide" => TagOnly(_ => Hide)
      | "Plain" =>
        Contents(
          pair(Header.decode, Body.decode)
          |> map(((header, body)) => Plain(header, body)),
        )
      | tag => raise(DecodeError("[Request] Unknown constructor: " ++ tag)),
    );

  open! Json.Encode;
  let encode: encoder(t) =
    fun
    | Show => object_([("tag", string("Show"))])
    | Hide => object_([("tag", string("Hide"))])
    | Plain(header, body) =>
      object_([
        ("tag", string("Plain")),
        ("contents", (header, body) |> pair(Header.encode, Body.encode)),
      ]);
};

module Response = {
  type t =
    | Initialized
    | Destroyed
    | InquiryResult(option(string));

  open Json.Decode;
  open Util.Decode;

  let decode: decoder(t) =
    sum(
      fun
      | "Initialized" => TagOnly(_ => Initialized)
      | "Destroyed" => TagOnly(_ => Destroyed)
      | "InquiryResult" =>
        Contents(optional(string) |> map(result => InquiryResult(result)))
      | tag => raise(DecodeError("[Response] Unknown constructor: " ++ tag)),
    );

  open! Json.Encode;
  let encode: encoder(t) =
    fun
    | Initialized => object_([("tag", string("Initialized"))])
    | Destroyed => object_([("tag", string("Destroyed"))])
    | InquiryResult(result) =>
      object_([
        ("tag", string("InquiryResult")),
        ("contents", result |> nullable(string)),
      ]);
};