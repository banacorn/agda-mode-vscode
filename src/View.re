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

  type t =
    | Show
    | Hide
    | Plain(Header.t, option(string));

  open Json.Decode;
  open Util.Decode;

  let decode: decoder(t) =
    sum(
      fun
      | "Show" => TagOnly(_ => Show)
      | "Hide" => TagOnly(_ => Hide)
      | "Plain" =>
        Contents(
          pair(Header.decode, optional(string))
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
        (
          "contents",
          (header, body) |> pair(Header.encode, nullable(string)),
        ),
      ]);
};

module Response = {
  type t =
    | Initialized
    | Destroyed;

  open Json.Decode;
  open Util.Decode;

  let decode: decoder(t) =
    sum(
      fun
      | "Initialized" => TagOnly(_ => Initialized)
      | "Destroyed" => TagOnly(_ => Destroyed)
      | tag => raise(DecodeError("[Response] Unknown constructor: " ++ tag)),
    );

  open! Json.Encode;
  let encode: encoder(t) =
    fun
    | Initialized => object_([("tag", string("Initialized"))])
    | Destroyed => object_([("tag", string("Destroyed"))]);
};