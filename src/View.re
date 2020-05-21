module Request = {
  type t =
    | Show
    | Hide
    | Plain(string, string);

  open Json.Decode;
  open Util.Decode;

  let decode: decoder(t) =
    sum(
      fun
      | "Show" => TagOnly(_ => Show)
      | "Hide" => TagOnly(_ => Hide)
      | "Plain" =>
        Contents(
          pair(string, string)
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
        ("contents", (header, body) |> pair(string, string)),
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