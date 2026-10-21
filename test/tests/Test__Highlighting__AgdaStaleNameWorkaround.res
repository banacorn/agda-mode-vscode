open Mocha

module Workaround = Highlighting__AgdaStaleNameWorkaround

let token = (start, end, aspects, source): Token.t<Token.agdaOffset> => {
  start,
  end,
  aspects,
  isTokenBased: false,
  note: None,
  source: source->Option.map(offset => (Parser.Filepath.make("/tmp/Test.agda"), offset)),
}

describe("Highlighting__AgdaStaleNameWorkaround.filter", () => {
  it("should drop Field tokens, with or without a definition site", () => {
    let tokens = [
      token(99, 101, [Field], None),
      token(101, 102, [Field], Some(56)),
      token(102, 104, [Field], Some(56)),
    ]
    Assert.deepStrictEqual(Workaround.filter(tokens), [])
  })

  it("should drop ConstructorInductive and ConstructorCoInductive tokens", () => {
    let tokens = [
      token(130, 132, [ConstructorInductive], None),
      token(132, 133, [ConstructorInductive], Some(54)),
      token(219, 221, [ConstructorCoInductive], None),
      token(221, 223, [ConstructorCoInductive], Some(86)),
    ]
    Assert.deepStrictEqual(Workaround.filter(tokens), [])
  })

  it("should drop a token with the Operator flag as a whole", () => {
    let tokens = [
      token(137, 138, [ConstructorInductive, Operator], Some(60)),
      token(140, 141, [Field, Operator], Some(70)),
    ]
    Assert.deepStrictEqual(Workaround.filter(tokens), [])
  })

  it("should keep the tokens of other kinds next to the dropped ones", () => {
    let tokens = [
      token(140, 143, [ConstructorInductive], None),
      token(144, 145, [Bound], Some(134)),
      token(147, 148, [ConstructorInductive], Some(54)),
      token(148, 149, [Function], Some(40)),
      token(110, 111, [Field], None),
      token(116, 119, [Primitive], Some(1)),
      token(120, 121, [Function, Operator], Some(30)),
    ]
    Assert.deepStrictEqual(
      Workaround.filter(tokens),
      [
        token(144, 145, [Bound], Some(134)),
        token(148, 149, [Function], Some(40)),
        token(116, 119, [Primitive], Some(1)),
        token(120, 121, [Function, Operator], Some(30)),
      ],
    )
  })

  it("should keep a reply with no Field or Constructor tokens unchanged", () => {
    let tokens = [
      token(90, 96, [Keyword], None),
      token(97, 98, [Symbol], None),
      token(105, 106, [Hole], None),
      token(110, 113, [Datatype], Some(20)),
      token(114, 115, [Bound], Some(110)),
    ]
    Assert.deepStrictEqual(Workaround.filter(tokens), tokens)
  })

  it("should keep an empty reply empty", () => {
    Assert.deepStrictEqual(Workaround.filter([]), [])
  })
})
