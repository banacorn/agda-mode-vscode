open Mocha
open Test__Util

describe_skip("Highlighting", () => {
// describe_only("Highlighting", () => {
  describe("updateSemanticHighlighting", () => {
    Async.it(
      "should work",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Issue180.agda")
        Js.log(ctx.state.tokens->Tokens.toString)
        Js.log(ctx.state.highlighting->Highlighting.getSemanticTokens->Array.map(Highlighting__SemanticToken.toString)->Array.join("\n"))

        // let tokens =
        //   ctx.state.tokens
        //   ->Tokens.toArray
        //   ->Array.map(
        //     ((token, range)) => {
        //       Js.log(token->Tokens.Token.toString)
        //       Editor.Range.toString(range) ++ " " ++ Tokens.Token.toString(token)
        //     },
        //   )
        // Assert.deepEqual(Array.length(tokens), 28)

        // let (decorations, semanticTokens) = Tokens.toDecorationsAndSemanticTokens(ctx.state.tokens, ctx.state.editor)
        
        // let highlighting = Highlighting.make()
        // await Highlighting.apply(highlighting, ctx.state.tokens, ctx.state.editor)

        // let (decorations, semanticTokens) = Tokens.toDecorationsAndSemanticTokens(ctx.state.tokens, ctx.state.editor)

        // Highlighting.applyChangeToSemanticTokens(semanticTokens, ctx.state.editor.document)

        // Js.log(decorations->Array.map(fst)->Array.map(Editor.Decoration.toString)->Array.join("\n"))
        // Js.log(ctx.state.highlighting->Highlighting.toString)
        // Js.log(semanticTokens->Array.map(Highlighting__SemanticToken.toString)->Array.join("\n"))
        ()
      },
    )
  })
})
