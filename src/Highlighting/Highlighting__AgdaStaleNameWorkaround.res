// WORKAROUND for an Agda bug: `Cmd_highlight` after Give or Refine highlights names at stale ranges.
//
// Reported upstream as https://github.com/agda/agda/issues/8858
// This module exists only because of that bug. Remove it once Agda stops sending the stale names.
//
// ## The bug
//
// After Give or Refine fills a goal, we send `Cmd_highlight` for the filled text (issue #77).
// Agda replies with the right highlighting, plus a second copy of some names at the ranges
// they had in the text that Give or Refine type-checked, which is no longer where they are.
//
// Traced in the Agda source at da66a8c75f11d10699a6b38b261efdf244b66f2a (also in 2.6.4.3, 2.7.0.1, 2.8.0):
//
//   1. Give and Refine parse their text at the range of the goal
//      (`give_gen` calls `B.parseExprIn ii rng s`, Agda/Interaction/InteractionTop.hs#L1028),
//      so the names in it get ranges inside the old goal.
//   2. Type-checking stores every record field, ambiguous constructor and ambiguous projection
//      in `stDisambiguatedNames`, keyed by the start offset of its range
//      (`disambiguateRecordFields`, Agda/TypeChecking/Rules/Term.hs#L1152,
//      and `storeDisambiguatedName`, Agda/Interaction/Highlighting/Generate.hs#L799-L814).
//      Nothing ever removes these entries.
//   3. `Cmd_highlight` runs `highlightExpr` in the real state, not under `freshTCM`
//      (Agda/Interaction/InteractionTop.hs#L623-L636 and #L1089-L1098),
//      which at the `Full` level calls `generateConstructorInfo`
//      (Agda/Interaction/Highlighting/Generate.hs#L156-L157).
//   4. `generateConstructorInfo` emits every entry of `stDisambiguatedNames` that starts inside
//      the highlighted range, at the range stored in step 2
//      (Agda/Interaction/Highlighting/Generate.hs#L343-L361).
//
// The stale range is not a constant shift from the real one: we indent the filled text and
// expand `?` into `{!   !}`, but Agda's text has neither.
//
// ## Why we drop every Field and Constructor token
//
// Agda merges the stale copies with the real highlighting before it replies
// (`constructorInfo <> nameInfo` in `generateAndPrintSyntaxInfo`), so the reply cannot be
// filtered token by token:
//
//   - a real name comes back split into pieces where a stale copy overlaps it;
//   - a stale copy can cover a whole identifier or symbol exactly (`r`, `=`, `}`, `(`);
//   - the merge keeps the stale kind, so a real name that a stale copy overlaps comes back as a
//     Field or a Constructor, whatever it really is (`x` in `con x y`, `r` in `foo r`).
//
// Stale copies are always Fields (projections included) or Constructors, so we drop every token
// with one of these kinds, `operator` flag and definition site included.
//
// ## What this costs
//
//   - Real names that a stale copy overlaps lose their highlighting, whatever their kind.
//   - Unambiguous constructors (`suc`) and qualified names (`A.con`, `R.foo`) have no stale copies,
//     but lose their highlighting too: nothing in the reply tells them apart from stale copies.
//
// All of these come back with the next Load.

module Aspect = Highlighting__AgdaAspect

let isStaleKind = aspect =>
  switch aspect {
  | Aspect.Field | ConstructorInductive | ConstructorCoInductive => true
  | _ => false
  }

// drops every token with a Field or Constructor aspect from a `Cmd_highlight` reply
let filter = (tokens: array<Token.t<'a>>): array<Token.t<'a>> =>
  tokens->Array.filter(token => !(token.aspects->Array.some(isStaleKind)))
