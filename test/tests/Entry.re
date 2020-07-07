open BsMocha;
let (it', it_skip') = Async.(it, it_skip);
open Mocha;

describe("Some Test Suite", () => {
  describe("List.map", () => {
    it("should map the values", () =>
      Assert.deep_equal(List.map(( * )(2), [1, 2, 3]), [2, 4, 6])
    );

    it("should work with an empty list", () =>
      Assert.equal(List.map(( * )(2), []), [])
    );
  })
});
