open Mocha
open Test__Util

let setup = async () => {
  // Reset the singleton status for testing
  await Registry__Connection.shutdown()
  // Give the OS a moment to clean up process table
  await Util.Promise_.setTimeout(500)
}

let makeDummyConnection = (): Connection.t => {
  %raw(`{
    TAG: "Agda",
    _0: {
      chan: { removeAllListeners: () => {} },
      process: { status: "Destroyed" },
      encountedFirstPrompt: false,
      version: "2.6.4",
      path: "mock-path"
    },
    _1: "mock-path",
    _2: "2.6.4"
  }`)
}

describe("Registry__Connection", () => {
  Async.it("Singleton: acquire returns the same connection for different owners", async () => {
    await setup()
    let makeCalled = ref(0)
    let dummyConnection = makeDummyConnection()

    let make = async () => {
      makeCalled := makeCalled.contents + 1
      Ok(dummyConnection)
    }

    // Proving that two owners share the same process
    let res1 = await Registry__Connection.acquire("owner1", make)
    let res2 = await Registry__Connection.acquire("owner2", make)

    Assert.deepStrictEqual(res1, Ok(dummyConnection))
    Assert.deepStrictEqual(res2, Ok(dummyConnection))
    Assert.deepStrictEqual(makeCalled.contents, 1)

    let view = Registry__Connection.inspect()
    Assert.deepStrictEqual(view.userCount, 2)
    Assert.deepStrictEqual(view.status, "Active")
  })

  Async.it("Serialization: concurrent execution is queued", async () => {
    await setup()
    let dummyConnection = makeDummyConnection()
    let make = async () => Ok(dummyConnection)
    let _ = await Registry__Connection.acquire("owner1", make)

    let executionOrder = []
    let task1 = async _ => {
      executionOrder->Array.push("task1-start")
      await Util.Promise_.setTimeout(50)
      executionOrder->Array.push("task1-end")
      Ok()
    }
    let task2 = async _ => {
      executionOrder->Array.push("task2-start")
      Ok()
    }

    // Proving that if we call execute twice, the second waits for the first
    let p1 = Registry__Connection.execute("owner1", task1)
    let p2 = Registry__Connection.execute("owner2", task2)

    let _ = await p1
    let _ = await p2

    // Verify that task2 started after task1 ended
    Assert.deepStrictEqual(executionOrder, ["task1-start", "task1-end", "task2-start"])
  })

  Async.it("Reentrancy: nested execution from same owner is allowed", async () => {
    await setup()
    let dummyConnection = makeDummyConnection()
    let make = async () => Ok(dummyConnection)
    let _ = await Registry__Connection.acquire("owner1", make)

    let executionOrder = []
    let task = async _ => {
      executionOrder->Array.push("outer-start")
      // Nested call from same owner
      let _ = await Registry__Connection.execute("owner1", async _ => {
        executionOrder->Array.push("inner")
        Ok()
      })
      executionOrder->Array.push("outer-end")
      Ok()
    }

    // Proving that an owner already holding the lock can execute again without blocking
    let _ = await Registry__Connection.execute("owner1", task)

    Assert.deepStrictEqual(executionOrder, ["outer-start", "inner", "outer-end"])
  })

  Async.it("Reference Counting: connection is destroyed only when all users release it", async () => {
    await setup()
    let dummyConnection = makeDummyConnection()
    let make = async () => Ok(dummyConnection)

    let _ = await Registry__Connection.acquire("owner1", make)
    let _ = await Registry__Connection.acquire("owner2", make)

    Assert.deepStrictEqual(Registry__Connection.inspect().userCount, 2)

    // Proving that status moves to Empty only after last user releases
    await Registry__Connection.release("owner1")
    Assert.deepStrictEqual(Registry__Connection.inspect().userCount, 1)
    Assert.deepStrictEqual(Registry__Connection.inspect().status, "Active")

    await Registry__Connection.release("owner2")
    Assert.deepStrictEqual(Registry__Connection.inspect().status, "Empty")
  })

  describe("Real-world Scenario", () => {
    This.timeout(30000)

    Async.it("should share a single connection mechanism between two different Agda files", async () => {
      await setup()
      
      // Load first file
      let agdaA = await AgdaMode.makeAndLoad("GotoDefinition.agda")
      
      // VERIFY Internal: Registry should show 1 process was made
      let view1 = Registry__Connection.inspect()
      Assert.deepStrictEqual(view1.makeCount, 1)
      Assert.deepStrictEqual(view1.userCount, 1)
      
      // Load second file
      let agdaB = await AgdaMode.makeAndLoad("Lib.agda")
      
      // VERIFY Internal: makeCount stays at 1 (Internal Proof of sharing mechanism)
      let view2 = Registry__Connection.inspect()
      Assert.deepStrictEqual(view2.makeCount, 1)
      Assert.deepStrictEqual(view2.userCount, 2)
      Assert.deepStrictEqual(view2.status, "Active")
      
      // TODO: Implement a robust, cross-platform external observation (e.g., filtered pgrep)
      // to mathematically prove that exactly 1 OS process was spawned throughout the system,
      // immune to zombie processes from previous test runs.

      // Cleanup
      await AgdaMode.quit(agdaA)
      Assert.deepStrictEqual(Registry__Connection.inspect().userCount, 1)
      
      await AgdaMode.quit(agdaB)
      Assert.deepStrictEqual(Registry__Connection.inspect().status, "Empty")
    })
  })
})
