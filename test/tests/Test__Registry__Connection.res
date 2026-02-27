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

let makeDestroyThrowingConnection = (): Connection.t => {
  %raw(`{
    TAG: "Agda",
    _0: {
      // chan: null causes Chan.destroy to throw (calls null.removeAllListeners())
      // which exercises the terminate path where Connection.destroy throws
      chan: null,
      process: { status: "Destroyed" },
      encountedFirstPrompt: false,
      version: "2.6.4",
      path: "mock-path-broken-destroy"
    },
    _1: "mock-path-broken-destroy",
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

  Async.it("execute should recover queue after a task throws", async () => {
    await setup()
    let dummyConnection = makeDummyConnection()
    let make = async () => Ok(dummyConnection)
    let _ = await Registry__Connection.acquire("owner1", make)

    // First request must throw/reject; assertion focuses on queue recovery
    let _ = switch await Registry__Connection.execute("owner1", async _ => raise(Failure("boom"))) {
    | exception _ => ()
    | _ => ()
    }

    // Second request should still complete if queue cleanup is correct
    let secondResult = ref(None)
    let completion =
      Registry__Connection.execute("owner2", async _ => Ok("recovered"))
      ->Promise.thenResolve(result => {
        secondResult := Some(result)
        "done"
      })

    let timeout = Util.Promise_.setTimeout(250)->Promise.thenResolve(_ => "timeout")
    let winner = await Promise.race([completion, timeout])

    switch winner {
    | "done" => Assert.deepStrictEqual(secondResult.contents, Some(Ok("recovered")))
    | "timeout" => Assert.fail("Queue remains blocked after a thrown task")
    | _ => Assert.fail("Unexpected race result")
    }
  })

  Async.it("acquire should recover after make throws", async () => {
    await setup()
    let dummyConnection = makeDummyConnection()

    // First acquire must throw/reject
    let _ = switch await Registry__Connection.acquire("owner1", async () => raise(Failure("connect-boom"))) {
    | exception _ => ()
    | _ => ()
    }

    // Second acquire should still complete if Connecting state is properly recovered
    let secondResult = ref(None)
    let completion =
      Registry__Connection.acquire("owner2", async () => Ok(dummyConnection))
      ->Promise.thenResolve(result => {
        secondResult := Some(result)
        "done"
      })
    let timeout = Util.Promise_.setTimeout(250)->Promise.thenResolve(_ => "timeout")
    let winner = await Promise.race([completion, timeout])

    switch winner {
    | "done" =>
      Registry__Connection.status := Empty
      Assert.deepStrictEqual(secondResult.contents, Some(Ok(dummyConnection)))
    | "timeout" =>
      // Manually reset to Empty: the timed-out acquire coroutine is leaked
      // (stuck on a never-resolving promise) but it will not mutate state or unblock,
      // so this is safe for test isolation.
      Registry__Connection.status := Empty
      Assert.fail("Acquire remains blocked after make throws")
    | _ =>
      Registry__Connection.status := Empty
      Assert.fail("Unexpected race result")
    }
  })

  Async.it("shutdown should not hang after make throws", async () => {
    await setup()

    // First acquire must throw/reject
    let _ = switch await Registry__Connection.acquire("owner1", async () => raise(Failure("connect-boom"))) {
    | exception _ => ()
    | _ => ()
    }

    let completion = Registry__Connection.shutdown()->Promise.thenResolve(_ => "done")
    let timeout = Util.Promise_.setTimeout(250)->Promise.thenResolve(_ => "timeout")
    let winner = await Promise.race([completion, timeout])

    switch winner {
    | "done" =>
      Registry__Connection.status := Empty
      Assert.ok(true)
    | "timeout" =>
      // Manually reset to Empty: the timed-out shutdown coroutine is leaked
      // (stuck on a never-resolving promise) but it will not mutate state or unblock,
      // so this is safe for test isolation.
      Registry__Connection.status := Empty
      Assert.fail("Shutdown hangs when Connecting is wedged")
    | _ =>
      Registry__Connection.status := Empty
      Assert.fail("Unexpected race result")
    }
  })

  Async.it("acquire should not block after destroy throws during terminate", async () => {
    await setup()
    let brokenConnection = makeDestroyThrowingConnection()
    let dummyConnection = makeDummyConnection()

    let _ = await Registry__Connection.acquire("owner1", async () => Ok(brokenConnection))

    // Last release triggers terminate, which currently wedges in Closing when destroy throws
    let _ = switch await Registry__Connection.release("owner1") {
    | exception _ => ()
    | _ => ()
    }

    let secondResult = ref(None)
    let completion =
      Registry__Connection.acquire("owner2", async () => Ok(dummyConnection))
      ->Promise.thenResolve(result => {
        secondResult := Some(result)
        "done"
      })
    let timeout = Util.Promise_.setTimeout(250)->Promise.thenResolve(_ => "timeout")
    let winner = await Promise.race([completion, timeout])

    switch winner {
    | "done" =>
      Registry__Connection.status := Empty
      Assert.deepStrictEqual(secondResult.contents, Some(Ok(dummyConnection)))
    | "timeout" =>
      // Manually reset to Empty: the timed-out acquire coroutine is leaked
      // (stuck on a never-resolving promise) but it will not mutate state or unblock,
      // so this is safe for test isolation.
      Registry__Connection.status := Empty
      Assert.fail("Acquire blocks after destroy throws during terminate")
    | _ =>
      Registry__Connection.status := Empty
      Assert.fail("Unexpected race result")
    }
  })

  Async.it("shutdown should not block after destroy throws during terminate", async () => {
    await setup()
    let brokenConnection = makeDestroyThrowingConnection()

    let _ = await Registry__Connection.acquire("owner1", async () => Ok(brokenConnection))

    // Last release triggers terminate, which currently wedges in Closing when destroy throws
    let _ = switch await Registry__Connection.release("owner1") {
    | exception _ => ()
    | _ => ()
    }

    let completion = Registry__Connection.shutdown()->Promise.thenResolve(_ => "done")
    let timeout = Util.Promise_.setTimeout(250)->Promise.thenResolve(_ => "timeout")
    let winner = await Promise.race([completion, timeout])

    switch winner {
    | "done" =>
      Registry__Connection.status := Empty
      Assert.ok(true)
    | "timeout" =>
      // Manually reset to Empty: the timed-out shutdown coroutine is leaked
      // (stuck on a never-resolving promise) but it will not mutate state or unblock,
      // so this is safe for test isolation.
      Registry__Connection.status := Empty
      Assert.fail("Shutdown blocks after destroy throws during terminate")
    | _ =>
      Registry__Connection.status := Empty
      Assert.fail("Unexpected race result")
    }
  })

  Async.it("release during connecting should not leave a phantom user", async () => {
    await setup()
    let dummyConnection = makeDummyConnection()
    let (connectionResult, resolveConnection, _) = Util.Promise_.pending()

    let acquireOwner1 = Registry__Connection.acquire("owner1", () => connectionResult)
    // Wait one tick so owner1 is definitely in the Connecting path.
    await Util.Promise_.setTimeout(0)

    // owner1 closes before the connection finishes
    await Registry__Connection.release("owner1")

    resolveConnection(Ok(dummyConnection))
    let _ = await acquireOwner1

    // owner2 can join and leave; if owner1 was properly released, registry should end empty
    let _ = await Registry__Connection.acquire("owner2", async () => Ok(dummyConnection))
    await Registry__Connection.release("owner2")

    let view = Registry__Connection.inspect()
    Assert.deepStrictEqual(view.status, "Empty")
    Assert.deepStrictEqual(view.userCount, 0)
  })

  Async.it("queued execute should not run after registry leaves active state", async () => {
    await setup()
    let dummyConnection = makeDummyConnection()
    let _ = await Registry__Connection.acquire("owner1", async () => Ok(dummyConnection))

    let (firstTaskCanFinish, resolveFirstTask, _) = Util.Promise_.pending()

    let first =
      Registry__Connection.execute("owner1", async _ => {
        let _ = await firstTaskCanFinish
        Ok("first-done")
      })

    // Allow the first task to acquire execution ownership before queueing the second.
    await Util.Promise_.setTimeout(0)

    let secondTaskRan = ref(false)
    let second =
      Registry__Connection.execute("owner2", async _ => {
        secondTaskRan := true
        Ok("second-ran")
      })

    // Allow the second task to enter the serialized queue and block on previousTaskDone.
    await Util.Promise_.setTimeout(0)

    // Simulate teardown while queued work is waiting its turn.
    Registry__Connection.status := Empty

    resolveFirstTask()
    let _ = await first
    let secondResult = await second

    Assert.deepStrictEqual(secondTaskRan.contents, false)
    switch secondResult {
    | Error(_) => Assert.ok(true)
    | Ok(_) => Assert.fail("Queued task ran even though registry was no longer active")
    }
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
