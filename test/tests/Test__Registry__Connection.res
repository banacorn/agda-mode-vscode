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

// Helpers for the "shutdown reaches Empty from every registry state" cases (#391)

let settleDeadlineMs = 250

type rejection
type settled<'a> = {
  outcome: result<'a, rejection>,
  statusAtSettle: string,
}

@send external onSettled: (promise<'a>, 'a => unit, rejection => unit) => unit = "then"
let makeError: string => 'e = %raw("message => new Error(message)")
// a JS error rethrown through `raise` arrives wrapped as { RE_EXN_ID: "JsError", _1: error }
let errorName: rejection => string = %raw(
  "e => e && (e.RE_EXN_ID === 'JsError' ? e._1.name : e.name)"
)

let tick = () => Util.Promise_.setTimeout(0)

// Apply at creation. The continuation is attached directly to the promise, so
// `statusAtSettle` is the registry status at the moment the promise settles.
let settles = (promise: promise<'a>, label: string): promise<settled<'a>> => {
  let (result, resolve, reject) = Util.Promise_.pending()
  let timer = Js.Global.setTimeout(
    () => reject(makeError("did not settle within 250ms: " ++ label)),
    settleDeadlineMs,
  )
  promise->onSettled(
    value => {
      Js.Global.clearTimeout(timer)
      resolve({outcome: Ok(value), statusAtSettle: Registry__Connection.inspect().status})
    },
    exn => {
      Js.Global.clearTimeout(timer)
      resolve({outcome: Error(exn), statusAtSettle: Registry__Connection.inspect().status})
    },
  )
  // an aborted case must not leave an unhandled deadline rejection behind
  result->Promise.then(_ => Promise.resolve())->Promise.catch(_ => Promise.resolve())->ignore
  result
}

type counted = {
  connection: Connection.t,
  destroyCount: unit => int,
  release: unit => unit,
}

let registeredConnections: ref<array<counted>> = ref([])

let makeCountingConnection = (~held=false): counted => {
  let make: bool => counted = %raw(`(held) => {
    let count = 0
    let release = () => {}
    let status = "Destroyed"
    if (held) {
      let resolve
      const pending = new Promise(r => { resolve = r })
      status = { TAG: "Destroying", _0: pending }
      release = () => resolve()
    }
    return {
      connection: {
        TAG: "Agda",
        _0: {
          chan: { removeAllListeners: () => { count++ } },
          process: { status },
          encountedFirstPrompt: false,
          version: "2.6.4",
          path: "mock-path"
        },
        _1: "mock-path",
        _2: "2.6.4"
      },
      destroyCount: () => count,
      release
    }
  }`)
  make(held)
}

type factory = {
  make: unit => promise<result<Connection__Core.t, Connection__Core.Error.t>>,
  resolveOk: counted => unit,
  resolveError: unit => unit,
  reject: exn => unit,
  callCount: unit => int,
}

let registeredFactories: ref<array<factory>> = ref([])

let cancellationError = Connection__Core.Error.Establish(
  Connection__Core.Error.Establish.mergeMany([]),
)

let makeControlledFactory = (): factory => {
  let (promise, resolve, reject) = Util.Promise_.pending()
  let calls = ref(0)
  let factory = {
    make: () => {
      calls := calls.contents + 1
      promise
    },
    resolveOk: counted => {
      registeredConnections := Array.concat(registeredConnections.contents, [counted])
      resolve(Ok(counted.connection))
    },
    resolveError: () => resolve(Error(cancellationError)),
    reject: exn => reject(exn),
    callCount: () => calls.contents,
  }
  registeredFactories := Array.concat(registeredFactories.contents, [factory])
  factory
}

let activeConnection = () =>
  switch Registry__Connection.status.contents {
  | Registry__Connection.Active(resource) => Some(resource.connection)
  | _ => None
  }

// Every counted connection is destroyed exactly once, except the one held by Active.
let assertNoLeaks = () => {
  let active = activeConnection()
  registeredConnections.contents->Array.forEach(counted => {
    let expected = switch active {
    | Some(connection) if connection === counted.connection => 0
    | _ => 1
    }
    Assert.deepStrictEqual(counted.destroyCount(), expected)
  })
}

type landing =
  | LandEmpty
  | LandActive
  | LandConnecting
  | LandClosing

let runCase = (name, body: unit => promise<unit>) =>
  Async.it(name, async () => {
    Registry__Connection.status := Empty
    registeredConnections := []
    registeredFactories := []

    let failure = ref(None)

    let attempt = async () => {
      await body()

      let shutdownResult = await settles(Registry__Connection.shutdown(), "finish: shutdown")
      Assert.deepStrictEqual(shutdownResult.statusAtSettle, "Empty")

      let freshFactory = makeControlledFactory()
      let fresh = makeCountingConnection()
      let acquiring = settles(
        Registry__Connection.acquire("owner-fresh", freshFactory.make),
        "finish: fresh acquire",
      )
      freshFactory.resolveOk(fresh)
      let freshResult = await acquiring
      Assert.deepStrictEqual(freshResult.outcome, Ok(Ok(fresh.connection)))
      Assert.deepStrictEqual(freshResult.statusAtSettle, "Active")
      Assert.deepStrictEqual(Registry__Connection.inspect().userCount, 1)

      // late orphan destruction is async
      await tick()
      assertNoLeaks()
    }

    switch await attempt() {
    | () => ()
    | exception exn => failure := Some(exn)
    }

    registeredFactories.contents->Array.forEach(factory => factory.resolveError())
    registeredConnections.contents->Array.forEach(counted => counted.release())
    await tick()
    Registry__Connection.status := Empty

    switch failure.contents {
    | Some(exn) => raise(exn)
    | None => ()
    }
  })

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

  Async.it("same-owner concurrent execute calls should be serialized", async () => {
    await setup()
    let dummyConnection = makeDummyConnection()
    let make = async () => Ok(dummyConnection)
    let _ = await Registry__Connection.acquire("owner1", make)

    let executionOrder = []
    let (firstTaskCanFinish, resolveFirstTask, _) = Util.Promise_.pending()

    let first =
      Registry__Connection.execute("owner1", async _ => {
        executionOrder->Array.push("task1-start")
        let _ = await firstTaskCanFinish
        executionOrder->Array.push("task1-end")
        Ok()
      })

    // Give the first execute call a tick to become active before queueing the second one.
    await Util.Promise_.setTimeout(0)

    let secondStarted = ref(false)
    let second =
      Registry__Connection.execute("owner1", async _ => {
        secondStarted := true
        executionOrder->Array.push("task2-start")
        Ok()
      })

    // If serialization is correct, second task cannot start until first task is released.
    await Util.Promise_.setTimeout(0)
    Assert.deepStrictEqual(secondStarted.contents, false)

    resolveFirstTask()
    let _ = await first
    let _ = await second

    Assert.deepStrictEqual(executionOrder, ["task1-start", "task1-end", "task2-start"])
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

      let listener = Log.collect(agdaA.channels.log)

      // Load second file
      let agdaB = await AgdaMode.makeAndLoad("Lib.agda")

      let activationEvents =
        listener()
        ->Array.filterMap(log =>
          switch log {
          | Log.Connection(Log.Connection.ActivationFlow(event)) => Some(event)
          | _ => None
          }
        )

      Assert.deepStrictEqual(activationEvents, [
        Log.Connection.ActivationFlow.ActivationStarted,
        Log.Connection.ActivationFlow.ExistingConnectionReused,
        Log.Connection.ActivationFlow.ActivationSucceeded,
      ])

      await AgdaMode.quit(agdaA)
      await AgdaMode.quit(agdaB)
    })
  })
})

describe("Registry__Connection shutdown reaches Empty from every registry state", () => {
  runCase("1. Empty: shutdown settles with Empty", async () => {
    let result = await settles(Registry__Connection.shutdown(), "s")
    Assert.deepStrictEqual(result.outcome, Ok())
    Assert.deepStrictEqual(result.statusAtSettle, "Empty")
  })

  runCase("2. Active: shutdown settles with Empty", async () => {
    let factory = makeControlledFactory()
    let counted = makeCountingConnection()
    let acquiring = Registry__Connection.acquire("owner-1", factory.make)
    factory.resolveOk(counted)
    Assert.deepStrictEqual(await acquiring, Ok(counted.connection))
    Assert.deepStrictEqual(Registry__Connection.inspect().status, "Active")

    let result = await settles(Registry__Connection.shutdown(), "s")
    Assert.deepStrictEqual(result.outcome, Ok())
    Assert.deepStrictEqual(result.statusAtSettle, "Empty")
  })

  runCase("3. Closing: concurrent shutdowns both wait for the teardown", async () => {
    let factory = makeControlledFactory()
    let held = makeCountingConnection(~held=true)
    let acquiring = Registry__Connection.acquire("owner-1", factory.make)
    factory.resolveOk(held)
    Assert.deepStrictEqual(await acquiring, Ok(held.connection))

    let s1 = settles(Registry__Connection.shutdown(), "s1")
    let s2 = settles(Registry__Connection.shutdown(), "s2")
    Assert.deepStrictEqual(Registry__Connection.inspect().status, "Closing")

    held.release()
    let r1 = await s1
    let r2 = await s2
    Assert.deepStrictEqual(r1.outcome, Ok())
    Assert.deepStrictEqual(r1.statusAtSettle, "Empty")
    Assert.deepStrictEqual(r2.outcome, Ok())
    Assert.deepStrictEqual(r2.statusAtSettle, "Empty")
  })

  runCase("4. Closing: concurrent shutdowns when destroy throws", async () => {
    let broken = makeDestroyThrowingConnection()
    let _ = await Registry__Connection.acquire("owner-1", async () => Ok(broken))

    let s1 = settles(Registry__Connection.shutdown(), "s1")
    let s2 = settles(Registry__Connection.shutdown(), "s2")
    let r1 = await s1
    let r2 = await s2

    Assert.deepStrictEqual(r1.outcome->Result.mapError(errorName), Error("TypeError"))
    Assert.deepStrictEqual(r1.statusAtSettle, "Empty")
    Assert.deepStrictEqual(r2.outcome, Ok())
    Assert.deepStrictEqual(r2.statusAtSettle, "Empty")
  })

  runCase("5. Connecting: shutdown cancels the initiator and a waiter", async () => {
    let factory = makeControlledFactory()
    let waiterFactory = makeControlledFactory()

    let initiator = settles(Registry__Connection.acquire("owner-1", factory.make), "initiator")
    await tick()
    Assert.deepStrictEqual(Registry__Connection.inspect().status, "Connecting")
    let waiter = settles(Registry__Connection.acquire("owner-3", waiterFactory.make), "waiter")
    await tick()

    let s1 = settles(Registry__Connection.shutdown(), "s1")
    let s2 = settles(Registry__Connection.shutdown(), "s2")

    let rs1 = await s1
    let rs2 = await s2
    let rInitiator = await initiator
    let rWaiter = await waiter

    Assert.deepStrictEqual(rs1.outcome, Ok())
    Assert.deepStrictEqual(rs1.statusAtSettle, "Empty")
    Assert.deepStrictEqual(rs2.outcome, Ok())
    Assert.deepStrictEqual(rs2.statusAtSettle, "Empty")
    Assert.deepStrictEqual(rInitiator.outcome, Ok(Error(cancellationError)))
    Assert.deepStrictEqual(rWaiter.outcome, Ok(Error(cancellationError)))
  })

  // Factory 1 belongs to the cancelled attempt, factory 2 to the second attempt.
  // The late Ok of factory 1 must not touch whatever state the registry is in by then.
  let staleOkCase = (name, landing) =>
    runCase(name, async () => {
      let factory1 = makeControlledFactory()
      let c1 = makeCountingConnection()
      let waiterFactory = makeControlledFactory()
      let factory2 = makeControlledFactory()
      let c2 = makeCountingConnection(~held=landing == LandClosing)

      let p1 = settles(Registry__Connection.acquire("owner-1", factory1.make), "p1")
      await tick()
      Assert.deepStrictEqual(Registry__Connection.inspect().status, "Connecting")
      let w = settles(Registry__Connection.acquire("owner-3", waiterFactory.make), "w")
      await tick()

      // cancel the attempt
      let s = settles(Registry__Connection.shutdown(), "s")
      let rs = await s
      let _ = await p1
      let _ = await w
      Assert.deepStrictEqual(rs.statusAtSettle, "Empty")

      // land the second state
      let p2 = switch landing {
      | LandEmpty => None
      | _ => Some(settles(Registry__Connection.acquire("owner-2", factory2.make), "p2"))
      }
      await tick()
      let s2 = ref(None)
      switch (landing, p2) {
      | (LandConnecting, _) =>
        Assert.deepStrictEqual(Registry__Connection.inspect().status, "Connecting")
      | (LandActive, Some(p2)) =>
        factory2.resolveOk(c2)
        Assert.deepStrictEqual((await p2).outcome, Ok(Ok(c2.connection)))
      | (LandClosing, Some(p2)) =>
        factory2.resolveOk(c2)
        Assert.deepStrictEqual((await p2).outcome, Ok(Ok(c2.connection)))
        s2 := Some(settles(Registry__Connection.shutdown(), "s2"))
        Assert.deepStrictEqual(Registry__Connection.inspect().status, "Closing")
      | _ => ()
      }

      // the late result of the cancelled attempt
      factory1.resolveOk(c1)
      await tick()

      // the landing state is untouched
      switch landing {
      | LandEmpty => Assert.deepStrictEqual(Registry__Connection.inspect().status, "Empty")
      | LandActive =>
        Assert.deepStrictEqual(Registry__Connection.inspect().status, "Active")
        Assert.deepStrictEqual(activeConnection(), Some(c2.connection))
        Assert.deepStrictEqual(Registry__Connection.inspect().userCount, 1)
      | LandConnecting =>
        Assert.deepStrictEqual(Registry__Connection.inspect().status, "Connecting")
      | LandClosing => Assert.deepStrictEqual(Registry__Connection.inspect().status, "Closing")
      }

      // finish the second attempt
      switch (landing, p2, s2.contents) {
      | (LandConnecting, Some(p2), _) =>
        factory2.resolveOk(c2)
        Assert.deepStrictEqual((await p2).outcome, Ok(Ok(c2.connection)))
        Assert.deepStrictEqual(Registry__Connection.inspect().status, "Active")
        Assert.deepStrictEqual(activeConnection(), Some(c2.connection))
        Assert.deepStrictEqual(Registry__Connection.inspect().userCount, 1)
      | (LandClosing, _, Some(s2)) =>
        c2.release()
        let r2 = await s2
        Assert.deepStrictEqual(r2.outcome, Ok())
        Assert.deepStrictEqual(r2.statusAtSettle, "Empty")
      | _ => ()
      }

      // last on purpose: a leftover waiter calls its factory only after the teardown
      await tick()
      Assert.deepStrictEqual(waiterFactory.callCount(), 0)
    })

  staleOkCase("6. late Ok of a cancelled attempt lands in Empty", LandEmpty)
  staleOkCase("7. late Ok of a cancelled attempt lands in Active", LandActive)
  staleOkCase("8. late Ok of a cancelled attempt lands in Connecting", LandConnecting)
  staleOkCase("9. late Ok of a cancelled attempt lands in Closing", LandClosing)
})
