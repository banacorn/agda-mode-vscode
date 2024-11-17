// This module makes sure that Last Responses are handled after NonLast Responses
//
// There are 2 kinds of Responses
//  NonLast Response :
//    * get handled first
//    * don't invoke `sendAgdaRequest`
//  Last Response :
//    * have priorities, those with the smallest priority number are executed first
//    * only get handled:
//        1. after prompt has reappeared
//        2. after all NonLast Responses
//        3. after all interactive highlighting is complete
//    * may invoke `sendAgdaRequest`
module Module: {
  type t<'a>
  let make: unit => t<'a>
  let addLast: (t<'a>, int, Response.t) => unit
  let runNonLast: (t<'a>, Response.t => promise<'a>, Response.t) => unit
  let runLast: (t<'a>, Response.t => promise<'a>) => promise<unit>
} = {
  type t<'a> = {
    // keep the number of running NonLast Response
    mutable tally: int,
    allDone: Chan.t<unit>,
    deferredLastResponses: array<(int, Response.t)>,
  }
  let make = () => {
    tally: 0,
    allDone: Chan.make(),
    deferredLastResponses: [],
  }
  // NonLast Responses should fed here
  let runNonLast = (self, handler, response) => {
    self.tally = self.tally + 1
    handler(response)
    ->Promise.finally(_ => {
      self.tally = self.tally - 1
      if self.tally == 0 {
        self.allDone->Chan.emit()
      }
    })
    ->Promise.done
  }
  // deferred (Last) Responses are queued here
  let addLast = (self, priority, response) => {
    self.deferredLastResponses->Array.push((priority, response))
  }
  // gets resolved once there's no NonLast Responses running
  let onceDone = self =>
    if self.tally == 0 {
      Promise.resolve()
    } else {
      self.allDone->Chan.once
    }
  // start handling Last Responses, after all NonLast Responses have been handled
  let runLast = async (self, handler) => {
    await self->onceDone
    // sort the deferred Responses by priority (ascending order)
    let deferredLastResponses = {
      self.deferredLastResponses->Array.sort((x, y) => Int.compare(fst(x), fst(y)))
      self.deferredLastResponses->Array.map(snd)
    }

    // insert `CompleteHighlightingAndMakePromptReappear` after handling Last Responses
    deferredLastResponses->Array.unshift(Response.CompleteHighlightingAndMakePromptReappear)

    let _ = await deferredLastResponses->Array.map(res => handler(res))->Util.oneByOne
  }
}

include Module
