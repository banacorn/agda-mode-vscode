// Module for representing a resource that may be acquired immediately or later.
module Module: {
  type t<'a>
  let make: unit => t<'a>
  let get: t<'a> => promise<'a>
  let set: (t<'a>, 'a) => unit
  let isPending: t<'a> => bool
} = {
  type state<'a> =
    | Pending(promise<'a>, 'a => unit)
    | Acquired('a)
  type t<'a> = {mutable state: state<'a>}

  let make = () => {
    let (promise, resolve, _) = Util.Promise_.pending()
    {state: Pending(promise, resolve)}
  }

  let get = async self => {
    switch self.state {
    | Pending(promise, _) => await promise
    | Acquired(value) => value
    }
  }

  let set = (self, value) => {
    switch self.state {
    | Pending(_, resolve) =>
      resolve(value)
      self.state = Acquired(value)
    | Acquired(_) => self.state = Acquired(value) // update the value
    }
  }

  let isPending = self => {
    switch self.state {
    | Pending(_, _) => true
    | Acquired(_) => false
    }
  }
}

include Module
