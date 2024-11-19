module Node = {
  type t<'a>
  @send external getValue: t<'a> => 'a = "getValue"
}

type t<'a>

////////////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////////////

@module("@datastructures-js/binary-search-tree") @new
external make: unit => t<'a> = "BinarySearchTree"

////////////////////////////////////////////////////////////////////////////////
// Methods
////////////////////////////////////////////////////////////////////////////////

@send external count: t<'a> => int = "count"

@send external insert: (t<'a>, int, 'a) => unit = "insert"

@send external has: (t<'a>, int) => bool = "has"

@send external find: (t<'a>, int) => Nullable.t<Node.t<'a>> = "find"
let find = (self: t<'a>, key: int): option<'a> =>
  self->find(key)->Nullable.toOption->Option.map(Node.getValue)

@send external max: t<'a> => Nullable.t<Node.t<'a>> = "max"
let max = (self: t<'a>): option<'a> => self->max->Nullable.toOption->Option.map(Node.getValue)

@send external min: t<'a> => Nullable.t<Node.t<'a>> = "min"
let min = (self: t<'a>): option<'a> => self->min->Nullable.toOption->Option.map(Node.getValue)

@send external upperBound: (t<'a>, int) => Nullable.t<Node.t<'a>> = "upperBound"
let upperBound = (self: t<'a>, key: int): option<'a> =>
  self->upperBound(key)->Nullable.toOption->Option.map(Node.getValue)

@send external lowerBound: (t<'a>, int) => Nullable.t<Node.t<'a>> = "lowerBound"
let lowerBound = (self: t<'a>, key: int): option<'a> =>
  self->lowerBound(key)->Nullable.toOption->Option.map(Node.getValue)

@send external floor: (t<'a>, int) => Nullable.t<Node.t<'a>> = "floor"
let floor = (self: t<'a>, key: int): option<'a> =>
  self->floor(key)->Nullable.toOption->Option.map(Node.getValue)

@send external ceil: (t<'a>, int) => Nullable.t<Node.t<'a>> = "ceil"
let ceil = (self: t<'a>, key: int): option<'a> =>
  self->ceil(key)->Nullable.toOption->Option.map(Node.getValue)

@send external remove: (t<'a>, int) => bool = "remove"

@send external clear: t<'a> => unit = "clear"

@send external traverseInOrder: (t<'a>, Node.t<'a> => unit) => unit = "traverseInOrder"

let toArray = (self: t<'a>): array<'a> => {
  let accum = []
  self->traverseInOrder(node => {
    let value = node->Node.getValue
    accum->Array.push(value)
  })
  accum
}
