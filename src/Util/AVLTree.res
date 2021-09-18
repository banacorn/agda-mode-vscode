open Belt
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

@send external find: (t<'a>, int) => Js.nullable<Node.t<'a>> = "find"
let find = (self: t<'a>, key: int): option<'a> =>
  self->find(key)->Js.Nullable.toOption->Option.map(Node.getValue)

@send external max: t<'a> => Js.nullable<Node.t<'a>> = "max"
let max = (self: t<'a>): option<'a> => self->max->Js.Nullable.toOption->Option.map(Node.getValue)

@send external min: t<'a> => Js.nullable<Node.t<'a>> = "min"
let min = (self: t<'a>): option<'a> => self->min->Js.Nullable.toOption->Option.map(Node.getValue)

@send external upperBound: (t<'a>, int) => Js.nullable<Node.t<'a>> = "upperBound"
let upperBound = (self: t<'a>, key: int): option<'a> =>
  self->upperBound(key)->Js.Nullable.toOption->Option.map(Node.getValue)

@send external lowerBound: (t<'a>, int) => Js.nullable<Node.t<'a>> = "lowerBound"
let lowerBound = (self: t<'a>, key: int): option<'a> =>
  self->lowerBound(key)->Js.Nullable.toOption->Option.map(Node.getValue)

@send external floor: (t<'a>, int) => Js.nullable<Node.t<'a>> = "floor"
let floor = (self: t<'a>, key: int): option<'a> =>
  self->floor(key)->Js.Nullable.toOption->Option.map(Node.getValue)

@send external ceil: (t<'a>, int) => Js.nullable<Node.t<'a>> = "ceil"
let ceil = (self: t<'a>, key: int): option<'a> =>
  self->ceil(key)->Js.Nullable.toOption->Option.map(Node.getValue)

@send external remove: (t<'a>, int) => bool = "remove"

@send external clear: t<'a> => unit = "clear"

@send external traverseInOrder: (t<'a>, Node.t<'a> => unit) => unit = "traverseInOrder"

let toArray = (self: t<'a>): array<'a> => {
  let accum = []
  self->traverseInOrder(node => {
    let value = node->Node.getValue
    Js.Array.push(value, accum)->ignore
  })
  accum
}
