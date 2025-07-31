// The code has been updated to preserve the API while adapting to the
// interface change of @datastructures-js/binary-search-tree from v4 to v5.
// Namely, the library no longer store keys aside from value. Instead, the
// constructor accepts a custom comparator and the field name of keys.

module Node = {
  type t<'a>
  @send external getKeyValue: t<'a> => (int, 'a) = "getValue"
  let getValue = (self: t<'a>): 'a => self->getKeyValue->(((_, v)) => v)
}

type t<'a>

type compareFunction<'a> = ((int, 'a), (int, 'a)) => int

type options = {key?: string}

////////////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////////////

@module("@datastructures-js/binary-search-tree") @new
external makeInner: (~compare: compareFunction<'a>=?, ~options: options=?, unit) => t<'a> =
  "BinarySearchTree"

let make = (): t<'a> => makeInner(~compare=((ka, _), (kb, _)) => ka - kb, ~options={key: "0"}, ())

////////////////////////////////////////////////////////////////////////////////
// Methods
////////////////////////////////////////////////////////////////////////////////

@send external count: t<'a> => int = "count"

@send external insert: (t<'a>, (int, 'a)) => unit = "insert"
let insert = (self: t<'a>, key: int, value: 'a) => self->insert((key, value))

@send external has: (t<'a>, int) => bool = "has"

@send external findKey: (t<'a>, int) => Nullable.t<Node.t<'a>> = "findKey"
let find = (self: t<'a>, key: int): option<'a> =>
  self->findKey(key)->Nullable.toOption->Option.map(Node.getValue)

@send external max: t<'a> => Nullable.t<Node.t<'a>> = "max"
let max = (self: t<'a>): option<'a> => self->max->Nullable.toOption->Option.map(Node.getValue)

@send external min: t<'a> => Nullable.t<Node.t<'a>> = "min"
let min = (self: t<'a>): option<'a> => self->min->Nullable.toOption->Option.map(Node.getValue)

@send external upperBoundKey: (t<'a>, int) => Nullable.t<Node.t<'a>> = "upperBoundKey"
let upperBound = (self: t<'a>, key: int): option<'a> =>
  self->upperBoundKey(key)->Nullable.toOption->Option.map(Node.getValue)

@send external lowerBoundKey: (t<'a>, int) => Nullable.t<Node.t<'a>> = "lowerBoundKey"
let lowerBound = (self: t<'a>, key: int): option<'a> =>
  self->lowerBoundKey(key)->Nullable.toOption->Option.map(Node.getValue)

@send external floorKey: (t<'a>, int) => Nullable.t<Node.t<'a>> = "floorKey"
let floor = (self: t<'a>, key: int): option<'a> =>
  self->floorKey(key)->Nullable.toOption->Option.map(Node.getValue)

@send external ceilKey: (t<'a>, int) => Nullable.t<Node.t<'a>> = "ceilKey"
let ceil = (self: t<'a>, key: int): option<'a> =>
  self->ceilKey(key)->Nullable.toOption->Option.map(Node.getValue)

@send external removeNode: (t<'a>, Node.t<'a>) => bool = "removeNode"
let remove = (self: t<'a>, key: int): bool =>
  self->findKey(key)->Nullable.toOption->Option.mapOr(false, node => self->removeNode(node))

@send external clear: t<'a> => unit = "clear"

@send external traverseInOrder: (t<'a>, Node.t<'a> => unit) => unit = "traverseInOrder"

let toArray = (self: t<'a>): array<(int, 'a)> => {
  let accum = []
  self->traverseInOrder(node => {
    let key = node->Node.getKeyValue->(((k, _)) => k)
    let value = node->Node.getValue
    accum->Array.push((key, value))
  })
  accum
}

let forEach = (self: t<'a>, f: 'a => unit): unit =>
  self->traverseInOrder(node => node->Node.getValue->f)

let forEachWithIndex = (self: t<'a>, f: (int, 'a) => unit): unit => {
  let index = ref(0)
  self->traverseInOrder(node => {
    let value = node->Node.getValue
    f(index.contents, value)
    index := index.contents + 1
  })
}
