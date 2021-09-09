type t<'a>

////////////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////////////

// Create new instance of interval tree
@module("@flatten-js/interval-tree") @new
external make: unit => t<'a> = "default"

////////////////////////////////////////////////////////////////////////////////
// Properties & Methods
////////////////////////////////////////////////////////////////////////////////

// Insert new item into the tree. Key is an interval object or pair of numbers [low, high].
// Value may represent any value or reference to any object. If value omitted, tree will store and retrieve keys as values.
// Method returns reference to the inserted node
@send external insert: (t<'a>, (int, int), 'a) => 'a = "insert"

// Method returns true if item {key, value} exists in the tree.
// Method may be useful if need to support unique items.
@send external exist: (t<'a>, (int, int), 'a) => bool = "exist"

// Removes item from the tree. Returns true if item was actually deleted, false if not found.
@send external remove: (t<'a>, (int, int), 'a) => bool = "remove"

// Returns array of values which keys intersected with given interval.
@send external search: (t<'a>, (int, int)) => array<'a> = "search"

// Optional outputMapperFn(value, key) enables to map search results into custom defined output.
@send
external searchAndMap: (t<'a>, array<(int, int)>, ('a, (int, int)) => 'b) => array<'b> = "search"

// Returns true if intersection between given and any interval stored in the tree found
@send external intersectAny: (t<'a>, (int, int)) => bool = "intersect_any"

// Returns number of items stored in the tree (getter)
@get external size: t<'a> => int = "size"

// Returns tree keys in ascendant order (getter)
@get external keys: t<'a> => array<(int, int)> = "keys"

// Returns items in ascendant keys order (getter)
@get external items: t<'a> => array<{"key": (int, int), "value": 'a}> = "items"

// Enables to traverse the whole tree and perform operation for each item
@send external forEach: (t<'a>, ((int, int), 'a) => unit) => unit = "forEach"

// Creates new tree with same keys using callback to transform (key,value) to a new value
@send external map: (t<'a>, ('a, (int, int)) => 'b) => t<'b> = "map"
