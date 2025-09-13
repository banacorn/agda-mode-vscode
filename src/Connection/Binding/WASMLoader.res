// WASM Loader bindings for the qbane.als-wasm-loader extension

// External bindings for WASM loader operations
@send external load: 'wasmAPILoader => 'wasm = "load"
@send external createMemoryFileSystem: 'wasm => promise<'memfs> = "createMemoryFileSystem"

// Factory creation helper
let createFactory = (constructor, wasm, mod) => %raw(`new constructor(wasm, mod)`)