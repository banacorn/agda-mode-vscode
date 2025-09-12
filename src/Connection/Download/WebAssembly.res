// WebAssembly JavaScript interface bindings for ReScript

// Core WebAssembly types
type wasmBytes = Uint8Array.t
type importObject<'a> = Js.Dict.t<Js.Dict.t<'a>>

// WebAssembly.Module
module Module = {
  type t

  @new @scope("WebAssembly")
  external make: wasmBytes => t = "Module"

  @scope("WebAssembly") @val
  external customSections: (t, string) => array<Uint8Array.t> = "Module.customSections"

  @scope("WebAssembly") @val
  external exports: t => array<'a> = "Module.exports"

  @scope("WebAssembly") @val
  external imports: t => array<'a> = "Module.imports"
}

// WebAssembly.Instance
module Instance = {
  type t

  @new @scope("WebAssembly")
  external make: (Module.t, importObject<'a>) => t = "Instance"

  @get
  external exports: t => Js.Dict.t<'a> = "exports"
}

// WebAssembly.Memory
module Memory = {
  type t
  type descriptor = {
    initial: int,
    maximum?: int,
    shared?: bool,
  }

  @new @scope("WebAssembly")
  external make: descriptor => t = "Memory"

  @get
  external buffer: t => ArrayBuffer.t = "buffer"

  @send
  external grow: (t, int) => int = "grow"
}

// WebAssembly.Table
module Table = {
  type t
  type descriptor = {
    element: string, // "anyfunc" or "externref"
    initial: int,
    maximum?: int,
  }

  @new @scope("WebAssembly")
  external make: descriptor => t = "Table"

  @get
  external length: t => int = "length"

  @send
  external get: (t, int) => option<'a> = "get"

  @send
  external set: (t, int, 'a) => unit = "set"

  @send
  external grow: (t, int, option<'a>) => int = "grow"
}

// WebAssembly.Global
module Global = {
  type t
  type descriptor = {
    value: string, // "i32", "i64", "f32", "f64", "v128", "externref", "anyfunc"
    @as("mutable") mutable_?: bool,
  }

  @new @scope("WebAssembly")
  external make: (descriptor, 'a) => t = "Global"

  @get
  external value: t => 'a = "value"

  @set
  external setValue: (t, 'a) => unit = "value"
}

// WebAssembly.Tag (for exception handling)
module Tag = {
  type t
  type tagType = {
    parameters: array<string>,
  }

  @new @scope("WebAssembly")
  external make: tagType => t = "Tag"

  @send
  external type_: t => tagType = "type"
}

// WebAssembly.Exception
module Exception = {
  type t

  @new @scope("WebAssembly")
  external make: (Tag.t, array<'a>) => t = "Exception"

  @send
  external getArg: (t, Tag.t, int) => 'a = "getArg"

  @send
  external is: (t, Tag.t) => bool = "is"

  @get
  external stack: t => option<string> = "stack"
}

// WebAssembly static methods
@scope("WebAssembly")
external compile: wasmBytes => promise<Module.t> = "compile"

@scope("WebAssembly")
external compileStreaming: promise<'response> => promise<Module.t> = "compileStreaming"

@scope("WebAssembly")
external instantiate: (Module.t, importObject<'a>) => promise<Instance.t> = "instantiate"

@scope("WebAssembly")
external instantiateFromBytes: (wasmBytes, importObject<'a>) => promise<{"module": Module.t, "instance": Instance.t}> = "instantiate"

@scope("WebAssembly")
external instantiateStreaming: (promise<'response>, importObject<'a>) => promise<{"module": Module.t, "instance": Instance.t}> = "instantiateStreaming"

@scope("WebAssembly")
external validate: wasmBytes => bool = "validate"

// WebAssembly Error types
module CompileError = {
  type t
  @new @scope("WebAssembly") external make: string => t = "CompileError"
}

module LinkError = {
  type t
  @new @scope("WebAssembly") external make: string => t = "LinkError"
}

module RuntimeError = {
  type t
  @new @scope("WebAssembly") external make: string => t = "RuntimeError"
}