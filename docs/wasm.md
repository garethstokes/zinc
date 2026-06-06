---
title: WebAssembly targets
nav_order: 12
---

# WebAssembly targets

## Why

Compiling Haskell to WebAssembly normally means setting up the GHC wasm
cross-compiler by hand. That setup is exactly the kind of thing zinc already
does: the wasm toolchain is delivered as a Nix flake, and zinc manages flakes. So
zinc compiles to wasm with no toolchain setup.

## What

`zinc build --target wasm32-wasi` builds a WebAssembly module. zinc provisions
the wasm GHC cross-compiler (and a runtime to run it) through the generated
flake, drives it like any other build, and keys the cache by target so native
and wasm artifacts coexist. Native is the default; `--target` selects an
architecture.

Two flavors are supported: a `wasm32-wasi` command module (run with the provided
`wasmtime`), and a browser reactor module with the JavaScript FFI. Template
Haskell works, via the toolchain's interpreter.

## How

Build and run a command module:

```
zinc build --target wasm32-wasi
zinc run --target wasm32-wasi
```

The cache is keyed by target, so a wasm build does not disturb your native build
and vice versa.

## Examples

```
$ zinc build --target wasm32-wasi
   Building (wasm32-wasi) [9/12] aeson
   Finished in 5.0s · myapp.wasm

$ zinc run --target wasm32-wasi
Hello from myapp!
```

Limitation: packages with C FFI or `system-libs` generally do not cross-compile
to wasm (there is rarely a wasm build of the C library). A closure member that
needs C is reported as unsupported for wasm rather than failing cryptically;
pure-Haskell closures build cleanly. WebAssembly has no native threads, so wasm
builds do not use the threaded runtime.
