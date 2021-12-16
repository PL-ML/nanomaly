# Nanomaly $\approx$ NanoML

A simple OCaml parsera and (static + dynamic) type checker in Haskell

## Compile and run

Note that executables or libraries can be configured in [nanoml.cabal](nanoml.cabal)

### Compile

```
stack setup
stack build
```

### Parse OCaml Code

```
stack exec -- ocaml-to-json  < prog.ml 

stack exec -- ocaml-to-seq   < prog.ml 
```