# Testing an OCaml Library

The simplest way to test a dune-based OCaml library is to install the libraries
dependencies and then run the tests.  

First, you'll need to grab the source code. 

```shark-import
https://github.com/patricoferris/ppx_deriving_yaml.git /data/ppx_deriving_yaml
```

Then setup an OCaml environment, luckily we have the `ocaml/opam` images that do the
heavy-lifting for us.

```shark-build:ocaml-env
((from "ocaml/opam"))
```

Then we can run our commands to install dependencies.

<!-- TODO: Support package manager "outputs" -->

```shark-run:ocaml-env
$ opam install /data/ppx_deriving_yaml --deps-only --with-test
$ opam exec -- dune build --root /data/ppx_deriving_yaml @install @runtest
```

