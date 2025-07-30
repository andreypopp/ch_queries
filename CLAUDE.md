# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository.

## Project overview

This is an OCaml project named "queries" using dune as the build system.

## Building and debugging the project

### Dune watch mode

The user typically runs dune build in watch mode outside Claude session):

- Check for `_build/.lock` to see if watch mode is active
- Build errors visible via `dune build <dir>` (works with watch mode)
- **Never** kill dune, remove lock file, or clean build when watch mode is
  active
- In watch mode, When you make changes, check if there are build errors with
  `dune build <dir>` first, otherwise you'll be running the old tests.
- To run tests:
    - `dune test` runs all tests
    - `dune test <specific test>` runs a specific test
  do not run tests if watch mode is active, as it will conflict with the build.
- When running an executable, use:
    `DUNE_CONFIG__GLOBAL_LOCK=disabled dune exec COMMAND --no-build -- ARGS...`
  so it doesn't conflict with the build running in watch mode.

## Project structure

- **queries/** - core libary `queries` which implements typesafe DSL combinators for query generation
- **queries_syntax/** - surface syntax + lexer/parser for the DSL
- **queries_ppx/** - a ppx rewriter which translates surface syntax to the typesafe DSL combinators
- **bin/** - some debug UI
- **test/** - tests in cram format (`*.t`), use `dune test` to run them, `dune test --auto-promote` to promote passing tests

## Code Style

- **Naming**: `snake_case` for values/functions/types, `My_module` for modules/variants
- **Philosophy**: Unix-style - do one thing well, fail loudly, clarity over cleverness
- **Interfaces**: One `.mli` per `.ml`, keep minimal
- **Docs**: Terse first line, document invariants not obvious behavior
- **Errors**: `function_name: what went wrong` format, fail fast
- **Type annotations**: Avoid explicit types unless required by type checker

## Developing with OCaml

We use `containers` library as a standard library. We prefer the version with
labels `ContainersLabels`. We configure in `dune` to always have `-open
ContainersLabels` in the compilation flags, so we don't have to write
`ContainersLabels.` before every module.

For parsing we use `menhir`, a parser generator. We use new syntax for rules.

For command line interfaces we use `cmdliner`.

For pretty printing we use `pprint` library.

When adding a new opam package, we need to install it, then add package to the
`dune-project`. Finall to use it, we need to add the lib (or ppx) to the `dune`
file in the relevant directory.

By default, as we use `containers`, the `=` operator has the type `int -> int
-> bool`. To compare values we need type specific operators, like
`String.equal` or `List.equal` and etc. Same holds for `compare` function (and
`>`, `<`, `>=`, `<=` operators).
