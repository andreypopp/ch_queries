# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository.

## Project overview

This is an OCaml project named "ch_queries" using dune as the build system.

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
- When running an executable, use (only works when watch mode is not active):
    `dune exec ch_queries -- ARGS...`
  so it doesn't conflict with the build running in watch mode.

## Project structure

- **ch_queries/** - core libary which implements typesafe DSL combinators for query generation
- **ch_queries_syntax/** - surface syntax + lexer/parser for the DSL
    - **ch_queries_syntax/syntax.ml** - AST types and data structures for the DSL
    - **ch_queries_syntax/lexer.mll** - lexer for the DSL (tokens are defined in the parser)
    - **ch_queries_syntax/parser.mly** - parser for the DSL
    - **ch_queries_syntax/printer.ml** - printer for the syntax, uses pprint library
- **ch_queries_ppx/** - a ppx rewriter which translates surface syntax to the typesafe DSL combinators
- **bin/** - some debug UI
- **test/** - tests in cram format (`*.t`)
    - to run: `dune test`
    - to run and promote the changes in expected vs current: `dune test --auto-promote`, only use when you've verified the changes are correct

when exploring a project structure, it is fine to read entire files (they are small)

## Architecture overview

The `ch_queries_ppx` parses the surface syntax with `ch_queries_syntax` and
translates it to the typesafe DSL combinators defined in `ch_queries`.

The `ch_queries` combinators are being translated back to `ch_queries_syntax`
but only to the subset which maps to SQL. Which is then printed to SQL string.

## Adding new syntax features

When adding new syntax to the query language, follow this pattern:

1. **Add to syntax.ml**: Define the AST types for the new feature
2. **Add to lexer.mll**: Add new keywords to the keywords table and handle in string_of_token
3. **Add to parser.mly**: 
   - Add token declarations
   - Add parsing rules
   - Include new fields in existing rules (e.g., SELECT queries)
4. **Update printer.ml**: Add pretty-printing support for the new syntax
5. **Update ch_queries.ml**: Add the feature to the core DSL types and translation
6. **Update ch_queries.mli**: Add interface signatures for new functions
7. **Update ch_queries_ppx.ml**: Add PPX transformation support
8. **Run `dune build`** and fix any build errors systematically

Always check that interface files (.mli) match implementation files (.ml) when adding new optional parameters to functions.

## Code Style

- **Naming**: `snake_case` for values/functions/types, `My_module` for modules/variants
- **Philosophy**: Unix-style - do one thing well, fail loudly, clarity over cleverness
- **Interfaces**: One `.mli` per `.ml`, keep minimal
- **Docs**: Terse first line, document invariants not obvious behavior
- **Errors**: `function_name: what went wrong` format, fail fast
- **Type annotations**: Avoid explicit types unless required by type checker
- **Git Commits**:
    - use imperative mood, e.g., "Add feature X", "Fix bug Y", "Refactor Z"
    - be concise but descriptive
    - no need to repeat everything you've done in the commit

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

## Developing with OCaml: common pitfalls

- **Record field access**: When the compiler can't infer types, use qualified access like `id.Syntax.node` instead of `id.node`
- **Pattern matching**: Always handle all cases in pattern matches. Use `_` only when explicitly ignoring unused variants  
- **Record construction**: When adding fields to existing record types, think first where the field values should come from (function arguments, extending other types), then consider default/empty values as a last resort
