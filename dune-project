(lang dune 3.19)

(using menhir 3.0)

(using mdx 0.2)

(name ch_queries)

(generate_opam_files true)

(source
 (github andreypopp/ch_queries))

(authors "Andrey Popp <me@andreypopp.com>")

(maintainers "Andrey Popp <me@andreypopp.com>")

(license MIT)

(package
 (name ch_queries)
 (synopsis "Type safe queries for ClickHouse")
 (depends
  ocaml
  menhir
  pprint
  (ppxlib
   (< "0.36"))
  ppx_hash
  ppx_compare
  cmdliner
  containers
  extunix
  integers
  (ocamlformat :with-test)
  (mdx :with-test))
 (tags
  (database sql dsl clickhouse)))
