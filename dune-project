(lang dune 3.16)

(name owo2)

(generate_opam_files true)

(source
 (github jalsol/owo-lang))

(authors "Nguyen Quang Truong")

(maintainers "Nguyen Quang Truong")

(package
 (name owo2)
 (depends ocaml dune))

(using menhir 3.0)
