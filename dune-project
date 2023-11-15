(lang dune 3.11)

(name quoridor)

(generate_opam_files true)

(source
 (uri git+https://gaufre.informatique.univ-paris-diderot.fr/quoridor-2023/projet-ocaml-2023---quoridor.git))

(authors "Evan Joffrin")

(maintainers "Ali Eljamai, Mathaui Yves Abdorohmang")

(license LICENSE)

(package
 (name quoridor)
 (synopsis "Quoridor Game")
 (description "Implementation of Quoridor Game in OCaml")
 (depends ocaml dune alcotest qcheck qcheck-alcotest))

