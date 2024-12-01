(lang dune 3.17)

(name aoc2024)
(version 0.1.0)

(generate_opam_files true)

(source
 (github Nicclassy/advent-of-code-2024))

(authors "Sixstring982" "Nicclassy")

(maintainers "Nicclassy")

(license BSD-3-Clause)

(documentation https://sixstring982.github.io/tanenbaum)

(package
 (name aoc2024)
 (synopsis "Advent of code 2024 solutions")
 (depends
  ocaml
  reason
  dune
  (cmdliner(>= 1.2.0))
  (piaf (>= 0.1.0))
  (fmt (>= 0.9.0)))
 (tags ("advent-of-code")))
