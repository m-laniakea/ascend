(lang dune 3.16)

(name ascend)

(generate_opam_files true)

(source
 (github m-laniakea/ascend))

(authors "eir")

(maintainers "m-laniakea")

(license AGPL-3.0-only)

(package
 (name ascend)
 (synopsis "Ascend - Terminal Dungeon RPG adventure")
 (description "Venture into the depths to retrieve the stolen artifact, and ascend! NetHack lite.")
 (depends
  (ocaml (>= 5.2.0))
  dune
  (notty (>= 0.2.3)))
 (tags
  (game terminal)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
