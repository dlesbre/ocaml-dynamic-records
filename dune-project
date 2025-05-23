(lang dune 3.18)

(name dynamic-record)

(version 0.1)

(generate_opam_files true)

(source
 (github dlesbre/ocaml-dynamic-record))

(authors "Dorian Lesbre <dorian.lesbre@gmail.com>")

(maintainers "Dorian Lesbre <dorian.lesbre@gmail.com>")

; (license LICENSE)

; (documentation https://url/to/documentation)

(package
 (name dynamic-record)
 (synopsis "Dynamic records allow adding new fields dynamically at any time")
 (description
  "Anyone can add a field at any time to a dynamic record. Newly added fields can be accessed even on record that were created before they were added. Adding a field requires specifying a default value, which is used to initialize it. Fields can be mutable or immutable. One can only access the fields one knows exist. ")
 (depends ocaml)
 (tags
  ("dynamic record" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
