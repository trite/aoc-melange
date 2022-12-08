(lang dune 3.1)

(using directory-targets 0.1)

(generate_opam_files true)

(name aoc-melange)

(license "MIT")

(maintainers <your_name>)

(authors <your_name>)

(homepage https://github.com/your/project)

(bug_reports https://github.com/your/project/issues)

(package
 (allow_empty)
 (name aoc-melange)
 (synopsis "Melange project")
 (description "Minimal melange project")
 (depends
  (ocaml
   (and
    (>= "4.14.0")
    (< "4.15.0")))
  (reason
   (and
    (>= "3.8.2")
    (< "4.0.0")))
  mel
  melange
  (ocaml-lsp-server :dev)
  (dot-merlin-reader :dev)))
