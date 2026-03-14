(lang dune 3.21.1)

(name effective_concurrency)

(generate_opam_files true)

(source
 (github michaelleejl/effective-concurrency))

(authors "Michael Lee <michael.lee@cl.cam.ac.uk>")

(maintainers "Michael Lee <michael.lee@cl.cam.ac.uk>")

(license MIT)

(documentation https://www.cl.cam.ac.uk/teaching/2526/Semantics/)

(package
 (name effective_concurrency)
 (synopsis "A collection of programs designed to teach the semantics of exceptions, effects, and concurrency.")
 (description "A collection of OCaml 5.3.0 programs designed to teach the semantics of exceptions, effects, and concurrency, designed for the University of Cambridge Part IB Semantics of Programming Languages course")
 (depends 
  (ocaml (>= 5.3.0))
 )
 (tags
  (exceptions effects concurrency semantics smallstep)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
