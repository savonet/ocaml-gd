(lang dune 2.8)
(version 1.1)
(name gd)
(source (github savonet/ocaml-gd))
(license LGPL-2.1-only)
(authors "The Savonet Team <savonet-users@lists.sourceforge.net>")
(maintainers "Romain Beauxis <toots@rastageeks.org>")

(generate_opam_files true)
(use_standard_c_and_cxx_flags false)

(package
 (name gd)
 (synopsis "OCaml interface to the GD graphics library.")
 (depends
  conf-gd
  conf-pkg-config
  dune
  dune-configurator)
)
