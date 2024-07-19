(lang dune 3.14)

(name okitten)

(version 0.1.0)

(generate_opam_files true)

(source
 (github bigspeedfpv/okitten))

(authors "Andrew Brower <bigspeedfpv@gmail.com>")

(maintainers "Andrew Brower <bigspeedfpv@gmail.com>")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name okitten)
 (synopsis "A Discord API wrapper for OCaml")
 (description "OKitten is a Discord API wrapper for OCaml ^_^")
 (depends ocaml dune cohttp cohttp-lwt-unix conduit-lwt dune-build-info fmt ipaddr ISO8601 lwt lwt_ppx lwt_ssl websocket websocket-lwt-unix logs uri yojson ppx_deriving ppx_yojson_conv)
 (tags
  (discord bot)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
