(lang dune 3.0)
(package
 (name tcp_server_ocaml)
 (allow_empty)
 (depends
  (ocaml (>= 4.14.0))
  lwt
  logs
  logs-fmt))

; (lang dune 3.16)
;
; (name tcp_server_ocaml)
;
; (generate_opam_files true)
;
; (source
;  (github username/reponame))
;
; (authors "Author Name")
;
; (maintainers "Maintainer Name")
;
; (license LICENSE)
;
; (documentation https://url/to/documentation)
;
; (package
;  (name tcp_server_ocaml)
;  (synopsis "A short synopsis")
;  (description "A longer description")
;  (depends ocaml dune)
;  (tags
;   (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html




