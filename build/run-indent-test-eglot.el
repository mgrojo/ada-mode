;; run tests using eglot, no wisi parser

(require 'cl-lib)

(setq ada-xref-backend 'eglot)
(setq ada-indent-backend 'none) ;; als 22 doesn't support range indent
(setq ada-face-backend 'eglot) ;; als 22 doesn't support semantic_tokens; FIXME: testing devel version

(setq skip-reindent-test (eq ada-indent-backend 'none))
(setq skip-recase-test   (eq ada-face-backend 'none))
(setq ada-eglot-require-gpr t)

(setq project-find-functions '(wisi-prj-current-cached))

;; when alire supports "install", install this in ~/.local
(setq
 gnat-lsp-server-exec
 (concat
  (cl-case system-type
    (gnu/linux
     "/Projects/alire-workspace/")
    (windows-nt
     "c:/Projects/alire/"))
  "ada_language_server_22.0.0_ef4bdf41/.obj/server/ada_language_server"))

(require 'run-indent-test)

(setq debug-on-error nil)

;;; end of file
