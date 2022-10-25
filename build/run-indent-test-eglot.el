;; run tests using eglot, no wisi parser

(setq ada-xref-backend 'eglot)
(setq ada-indent-backend 'none) ;; als 22 doesn't support range indent
(setq ada-face-backend 'none) ;; als 22 doesn't support semantic_tokens

(setq skip-reindent-test (eq ada-indent-backend 'none))
(setq skip-recase-test   (eq ada-face-backend 'none))
(setq ada-eglot-require-gpr t)

(setq project-find-functions '(wisi-prj-current-cached))

;; FIXME: when alire supports "install", install this somewhere more standard
(setq gnat-lsp-server-exec "/Projects/alire/ada_language_server_22.0.0_ef4bdf41/.obj/server/ada_language_server")

(require 'run-indent-test)

(setq debug-on-error nil)

;;; end of file
