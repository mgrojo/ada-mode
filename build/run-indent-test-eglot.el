;; run tests using eglot

(setq ada-xref-backend 'eglot)
(setq ada-indent-backend 'eglot)
(setq ada-face-backend 'eglot) ;; FIXME; als 22 doesn't support faces; should get error message

(setq project-find-functions '(wisi-prj-current-cached))

;; FIXME: when alire supports "install", install this somewhere more standard
(setq gnat-lsp-server-exec "/Projects/alire/ada_language_server_22.0.0_ef4bdf41/.obj/server/ada_language_server")


;; ;; we need the wisi parser for faces
;; (setq ada-mode-dir (file-name-directory (locate-file "ada-mode.el" load-path)))
;; (setq ada-process-parse-exec (expand-file-name "ada_mode_wisi_lalr_parse" ada-mode-dir))

(require 'run-indent-test)

(setq debug-on-error nil)

;;; end of file
