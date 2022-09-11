;; assumes wisi autoloads are defined.

(wisi-prj-select-cache "sal.prj" (create-ada-prj :name "sal main") "Makefile")

;; FIXME: use ada-eglot
;; (setq-default
;;  eglot-workspace-configuration
;;  `((ada (projectFile . ,(expand-file-name "stephes_ada_library.gpr")))))

;; (eglot
;;  'ada-lite-mode
;;  (wisi-prj-current-cached nil)
;;  'eglot-lsp-server ;; class
;;  #'gnat-find-als ;; contact
;;  "Ada" ;; language-id
;;  )

;; end of file
