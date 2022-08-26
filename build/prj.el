;; assumes wisi autoloads are defined.

(when (memq #'project-try-vc project-find-functions)
  (remove-hook 'project-find-functions #'project-try-vc))

(unless (memq #'wisi-prj-current-cached project-find-functions)
  (add-hook 'project-find-functions #'wisi-prj-current-cached 90)
  (add-hook 'xref-backend-functions #'wisi-prj-xref-backend 10))

;;; Compiling SAL with standard .gpr, ada-mode 7.3.beta
;;  (wisi-prj-select-cache "sal.prj" (create-ada-prj :name "sal stephe-2") "Makefile"))

;;; Compiling SAL with Alire .gpr, eglot
(wisi-prj-select-cache "sal.prj" (create-ada-prj :name "sal stephe-2") "Makefile")

;; FIXME: turn off stuff that conflicts with eglot

(setq-default
 eglot-workspace-configuration
 `((ada (projectFile . ,(expand-file-name "stephes_ada_library.gpr")))))

(eglot
 'ada-lite-mode
 (wisi-prj-current-cached nil)
 'eglot-lsp-server ;; class
 #'gnat-find-als ;; contact
 "Ada" ;; language-id
 )

;; end of file
