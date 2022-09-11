;; Set up building with Alire, editing with mixed ada-mode and eglot. -*- no-byte-compile : t -*-
;;
;; No language server for gpr, so use wisi parser for that.
;; For Ada, use as many eglot features as possible

(require 'ada-mode)
;; This require is not needed for the following code, but is needed to
;; ensure ada-mode-hook has sal-ada-mode-setup.

(setq ada-indent-engine 'eglot) ;; testing eglot indent

(setq ada-xref-tool 'eglot) ;; testing wisi-xref lsp backend

(add-hook 'ada-mode-hook #'ada-eglot-setup)

(let* ((gpr-file (expand-file-name "wisi_alire.gpr" (file-name-directory load-file-name)))
       (prj-file (expand-file-name "prj-eglot.prj" (file-name-directory load-file-name)))
       (eglot-workspace-configuration (list `(ada (projectFile . ,gpr-file))))

       (project
	(create-alire-project
	 :prj-name "wisi stephe-3 Alire eglot"
	 :prj-file prj-file
	 :gpr-file gpr-file)))

  (wisi-prj-select-cache prj-file nil "Makefile")

  ;; ada_language_server gets GPR_PROJECT_PATH from its process environment.
  (let ((process-environment
	 (append
	  (copy-sequence process-environment)
	  (wisi-prj-compile-env project)
	  (wisi-prj-file-env project))))

    (eglot 'ada-mode ;; managed-major-mode
	   project ;; project; project-root is server process directory
	   'eglot-lsp-server ;; class
	   'gnat-find-als ;; contact
	   "Ada" ;; language-id
	   ))
  )
