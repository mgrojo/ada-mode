;; Set up building with Alire, editing with mixed ada-mode and eglot.

(let* ((gpr-file (expand-file-name "build/wisitoken_alire.gpr" (file-name-directory load-file-name)))
       (eglot-workspace-configuration (list `(ada (projectFile . ,gpr-file))))
       (project
	(create-alire-project
	 :prj-name "wisitoken stephe-3 Alire eglot"
	 :gpr-file gpr-file)))

  (wisi-prj-select-cache load-file-name project)

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
