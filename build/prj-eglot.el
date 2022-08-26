;; set up eglot for the emacs ada-mode project

;; FIXME: merge this into ada-eglot-setup
(let ((process-environment
       (copy-sequence
	(append
	 (list (concat "GPR_PROJECT_PATH="
		       "/Projects/org.emacs.ada-mode"
		       ":/Projects/org.emacs.wisi"
		       ":/Projects/org.wisitoken/build"
		       ":/Projects/org.stephe_leake.sal/build"
		       ":/Projects/org.stephe_leake.makerules"
		       ":/Projects/org.stephe_leake.aunit_ext/build"))
	 process-environment)))
      (eglot-workspace-configuration
       ;; This is sent in a workspace/didChangeConfiguration message.
       `((ada (projectFile . ,(expand-file-name "../ada_mode_wisi_parse.gpr"))))))

  (eglot 'ada-mode ;; managed-major-mode
	 (project-current) ;; project; project-root is server process directory
	 'eglot-lsp-server ;; class
	 'ada-eglot-find-server ;; contact
	 "Ada" ;; language-id
	 )
  nil)
;; end of file
