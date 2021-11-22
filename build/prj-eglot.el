;; set up eglot for the emacs ada-mode project

(require 'ada-eglot-mode)

;; Don't use ada-mode for Ada buffers; pure eglot.
(if (assoc "\\.ad[abs]\\'" auto-mode-alist)
    (setcdr (assoc "\\.ad[abs]\\'" auto-mode-alist) #'ada-eglot-mode)
  (setq auto-mode-alist (append auto-mode-alist (list (cons "\\.ad[sb]" #'ada-eglot-mode)))))

(setq ada-eglot-gnat-prj-file "/Projects/org.emacs.ada-mode/ada_mode_wisi_parse.gpr")

(setq-default
 eglot-workspace-configuration
 ;; FIXME: how do we do this in a .dir-local.el or let-binding? post
 ;; answer in an eglot discussion
 ;;
 ;; This is sent in a workspace/didChangeConfiguration message.
 `((ada (projectFile . ,ada-eglot-gnat-prj-file))))

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
	 process-environment))))

  (eglot 'ada-eglot-mode ;; managed-major-mode
	 'ada-eglot-project ;; project; project-root is server process directory
	 'eglot-lsp-server ;; class
	 'ada-eglot-find-server ;; contact
	 "Ada" ;; language-id
	 )
  nil)
;; end of file
