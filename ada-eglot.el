;;; ada-eglot.el --- Ada definitions for eglot -*- lexical-binding: t; -*-

(require 'ada-core)
(require 'eglot)
(require 'gnat-compiler)
(require 'wisi)

(defun ada-eglot-indent-line ()
  "For `indent-line-function'."
  (let ((savep (copy-marker (point)))
	(to-indent nil))
    (back-to-indentation)
    (when (>= (point) savep)
      (setq to-indent t))

    ;; (1+ line-end-pos) is needed to compute indent for a line. It
    ;; can exceed (point-max); the parser must be able to handle that.
    ;;
    (eglot-format (line-beginning-position) (1+ (line-end-position)))

    (goto-char savep)
    (when to-indent (back-to-indentation))
    ))

;;; xref interface, via wisi

;;; startup
(defun ada-eglot-require-eglot ()
  "Ensure eglot is started for the current project."
  (unless (eglot-current-server)
    (let ((prj (project-current))
	  (process-environment (copy-sequence process-environment))
	  eglot-workspace-configuration)

      (when (and (wisi-prj-p prj)
		 (gnat-compiler-p (wisi-prj-compiler prj)))
	(setq eglot-workspace-configuration
	      ;; This is sent in a workspace/didChangeConfiguration message.
	      (list (list 'ada (cons 'projectFile (gnat-compiler-gpr-file (wisi-prj-compiler prj))))))

	(setq process-environment
	      (append (wisi-prj-file-env prj) ;; for GPR_PROJECT_PATH
		      process-environment)))

      (when prj
	(eglot 'ada-mode 	 ;; managed-major-mode
	       prj 		 ;; project; project-root is server process directory
	       'eglot-lsp-server ;; class
	       'gnat-find-als 	 ;; contact
	       "Ada" 		 ;; language-id
	       )

	(when (eglot-current-server)
	  (cl-ecase ada-face-backend
	    (wisi nil)
	    (eglot
	     (unless (plist-get (oref (eglot-current-server) capabilities) :semanticTokensProvider)
	       (display-warning 'ada "LSP server does not support faces; change ada-face-backend")))
	    )

	  (cl-ecase ada-indent-backend
	    (wisi nil)
	    (eglot
	     ;; :documentFormattingProvider does the whole file at once; not
	     ;; useful for indent-region. IMPROVEME: in als devel version? fix it!
	     (unless (plist-get (oref (eglot-current-server) capabilities) :documentRangeFormattingProvider)
	       (display-warning 'ada "LSP server does not support line or range indent; change ada-indent-backend"))

	       (setq-local indent-line-function #'ada-eglot-indent-line)
	       (setq-local indent-region-function #'eglot-format)))
	    )

	;; We just assume the language server supports xref, completion
	))))

;;;###autoload
(defun ada-eglot-setup ()
  "Configure elgot settings for Ada."
  ;; Called from ada-mode when any ada-*-backend is eglot.  This can
  ;; fail to start eglot if there is no current project (which is
  ;; always the case in ada-mode unit tests).
  (ada-eglot-require-eglot)

  (cl-ecase ada-face-backend
    (wisi
     (setq-local wisi-disable-face nil))

    (eglot
     (setq-local wisi-disable-face t)
     ;; FIXME: tell font-lock to use eglot.

     ;; IMPROVEME: LSP defines Semantic Tokens for syntactic
     ;; highlighting/font-lock. Not supported in eglot.el 1.8,
     ;; ada_language_server 22.0. Supported in devel versions;
     ;; https://github.com/joaotavora/eglot/issues/615
     ;; https://github.com/AkibAzmain/eglot/tree/semantic-tokens
     ;; https://github.com/AdaCore/ada_language_server/issues/879
     ;;
     ;; (setq wisi-disable-face t)
     ;; (setq eglot-enable-semantic-tokens t)
     ;; see the default settings in eglot-semantic-token-faces
     ))

  (cl-ecase ada-indent-backend
    (wisi
     ;; wisi-setup does the work
     (setq-local wisi-disable-indent nil))

    (eglot
     (setq-local wisi-disable-indent t)

     (setq-local indent-line-function #'ada-eglot-indent-line)
     (setq-local indent-region-function #'eglot-format)))

  (cl-ecase ada-xref-backend
    ((gpr_query gnat)
     (setq-local eglot-stay-out-of (default-value 'eglot-stay-out-of))
     (add-to-list 'eglot-stay-out-of 'xref)
     (unless (memq #'wisi-prj-xref-backend xref-backend-functions)
       (add-hook 'xref-backend-functions #'wisi-prj-xref-backend 10 t)))

    (eglot
     ;; IMPROVEME: implement lsp backend for wisi-xref, take
     ;; advantage of ada_language_server extensions. waiting for
     ;; test cases.
     (add-hook 'xref-backend-functions #'eglot-xref-backend -10 t)

     ;; FIXME: don't bind C-c C-d? move to gpr-query minor mode
     ;; or implement via als extensions
     ))

  ;; eglot is always better at completion, so no separate user
  ;; config for this.
  (setq wisi-disable-completion t)

  (when (and wisi-disable-face
	     wisi-disable-indent
	     (eq ada-xref-backend 'eglot))
    (setq wisi-disable-parser t))
  )

(cl-defmethod wisi-compiler-select-prj :after ((_compiler gnat-compiler) _project)
  ;; Connect to the correct eglot instance; the gpr-file may have changed.
  (when (or (eq ada-xref-backend   'eglot)
	    (eq ada-indent-backend 'eglot)
	    (eq ada-face-backend   'eglot))
    (ada-eglot-require-eglot)))

;; create-ada-prj runs create-%s-xref, where %s is ada-xref-backend. So
;; we need create-eglot-xref. It returns nil, since we don't use any
;; wisi xref functions; just the ones provided by eglot. FIXME: except
;; wisi-xref-other.
;;;###autoload
(defun create-eglot-xref () nil)

(provide 'ada-eglot)
;;; ada-eglot.el ends here.
