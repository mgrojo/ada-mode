;;; ada-eglot.el --- Ada definitions for eglot -*- lexical-binding: t; -*-

(require 'eglot) ;; for eglot-stay-out-of
(require 'ada-mode) ;; ada-process-parse-exec, ada-xref-tool
(require 'wisi) ;; wisi-disable-parser

(defcustom ada-indent-engine 'wisi
  "Indent engine to use for Ada; wisi or eglot."
  :type 'symbol
  :options '(wisi eglot)
  :group 'ada)

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

;;;###autoload
(defun ada-eglot-setup ()
  "Configure elgot settings for Ada.
This should be added to `ada-mode-hook' when using ada-mode with eglot."
  ;; find-file runs find-file-noselect-1
  ;; which runs set-auto-mode
  ;; which runs ada-mode
  ;;     which sets wisi-disable-parser t if ada-process-parse-exec not found
  ;; then ada-mode-hook
  ;;     wisi-disable-parser can be trusted here
  ;; then after-change-major-mode-hook
  ;;     which runs eglot--maybe-activate-editing-mode, which could start eglot
  ;;         but we need to start eglot where we know the gpr file, to bind eglot-workspace-configuration
  ;;         so that's done in the per-project prj-eglot.el
  ;;         Which is also where the wisi parser is configured
  ;;         if eglot is already running a session for this root directory,
  ;;            eglot--maybe-activate-editing-mode just connects to it.
  ;; then runs find-file-hook
  ;;     which runs eglot--maybe-activate-editing-mode again

  (when (eq major-mode 'ada-mode)
    ;; IMPROVEME; add user-visible config for this stuff?
    ;; minor-modes that are all set to use wisi by default?
    ;; Or just let users copy and edit this?

    ;; We don't need a warning here; user has already thought about
    ;; this, since they added ada-eglot-setup to ada-mode-hook.
    (setq wisi-disable-parser
	  (or wisi-disable-parser
	      (null (executable-find ada-process-parse-exec))))

    ;; IMPROVEME: LSP defines Semantic Tokens for syntactic
    ;; highlighting/font-lock. Not supported in eglot.el 1.8,
    ;; ada_language_server 22.0. Supported in devel versions;
    ;; https://github.com/joaotavora/eglot/issues/615
    ;; https://github.com/AkibAzmain/eglot/tree/semantic-tokens
    ;; https://github.com/AdaCore/ada_language_server/issues/879
    ;;
    ;; (setq wisi-disable-face t)
    ;; (setq eglot-enable-semantic-tokens t)
    ;; ;; see the default settings in eglot-semantic-token-faces
    (setq-local wisi-disable-face nil)

    (cl-ecase ada-indent-engine
      (wisi
       ;; wisi-setup does the work
       (setq-local wisi-disable-indent nil))

      (eglot
       (setq-local wisi-disable-indent t)

       ;; :documentFormattingProvider does the whole file at once; not
       ;; useful for indent-region
       (when (and (eglot-current-server)
		  (plist-get (oref (eglot-current-server) capabilities) :documentRangeFormattingProvider))
	 (setq-local indent-line-function #'ada-eglot-indent-line)
	 (setq-local indent-region-function #'eglot-format)))
      )

    (cl-ecase ada-xref-tool
      ((gpr_query gnat)
       (setq-local eglot-stay-out-of (default-value 'eglot-stay-out-of))
       (add-to-list 'eglot-stay-out-of 'xref)
       (unless (memq #'wisi-prj-xref-backend xref-backend-functions)
	 (add-hook 'xref-backend-functions #'wisi-prj-xref-backend 10 t)))

      (eglot
       ;; IMPROVEME: implement lsp backend for wisi-xref, take
       ;; advantage of ada_language_server extensions.
       (add-hook 'xref-backend-functions #'eglot-xref-backend -10 t)

       ;; FIXME: don't bind C-c C-d? move to gpr-query minor mode
       ;; or implement via als extensions
       ))

    ;; eglot is always better at completion, so no separate user
    ;; config for this.
    (setq wisi-disable-completion t))
  )

;; create-ada-prj runs create-%s-xref, where %s is ada-xref-tool. So
;; we need create-eglot-xref. It returns nil, since we don't use any
;; wisi xref functions; just the ones provided by eglot. FIXME: except
;; wisi-xref-other.
;;;###autoload
(defun create-eglot-xref () nil)

(provide 'ada-eglot)
;;; ada-eglot.el ends here.
