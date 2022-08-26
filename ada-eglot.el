;;; ada-eglot.el --- Ada definitions for eglot -*- lexical-binding: t; -*-

(require 'eglot) ;; for eglot-stay-out-of
(require 'ada-mode) ;; ada-process-parse-exec, ada-xref-tool

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
This should be added to `find-file-hook' and
`after-change-major-mode-hook', before
`eglot--maybe-activate-editing-mode'."

  (when (eq major-mode 'ada-mode)
    ;; Improveme; add user-visible config for this stuff?
    ;; minor-modes that are all set to use wisi by default?
    ;; Or just let users copy and edit this?

    (setq-local wisi-disable-parser
	  (or wisi-disable-parser
	      (null (executable-find ada-process-parse-exec))))

    ;; LSP defines Semantic Tokens for syntactic
    ;; highlighting/font-lock. Not supported in eglot.el 1.8,
    ;; ada_language_server 22.0; supported in devel versions;
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
       (setq wisi-disable-indent t)
       (setq-local indent-line-function #'ada-eglot-indent-line)
       (setq-local indent-region-function #'eglot-format))
      )

    (cl-ecase ada-xref-tool
      ((gpr_query gnat)
       (add-to-list 'eglot-stay-out-of 'xref))

      (eglot
       (error "FIXME: implement lsp backend for wisi-xref"))
      )

    ;; eglot is always better at completion, so no separate user
    ;; config for this.
    (setq wisi-disable-completion t))
  )

(provide 'ada-eglot)
;;; ada-eglot.el ends here.
