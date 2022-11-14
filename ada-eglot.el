;;; ada-eglot.el --- Ada definitions for eglot -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022  Free Software Foundation, Inc.
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

(require 'ada-core)
(require 'ada-indent-user-options)
(require 'eieio)
(require 'eglot)
(require 'gnat-compiler)
(require 'wisi)

(defclass eglot-ada (eglot-lsp-server)
  ((indexing-done
    :accessor eglot-ada-indexing-done
    :initform nil))
  :documentation "AdaCore's ada_language_server.")

(cl-defmethod eglot-handle-notification ((server eglot-ada) (_method (eql $/progress))
   &key _token value &allow-other-keys)
  "Handle notification $/progress."
  (when (string= (plist-get value :kind) "end")
    (setf (eglot-ada-indexing-done server) t)))

(defun ada-eglot-wait-indexing-done ()
  (let ((server (eglot-current-server)))
    (message "waiting for ada_language_server indexing ...")
    (while (not (eglot-ada-indexing-done server))
      (accept-process-output))
    (message "waiting for ada_language_server indexing ... done")
    ))

(defun ada-eglot-indent-line ()
  "For `indent-line-function'."
  (let ((savep (copy-marker (point)))
	(to-indent nil)
	(tab-width ada-indent)
	;; https://github.com/AdaCore/ada_language_server/issues/1075
	;; eglot sets FormattingOptions tabSize to tab-width
	)
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

(defun ada-eglot-indent-region (begin end)
  "For `indent-region-function'."
  (let ((tab-width ada-indent)
	;; https://github.com/AdaCore/ada_language_server/issues/1075
	;; eglot sets FormattingOptions tabSize to tab-width
	)
    (eglot-format begin end)
    ))

;;; startup
(defvar ada-eglot-require-gpr nil
  ;; Default nil to allow newbies and small projects to run without a
  ;; gpr file; als uses a default.  unit tests can set t if the gpr
  ;; file is set after the file under test is opened.
)

(defun ada-eglot-require-eglot ()
  "Ensure eglot is started for the current project."
  (unless (eglot-current-server)
    (let* ((prj (eglot--current-project)) ;; provides a default if (current-project) is nil.
 	   (process-environment (copy-sequence process-environment))
	   (gpr-file
	    (when (and (wisi-prj-p prj)
		       (gnat-compiler-p (wisi-prj-compiler prj)))
	      (gnat-compiler-gpr-file (wisi-prj-compiler prj)))))

      (when (wisi-prj-p prj)
	(setq process-environment
	      (append (wisi-prj-file-env prj) ;; for GPR_PROJECT_PATH
		      process-environment)))

      (unless (and ada-eglot-require-gpr
		   (null gpr-file))

	(eglot 'ada-mode 	 ;; managed-major-mode
	       prj 		 ;; project; project-root is server process directory
	       'eglot-ada        ;; class
	       (if gpr-file
		   (list (gnat-find-als)
			 :initializationOptions (list (list :ada (cons 'projectFile gpr-file))))
		 (list (gnat-find-als)))   ;; contact IMPROVME: allow other servers?
	       "Ada" 		 ;; language-id
	       )

	(when (eglot-current-server)
	  (when (eq ada-face-backend 'eglot)
	    (unless (eglot--server-capable :semanticTokensProvider :range)
	      (display-warning 'ada "LSP server does not support faces; change ada-face-backend"))
	    (unless (boundp 'eglot-enable-semantic-tokens)
	      (display-warning 'ada "current version of eglot does not support faces"))
	    ))

	(when (eq ada-indent-backend 'eglot)
	  ;; :documentFormattingProvider does the whole file at once; not
	  ;; useful for indent-region. IMPROVEME: in als devel version? fix it!
	  (unless (plist-get (oref (eglot-current-server) capabilities) :documentRangeFormattingProvider)
	    (display-warning 'ada "LSP server does not support line or range indent; change ada-indent-backend"))

	  (setq-local indent-line-function #'ada-eglot-indent-line)
	  (setq-local indent-region-function #'ada-eglot-indent-region))
	;; We just assume the language server supports xref, completion
	))))

;;;###autoload
(defun ada-eglot-setup ()
  "Configure elgot settings for Ada."
  ;; Called from ada-mode when any ada-*-backend is eglot.

  (when (null ada-eglot-require-gpr)
    ;; The user is not using a project, so wisi-select-prj is not
    ;; called; start eglot now. See comment in ada-mode on startup
    ;; cases.
    (ada-eglot-require-eglot))

  (cl-ecase ada-face-backend
    (none
     (setq-local wisi-disable-face t))

    (eglot
     (setq-local wisi-disable-face t)
     (when (and (boundp 'eglot-enable-semantic-tokens)
		(boundp 'eglot-semantic-token-modifier-faces))
       (setq eglot-enable-semantic-tokens t)

       ;; It's tempting to delete defaultLibrary from the supported token
       ;; modifiers; there's no need to distinguish "Ada" from other
       ;; packages. But that's actually a user preference.
       ;;
       ;; In addition, als 23 raises CONSTRAINT_ERROR if we do that;
       ;; https://github.com/AdaCore/ada_language_server/issues/1070. So
       ;; we encourage the user to modify
       ;; eglot-semantic-token-modifier-faces instead.
       ))

    (wisi
     ;; wisi-setup does the work
     (setq-local wisi-disable-face nil))

    (other
     (setq-local wisi-disable-face t))
    )

  (cl-ecase ada-indent-backend
    (none
     (setq-local wisi-disable-indent t))

    (eglot
     (setq-local wisi-disable-indent t)

     (setq-local indent-line-function #'ada-eglot-indent-line)
     (setq-local indent-region-function #'eglot-format))

    (wisi
     ;; wisi-setup does the work
     (setq-local wisi-disable-indent nil))

    (other
     (setq-local wisi-disable-indent t))
    )

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

     ;; IMPROVEME: don't bind C-c C-d? move to gpr-query minor mode
     ;; or implement via als extensions
     )

    (other
     (setq-local wisi-disable-face t))
    )

  ;; eglot is always better at completion, so no separate user
  ;; config for this.
  (setq wisi-disable-completion t)

  (when (and wisi-disable-face
	     wisi-disable-indent
	     (not (eq ada-xref-backend 'wisi)))
    (setq wisi-disable-parser t)))

(cl-defmethod wisi-prj-select :after ((_project wisi-prj))
  ;; Connect to or create an eglot instance, providing a gpr file if
  ;; declared.
  (when (or (eq ada-xref-backend   'eglot)
	    (eq ada-indent-backend 'eglot)
	    (eq ada-face-backend   'eglot))
    (ada-eglot-require-eglot)))

(cl-defmethod wisi-prj-deselect :after ((_project wisi-prj))
  ;; Shutdown a corresponding eglot instance (defined in
  ;; eglot-current-server by combination of major-mode and
  ;; current-project), to allow gpr file and other settings to change.
  (when (eglot-current-server)
    (eglot-shutdown (eglot-current-server))))

;;;###autoload
(defun create-eglot-xref ()
;; ada-prj-make-xref calls create-%s-xref with no args, where %s is
;; ada-xref-backend.  FIXME: something else calls it with a :gpr-file
;; arg; that should set the gpr-file after creating the xref.
  'eglot)

;;; debugging
(defun ada-eglot-log-to-als-test (&optional prompt)
  "Convert EGLOT log in current buffer to an ada_language_server test.
Command file name defaults to \"new-test.json\"; with user arg,
prompt for it."
  (interactive "P")
  (let* ((test-buffer-name
	 (if prompt
	     (read-string "command file name: " "new-test.json")
	   "new-test.json"))
	(log-buffer (current-buffer))
	(test-buffer (get-buffer-create test-buffer-name)))
    (set-buffer test-buffer)
    (erase-buffer)

    (set-buffer test-buffer)
    (insert "[\n    {\"comment\": [\"\"]},\n")
    (insert "    {\"start\": {\"cmd\": [\"${ALS}\"]}},\n")

    (set-buffer log-buffer)
    (goto-char (point-min))

    (while (search-forward-regexp
	    (concat
	     "^\\(?:\\[client-request\\] (id:\\([0-9]+\\))\\)"
	     "\\|\\(?:\\[client-notification\\]\\)")
	    nil t)
      (let ((request-id (match-string 1))
	    begin msg)
	(forward-line 1)
	(setq begin (point))
	(forward-sexp)
	(setq msg (car (read-from-string (buffer-substring-no-properties begin (point)))))

	(set-buffer test-buffer)
	(insert "    {\"send\": {\n        \"request\": \n            ")
	(insert (json-encode msg))

	(set-buffer log-buffer)
	(forward-line 1)
	(if (and request-id
		 (looking-at (format "\\[server-reply\\] (id:%s)" request-id)))
	  (progn
	    (forward-line 1)
	    (setq begin (point))
	    (forward-sexp)
	    (setq msg (car (read-from-string (buffer-substring-no-properties begin (point)))))

	    (set-buffer test-buffer)
	    (insert ",\n        \"wait\" : [\n")
	    ;; IMPROVEME: initialize response in test includes log_filename, which eglot response does not.
	    ;; It also uses {} instead of null
	    ;; And even after fixing those, wait times out. So we simplify by hand until it works
	    ;; Also need to edit gpr file in didChangeConfiguration
	    ;; Also have to wait for indexing to complete.
	    (insert (json-encode msg))
	    (insert "]\n"))

	  ;; no wait
	  (set-buffer test-buffer)
	  (insert ",\n        \"wait\" : []"))

	(insert "}},\n")
	(set-buffer log-buffer)
	))

    (set-buffer test-buffer)
    (insert "   {\"stop\": {\"exit_code\": 0}}\n]\n")
    (if (buffer-file-name)
	(save-buffer)
      (write-file test-buffer-name))))

(provide 'ada-eglot)
;;; ada-eglot.el ends here.
