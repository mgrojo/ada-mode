;; gpr-wisi.el --- Indentation engine for gpr mode, using the wisi parser  -*- lexical-binding:t -*-
;;
;; Copyright (C) 2013 - 2017 Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
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
;;
;;; History: first version Jan 2013
;;
;;;;

(require 'gpr-indent-user-options)
(require 'gpr-mode)
(require 'wisi)

(defconst gpr-wisi-class-list
  '(
    motion
    statement-end
    statement-start
    ))

(defun gpr-wisi-which-function ()
  "For `gpr-which-function'."
  (wisi-validate-cache (point) nil 'navigate)
  ;; no message on parse fail, since this could be called from which-func-mode
  (when (> (wisi-cache-max 'navigate) (point))
    (let ((cache (wisi-backward-cache))
	  done
	  project-pos
	  package-pos
	  decl-pos)
      (while (and cache (not done))
	;; find attribute_declaration and package containing point (if any)
	(cond
	 ((not (eq (wisi-cache-class cache) 'statement-start))
	  nil)

	 ((eq (wisi-cache-nonterm cache) 'attribute_declaration)
	  (setq decl-pos (point)))

	 ((eq (wisi-cache-nonterm cache) 'package_spec)
	  (setq package-pos (point))
	  (setq done t))

	 ((eq (wisi-cache-nonterm cache) 'simple_project_declaration)
	  (setq project-pos (point))
	  (setq done t))
	 )

	(setq cache (wisi-goto-containing cache)))

      (cond
       (package-pos
	(goto-char package-pos)
	(setq done t))

       (decl-pos
	(goto-char decl-pos)
	(setq done t))

       (project-pos
	(goto-char project-pos)
	(setq done t))

       (t ;; before project
	(setq done nil))
       )

      (when done
	(wisi-forward-token); keyword
	(wisi-token-text (wisi-forward-token))); name

      )))

;;; debugging
(defun gpr-wisi-debug-keys ()
  "Add debug key definitions to `gpr-mode-map'."
  (interactive)
  (define-key gpr-mode-map "\M-h" 'wisi-show-containing-or-previous-cache)
  (define-key gpr-mode-map "\M-j" 'wisi-show-cache)
  (define-key gpr-mode-map "\M-k" 'wisi-show-token)
  )

;;;;

(defvar gpr_grammar-elisp-parse-table nil) ;; gpr_grammar-elisp.el
(defvar gpr_grammar-elisp-token-table-raw nil) ;; gpr_grammar-elisp.el and gpr_grammar-process.el
(defvar gpr_grammar-elisp-keyword-table-raw nil) ;; gpr_grammar-elisp.el and gpr_grammar-process.el
(defvar gpr_grammar-process-action-table nil) ;; gpr_grammar-process.el
(defvar gpr_grammar-process-token-table nil) ;;gpr_grammar-process.el

(defun gpr-wisi-setup ()
  "Set up a buffer for parsing gpr files with wisi."
  (wisi-setup
   :indent-calculate nil
   :post-indent-fail nil
   :class-list gpr-wisi-class-list
   :parser
   (cond
    ((or (null gpr-parser)
	 (eq 'elisp gpr-parser))
     (require 'gpr_grammar-elisp)
     (wisi-make-elisp-parser
      gpr_grammar-elisp-parse-table
      #'wisi-forward-token))

    ((eq 'process gpr-parser)
     (require 'gpr_grammar-process)
     (wisi-make-process-parser
      :label "gpr"
      :exec gpr-process-parse-exec
      :token-table (nth 0 gpr_grammar-process-token-table)
      :action-table (nth 0 gpr_grammar-process-action-table)
      :terminal-hashtable (nth 1 gpr_grammar-process-token-table)))
    )

   :lexer (wisi-make-elisp-lexer
	   :token-table-raw gpr_grammar-elisp-token-table-raw
	   :keyword-table-raw gpr_grammar-elisp-keyword-table-raw
	   :string-quote-escape-doubled nil
	   :string-quote-escape nil))

  (setq gpr-indent-statement 'wisi-indent-statement)
  (set (make-local-variable 'comment-indent-function) 'wisi-comment-indent)
  )

(add-hook 'gpr-mode-hook 'gpr-wisi-setup)

(setq gpr-which-function 'gpr-wisi-which-function)

(setq gpr-show-parse-error 'wisi-show-parse-error)

(provide 'gpr-wisi)

;; end of file
