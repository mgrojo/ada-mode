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
;;; code style
;;
;; I don't use 'pcase', because it gives _really_ confusing errors
;; when I forget a ')' somewhere. Even worse, the error message is
;; given when you use edebug on a defun, not when you eval it. This
;; code is hard enough to debug!
;;
;;;;

(require 'gpr-indent-user-options)
(require 'gpr-grammar-elisp)
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
    (let ((cache (wisi-backward-cache)))
      (while (and cache
		  (not (and
			(memq (wisi-cache-nonterm cache) '(package_spec simple_project_declaration))
			(eq (wisi-cache-class cache) 'statement-start))))
	(setq cache (wisi-goto-containing cache)))
      (when cache
	(wisi-forward-token); package | project
	(wisi-token-text (wisi-forward-token)); name
	))
    ))

;;; debugging
(defun gpr-wisi-debug-keys ()
  "Add debug key definitions to `gpr-mode-map'."
  (interactive)
  (define-key gpr-mode-map "\M-h" 'wisi-show-containing-or-previous-cache)
  (define-key gpr-mode-map "\M-j" 'wisi-show-cache)
  (define-key gpr-mode-map "\M-k" 'wisi-show-token)
  )

;;;;
(defun gpr-wisi-setup ()
  "Set up a buffer for parsing Ada files with wisi."
  (wisi-setup '()
	      nil
	      gpr-wisi-class-list
	      gpr-grammar-elisp-keyword-table
	      gpr-grammar-elisp-token-table
	      gpr-grammar-elisp-parse-table
	      nil nil)

  (setq gpr-indent-statement 'wisi-indent-statement)
  (set (make-local-variable 'comment-indent-function) 'wisi-comment-indent)
  )

(add-hook 'gpr-mode-hook 'gpr-wisi-setup)

(setq gpr-which-function 'gpr-wisi-which-function)

(setq gpr-show-parse-error 'wisi-show-parse-error)

(provide 'gpr-wisi)
(provide 'gpr-indent-engine)

;; end of file
