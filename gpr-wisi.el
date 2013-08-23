;;; An indentation engine for gpr mode, using the wisent LALR parser
;;
;; [1] GNAT user guide (info "gnat_ugn")
;;
;; Copyright (C) 2013 Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
;; Keywords: languages ada
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
;; not using lexical-binding or cl-lib because we support Emacs 23
;;
;; I don't use 'pcase', because it gives _really_ confusing errors
;; when I forget a ')' somewhere. Even worse, the error message is
;; given when you use edebug on a defun, not when you eval it. This
;; code is hard enough to debug!
;;
;;;;

;; we reuse some stuff from ada-mode
(require 'ada-indent-user-options)
(require 'gpr-grammar-wy)
(require 'gpr-mode)
(require 'wisi)

(defconst gpr-wisi-class-list
  '(
    block-start
    block-middle
    block-end
    close-paren
    open-paren
    statement-end
    statement-other
    statement-start
    ))

(defun gpr-wisi-indent-cache (offset cache)
  "Return indentation of OFFSET relative to CACHE or containing ancestor of CACHE that is at a line beginning."
  (let ((indent (current-indentation)))
    (while (and cache
		(not (= (current-column) indent)))
      (when (eq 'WHEN (wisi-cache-token cache))
	(setq offset (+ offset ada-indent-when)))
      (setq cache (wisi-goto-containing cache))
      (setq indent (current-indentation)))
  (+ (current-indentation) offset)
  ))

(defun gpr-wisi-indent-containing (offset cache)
  "Return indentation of OFFSET relative to containing ancestor of CACHE that is at a line beginning."
  (gpr-wisi-indent-cache offset (wisi-goto-containing cache)))

(defun gpr-wisi-before-cache ()
  (let ((cache (wisi-get-cache (point))))
    (when cache
      (ecase (wisi-cache-class cache)
	(block-start (wisi-indent-start ada-indent (wisi-backward-cache)))
	(block-end (wisi-indent-start 0 cache))
	(block-middle
	 (wisi-indent-start
	  (if (eq (wisi-cache-token cache) 'WHEN) ada-indent-when 0)
	  ;; FIXME (later): need test of ada-indent-when in gpr
	  cache))
	(close-paren (wisi-indent-paren 0))
	(open-paren nil); let after-keyword handle it
	(statement-start
	 (if (not (wisi-get-containing-cache cache))
	     ;; at bob
	     0
	   ;; not at bob
	   (gpr-wisi-indent-containing ada-indent cache)))
	))
    ))

(defun gpr-wisi-after-cache ()
  (let ((cache (wisi-backward-cache)))
    (if (not cache)
	;; bob
	0
      (ecase (wisi-cache-class cache)
	(block-end
	 (wisi-indent-current 0))

	(block-middle
	 (case (wisi-cache-token cache)
	   (WHEN
	    (gpr-wisi-indent-cache ada-indent-broken cache))
	   (t
	    (gpr-wisi-indent-cache ada-indent cache))
	   ))

	(block-start
	 (case (wisi-cache-token cache)
	   (EQUAL_GREATER
	    (gpr-wisi-indent-containing ada-indent cache))
	   (t
	    (gpr-wisi-indent-cache ada-indent cache))
	   ))

	(open-paren
	 (1+ (current-column)))

	(statement-end
	 (wisi-indent-start 0 cache))

	((statement-other close-paren)
	 ;; test/gpr/simple.gpr
	 ;; ) & Style_Checks
	 ;; & Standard_Common.Compiler'Default_Switches;
	 ;;
	 ;; for Source_Dirs use
	 ;;   ("../auto",
	 (wisi-indent-start ada-indent-broken cache))

	(statement-start
	 ;; test/gpr/simple.gpr
	 ;; type GNAT_Version_Type
	 ;;   is ("7.0.1",
	 ;; hanging
	 (gpr-wisi-indent-cache ada-indent-broken cache))
	))
    ))

(defun gpr-wisi-post-parse-fail ()
  "For `wisi-post-parse-fail-hook'."
  ;; keep it simple :)
  nil)

(defun gpr-wisi-which-function ()
  "For `gpr-which-function'."
  (wisi-validate-cache (point))
  (let ((cache (wisi-backward-cache)))
    (while (and cache
		(not (and
		      (memq (wisi-cache-nonterm cache) '(package_spec simple_project_declaration))
		      (eq (wisi-cache-class cache) 'statement-start))))
      (setq cache (wisi-goto-containing cache)))
    (when cache
      (wisi-forward-token); package | project
      (wisi-forward-token t); name
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
;;;###autoload
(defun gpr-wisi-setup ()
  "Set up a buffer for parsing Ada files with wisi."
  (wisi-setup '(gpr-wisi-before-cache
		gpr-wisi-after-cache)
	      'gpr-wisi-post-parse-fail
	      gpr-wisi-class-list
	      gpr-grammar-wy--keyword-table
	      gpr-grammar-wy--token-table
	      gpr-grammar-wy--parse-table)

  (set (make-local-variable 'comment-indent-function) 'wisi-comment-indent)
  (setq gpr-which-function 'gpr-wisi-which-function)
  )

(add-hook 'gpr-mode-hook 'gpr-wisi-setup)

(provide 'gpr-wisi)
(provide 'gpr-indent-engine)

;; end of file
