;;; An indentation engine for Ada mode, using the wisent LALR parser
;;
;; [1] ISO/IEC 8652:2012(E); Ada 2012 reference manual
;;
;; Copyright (C) 2012, 2013  Free Software Foundation, Inc.
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
;;; History: first experimental version Oct 2012
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

(require 'ada-indent-user-options)
(require 'ada-grammar-wy)
(require 'wisi)

(defun ada-wisi-before-keyword ()
  (let ((cache (wisi-get-cache (point)))
	top-class)
    (when cache
      (setq top-class (wisi-cache-class cache))
      (ecase top-class
	(block-start
	 (case (wisi-cache-symbol cache)
	   (IS ;; subprogram body
	    (wisi-indent-statement-start 0 cache t))

	   (t ;; other
	    (wisi-indent-statement-start ada-indent cache t))))

	((block-middle block-end) (wisi-indent-statement-start 0 cache nil))
	(close-paren (wisi-indent-paren 0))

	((open-paren statement-start)
	 ;; let after-keyword handle it
	 nil)

	(statement-middle;; FIXME: doc statement-middle vs block-middle
	 (wisi-indent-statement-start
	  (if (eq (wisi-cache-symbol cache) 'when) ada-indent-when 0)
	  cache
	  nil))

	((return-1;; parameter list
	  return-2);; no parameter list
	 (let ((indent
		(if (and
		     (eq top-class 'return-2)
		     (<= ada-indent-return 0))
		    ada-indent-broken
		  ada-indent-return)))

	   (setq cache (wisi-goto-statement-start cache nil))
	   (cond
	    ((and
	      (eq top-class 'return-1)
	      (<= ada-indent-return 0))
	     ;; indent relative to "("
	     (setq cache (car (wisi-forward-cache)))
	     (unless (eq 'open-paren (wisi-cache-class cache))
	       (error "unrecognized statement"))
	     (+ (current-column) indent))

	    (t ;; indent relative to "function".
	     (case (wisi-cache-symbol cache)
	       (formal_subprogram_declaration
		(wisi-forward-token t);; "with"
		(forward-comment (point-max))
		(+ (current-column) indent))

	       (subprogram_declaration
		(+ (current-column) indent))
	       )))
	   ))
	))
    ))

(defun ada-wisi-after-keyword ()
  (let ((start (point))
	(cache (car (wisi-backward-cache))))
    (if (not cache)
	;; bob
	0
      (case (wisi-cache-class cache)
	(block-end
	 (wisi-indent-current 0))

	((block-start block-middle)
	 (wisi-indent-current ada-indent))

	(list-break
	 (wisi-indent-paren 1))

	(open-paren
	 ;; 1) A parenthesized expression, or the first item in an aggregate:
	 ;;
	 ;;    (foo +
	 ;;       bar)
	 ;;    (foo =>
	 ;;       bar)
	 ;;
	 ;;     we are indenting 'bar'
	 ;;
	 ;; 2) A parenthesized expression, or the first item in an
	 ;;    aggregate, and there is a comment or whitespace between
	 ;;    ( and the first token:
	 ;;
	 ;;    (
	 ;;     foo + bar)
	 ;;    (
	 ;;     foo => bar)
	 ;;
	 ;;    we are indenting 'foo'
	 ;;
	 ;; We distinguish the two cases by going to the first token,
	 ;; and comparing point to start.
	 (let ((paren-column (current-column)))
	   (wisi-forward-token t); "("
	   (forward-comment (point-max))
	   (if (= (point) start)
	       ;; 2)
	       (1+ paren-column)
	     ;; 1)
	     (+ paren-column 1 ada-indent-broken))))

	(statement-end
	 (wisi-indent-statement-start 0 cache nil))

	(statement-middle;; when, else
	 (wisi-indent-current ada-indent))

	(t
	 ;; hanging
	 (wisi-indent-statement-start ada-indent-broken cache nil))
	))
    ))

(defun ada-wisi-which-function-1 (cache-skip keyword body)
  "used in `ada-wisi-which-function'."
  (let (region
	result
	(token (wisi-forward-cache)))

    (while (< 0 cache-skip)
      (setq token (wisi-forward-cache))
      (setq cache-skip (1- cache-skip)))

    (unless (eq 'name (wisi-cache-class (car token)))
      (error "%s is not a name token" token))

    (setq region (cadr token))
    (setq result (buffer-substring-no-properties (car region) (cadr region)))

    (when (not ff-function-name)
      (setq ff-function-name
	    (concat
	     keyword
	     (when body "\\s-+body")
	     "\\s-+"
	     result
	     ada-symbol-end)))
    result))

(defun ada-wisi-which-function ()
  "For `ada-which-function'."
  (wisi-validate-cache (point))
  (save-excursion
    (let ((result nil))

      (while (not result)
	(setq cache (car (wisi-backward-cache)))
	(if (null cache)
	    ;; bob
	    (setq result "")

	  ;; We are probably on the statement start we are looking
	  ;; for, so don't go to containing.
	  (setq cache (wisi-goto-statement-start cache nil))

	  (if (null cache)
	      ;; bob
	      (setq result "")

	    (case (wisi-cache-symbol cache)
	      (generic_formal_part
	       ;; name is after next statement keyword
	       (wisi-next-statement-cache cache)
	       (setq cache (wisi-get-cache (point))))
	      )

	    ;; add or delete 'body' as needed
	    (case (wisi-cache-symbol cache)
	      (package_specification
	       (setq result (ada-wisi-which-function-1 0 "package" t)))

	      (protected_type_declaration
	       (setq result (ada-wisi-which-function-1 1 "protected" t)))

	      (task_type_declaration
	       ;; FIXME: need test
	       (setq result (ada-wisi-which-function-1 1 "task" t)))

	      (subprogram_specification
	       ;; FIXME: function or procedure? get token, or store that in cache?
	       (setq result (ada-wisi-which-function-1 0 "function" nil)))
	      ))))
      result)))

;;; debugging
(defun ada-wisi-debug-keys ()
  "Add debug key definitions to `ada-mode-map'."
  (interactive)
  (define-key ada-mode-map "\M-j" 'wisi-show-cache)
  (define-key ada-mode-map "\M-k" 'wisi-show-token)
  )

(defun ada-wisi-debug-setup ()
  "Set up with debug grammar."
  (interactive)
  (wisi-setup '(ada-wisi-before-keyword
		ada-wisi-after-keyword)
	      debug-wy--keyword-table
	      debug-wy--token-table
	      debug-wy--parse-table)
  )

;;;###autoload
(defun ada-wisi-setup ()
  "Set up a buffer for parsing Ada files with wisi."
  (wisi-setup '(ada-wisi-before-keyword
		ada-wisi-after-keyword)
	      ada-grammar-wy--keyword-table
	      ada-grammar-wy--token-table
	      ada-grammar-wy--parse-table)

  (set (make-local-variable 'comment-indent-function) 'wisi-comment-indent)

  (font-lock-add-keywords nil
   ;; use keyword cache to distinguish between 'function ... return <type>;' and 'return ...;'
   (list
    (list
     (concat
      "\\<\\("
      "return[ \t]+access[ \t]+constant\\|"
      "return[ \t]+access\\|"
      "return"
      "\\)\\>[ \t]*"
      ada-name-regexp "?")
     '(1 font-lock-keyword-face)
     '(2 (if (eq (when (not (ada-in-string-or-comment-p))
		   (wisi-validate-cache (match-end 2))
		   (and (wisi-get-cache (match-beginning 2))
			(wisi-cache-class (wisi-get-cache (match-beginning 2)))))
		 'type)
	     font-lock-type-face
	   'default)
	 nil t)
     )))

  (set (make-local-variable 'ada-which-function) 'ada-wisi-which-function)
  (set (make-local-variable 'ada-in-paramlist-p) 'ada-wisi-in-paramlist-p)
  (set (make-local-variable 'ada-scan-paramlist) 'ada-wisi-scan-paramlist)
  (set (make-local-variable 'ada-goto-declaration-start) 'ada-wisi-goto-declaration-start)
  (set (make-local-variable 'ada-goto-declarative-region-start) 'ada-wisi-goto-declarative-region-start)
  (set (make-local-variable 'ada-next-statement-keyword) 'wisi-forward-statement-keyword)
  (set (make-local-variable 'ada-prev-statement-keyword) 'wisi-backward-statement-keyword)
  (set (make-local-variable 'ada-make-subprogram-body) 'ada-wisi-make-subprogram-body)
  )

(add-hook 'ada-mode-hook 'ada-wisi-setup)

(provide 'ada-wisi)
(provide 'ada-indent-engine)

;; end of file
