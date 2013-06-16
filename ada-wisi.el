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

(defconst ada-wisi-class-list
  '(
    block-end
    block-middle ;; not start of statement
    block-start ;; start of block is start of statement
    close-paren
    list-break
    name
    open-paren
    return
    return-1
    return-2
    statement-end
    statement-other
    statement-start
    type
    ))

;;;; indentation

(defun ada-wisi-indent-statement-start (offset cache containing &optional before)
  "Return indentation of OFFSET plus statement-start for CACHE.
If at start of a statement and CONTAINING is non-nil, use start
of containing statement.
BEFORE should be t when called from ada-wisi-before-cache, nil otherwise."
  (save-excursion
    (cond
     ((markerp (wisi-cache-start cache))
      (let ((start-cache (wisi-goto-statement-start cache containing))
	    (indent (current-indentation)))
	(cond
	 ((ada-in-paren-p)
	  (ada-goto-open-paren 1)
	  (+ (current-column) offset))

	 ((= (current-column) (current-indentation))
	  (case (wisi-cache-token start-cache)
	    (label_opt
	     (+ (current-column) (- ada-indent-label) offset))

	    (t
	     (+ indent offset))
	    ))

	 (t
	  ;; statement-start is preceded by something on same line. Handle common cases nicely.
	  (cond
	   ((eq 'select_alternative (wisi-cache-nonterm start-cache))
	    (wisi-goto-statement-start start-cache t)
	    (+ (current-column) offset ada-indent-when))

	   (t
	    (let* (prev-column
		   (prev-cache
		    (save-excursion
		      (prog1
			  (wisi-goto-statement-start start-cache t)
			(setq prev-column (current-column))))))
	      (cond
	       ((eq 'label_opt (wisi-cache-token prev-cache))
		(+ prev-column (- ada-indent-label) offset))

	       (t
		(+ indent offset))
	       )))
	   ))
	 )))

     (t
      (cond
       ((ada-in-paren-p)
	(ada-goto-open-paren 1)
	(+ (current-column) offset))

       (t
	;; at outermost containing statement. If called from
	;; ada-wisi-before-cache, we want to ignore OFFSET (indenting
	;; 'package' in a package spec). If called from
	;; ada-wisi-after-cache, we want to include offset (indenting
	;; first declaration in the package).
	(if before 0 offset))
       ))
      )))

(defun ada-wisi-before-cache ()
  "Point is at indentation, before a cached token. Return new indentation for point."
  (let ((cache (wisi-get-cache (point)))
	top-class)
    (when cache
      (setq top-class (wisi-cache-class cache))
      (ecase top-class
	(block-start
	 (case (wisi-cache-token cache)
	   (IS ;; subprogram body
	    (ada-wisi-indent-statement-start 0 cache t t))

	   (t ;; other
	    (ada-wisi-indent-statement-start ada-indent cache t t))))

	(block-end
	 (ada-wisi-indent-statement-start 0 cache nil t))

	(block-middle
	 (case (wisi-cache-token cache)
	   (WHEN
	    (ada-wisi-indent-statement-start ada-indent-when cache nil t))

	   (t
	    (ada-wisi-indent-statement-start 0 cache nil t))
	   ))

	(close-paren (wisi-indent-paren 0))

	(name
	 (ada-wisi-indent-statement-start ada-indent-broken cache nil t))

	(open-paren
	 (ada-wisi-indent-statement-start ada-indent-broken cache nil t))

	((return-1;; parameter list
	  return-2);; no parameter list
	 (let ((return-pos (point))
	       (indent
		(if (and
		     (eq top-class 'return-2)
		     (<= ada-indent-return 0))
		    ada-indent-broken
		  ada-indent-return)))

	   (ada-prev-statement-keyword) ;; matching 'function'
	   (cond
	    ((and
	      (eq top-class 'return-1)
	      (<= ada-indent-return 0))
	     ;; indent relative to "("
	     (wisi-forward-find-class 'open-paren return-pos)
	     (+ (current-column) indent))

	    (t
	     (+ (current-column) ada-indent-broken))
	    )))

	(statement-end
	 (ada-wisi-indent-statement-start ada-indent-broken cache nil t))

	(statement-other
	 (cond
	  ((save-excursion
	     (let ((start-cache (wisi-goto-statement-start cache nil)))
	       (cond
		((eq 'asynchronous_select (wisi-cache-nonterm start-cache))
		 ;; indenting 'abort'
		 (+ (current-column) ada-indent-broken))

		((eq 'generic_renaming_declaration (wisi-cache-nonterm start-cache))
		 ;; indenting keyword following 'generic'
		 (current-column))

		((eq 'label_opt (wisi-cache-token start-cache))
		 (+ (current-column) (- ada-indent-label)))

		((eq 'select_alternative (wisi-cache-nonterm start-cache))
		 ;; indenting '=>'
		 (+ (current-column) ada-indent-broken))
	       ))))

	  (t nil);; else let after-cache handle it
	  ))

	(statement-start
	 (cond
	  ((eq 'label_opt (wisi-cache-token cache))
	   (ada-wisi-indent-statement-start (+ ada-indent-label ada-indent) cache t t))

	  (t
	   (ada-wisi-indent-statement-start ada-indent cache t t))
	  ))

	(type
	 (ada-wisi-indent-statement-start ada-indent-broken cache t t))
	))
    ))

(defun ada-wisi-after-cache ()
  "Point is at indentation, not before a cached token. Find previous
cached token, return new indentation for point."
  (let* ((start (point))
	 (prev-token (save-excursion (wisi-backward-token)))
	 (cache-region (wisi-backward-cache))
	 (cache (car cache-region)))

    (cond
     ((not cache) ;; bob
	0)

     (t
      (case (wisi-cache-class cache)
	((name type) ;; not useful for indenting
	 (setq cache-region (wisi-backward-cache))
	 (setq cache (car cache-region)))
	)

      (ecase (wisi-cache-class cache)
	(block-end
	 ;; indenting block/subprogram name after 'end'
	 (wisi-indent-current ada-indent-broken))

	(block-middle
	 (case (wisi-cache-token cache)
	   (WHEN
	    ;; between 'when' and '=>'
	    (+ (current-column) ada-indent-broken))

	   ((THEN ELSE)
	    (let* ((start-cache (save-excursion (wisi-goto-statement-start cache nil)))
		   (indent
		    (ecase (wisi-cache-nonterm start-cache)
		      (asynchronous_select ada-indent)
		      (if_statement ada-indent)
		      (if_expression ada-indent-broken))))
	      (ada-wisi-indent-statement-start indent cache t)))

	   (t
	    ;; block-middle keyword may not be on separate line:
	    ;;       function Create (Model   : in Integer;
	    ;;                        Context : in String) return String is
	    (ada-wisi-indent-statement-start ada-indent cache nil))
	   ))

	(block-start
	 (case (wisi-cache-nonterm cache)
	   (if_expression
	    (ada-wisi-indent-statement-start ada-indent-broken cache nil))

	   (select_alternative
	    (ada-wisi-indent-statement-start (+ ada-indent-when ada-indent-broken) cache nil))

	   (t ;; other; normal block statement
	    (ada-wisi-indent-statement-start ada-indent cache nil))
	   ))

	(close-paren
	 ;; hanging
	 (ada-wisi-indent-statement-start ada-indent-broken cache nil))

	(list-break
	 (if (equal (nth 1 cache-region) (cddr prev-token))
	     ;; prev-token has cache; not hanging
	     (wisi-indent-paren 1)
	   ;; else hanging
	   (wisi-indent-paren (+ 1 ada-indent-broken))))

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

	((return-1 return-2)
	 ;; hanging. Intent relative to line containing matching 'function'
	 (ada-prev-statement-keyword)
	 (back-to-indentation)
 	 (+ (current-column) ada-indent-broken))

	(statement-end
	 (ada-wisi-indent-statement-start 0 cache nil))

	(statement-other
	 (ecase (wisi-cache-token cache)
	   (COMMA
	    ;; between ',' and 'when'; must be indenting a comment
	    (ada-wisi-indent-statement-start ada-indent-when cache nil))

	   (EQUAL_GREATER
	    (ecase (wisi-cache-nonterm (wisi-goto-statement-start cache nil))
	      (block_statement
	       ;; in exception handler
	       (+ (current-column) ada-indent-when ada-indent))

	      (case_expression
	       ;; between '=>' and ','
	       (+ (current-column) ada-indent-when ada-indent))

	      (case_statement
	       (+ (current-column) ada-indent-when ada-indent))

	      (generic_renaming_declaration
	       ;; not indenting keyword following 'generic'
	       (+ (current-column) ada-indent-broken))

	      ))

	   ;; otherwise just hanging
	   ((ACCEPT FUNCTION PROCEDURE)
	    (back-to-indentation)
	    (+ (current-column) ada-indent-broken))

	  ))

	(statement-start
	 ;; hanging
	 (ada-wisi-indent-statement-start ada-indent-broken cache nil))

	)))
    ))

(defun ada-wisi-comment ()
  "Compute indentation of a comment. For `wisi-indent-functions'."
  ;; We know we are at the first token on a line.
  (when (looking-at comment-start-skip)

    ;; We are at a comment; indent to previous code or comment.
    (cond
     ((and ada-indent-comment-col-0
	   (= 0 (current-column)))
      0)

     ((or
       (save-excursion (forward-line -1) (looking-at "\\s *$"))
       (save-excursion (forward-comment -1)(not (looking-at comment-start))))
      ;; comment is after a blank line or code; indent as if code
      ;;
      ;; ada-wisi-before-cache will find the keyword _after_ the
      ;; comment, which could be a block-middle or block-end, and that
      ;; would align the comment with the block-middle, which is wrong. So
      ;; we only call ada-wisi-after-cache.

      (ada-wisi-after-cache))

      (t
       ;; comment is after a comment
       (forward-comment -1)
       (current-column))
      )))

;;;; ada-mode functions (alphabetical)

(defun ada-wisi-declarative-region-start-p (cache)
  "Return t if cache is a keyword starting a declarative region."
  (case (wisi-cache-token cache)
   (DECLARE t)
   (IS
    (memq (wisi-cache-class cache) '(block-middle)))
   (t nil)
   ))

(defun ada-wisi-goto-declarative-region-start ()
  "For `ada-goto-declarative-region-start', which see."
  (let ((done nil)
	(cache
	 (or
	  (wisi-get-cache (point))
	  ;; we use forward-cache here, to handle the case where point is after a subprogram declaration:
	  ;; declare
	  ;;     ...
	  ;;     function ... is ... end;
	  ;;     <point>
	  ;;     function ... is ... end;
	  (car (wisi-forward-cache)))))
    (if (ada-wisi-declarative-region-start-p cache)
	(wisi-forward-token t)
      (while (not done)
	(setq cache (wisi-goto-statement-start cache t t))
	(case (wisi-cache-nonterm cache)
	  (block_statement
	   (ecase (wisi-cache-token cache)
	     (DECLARE (wisi-forward-token t))
	     (BEGIN nil);; user will need to add a declare keyword
	     )
	   (setq done t))

	  (subprogram_body ;; overriding_indicator_opt preceding subprogram_specification
	   (ada-next-statement-keyword) ;; procedure/function
	   (ada-next-statement-keyword) ;; is
	   (wisi-forward-token t)
	   (setq done t))

	  ((package_body protected_body subprogram_specification task_body)
	   (ada-next-statement-keyword) ;; is
	   (wisi-forward-token t)
	   (setq done t))
	  )))
    ))

(defun ada-wisi-in-paramlist-p ()
  "For `ada-in-paramlist-p'."
  ;; (info "(elisp)Parser State" "*syntax-ppss*")
  (let ((parse-result (syntax-ppss)))
    (and (> (nth 0 parse-result) 0)
	 (eq 'formal_part
	     (wisi-cache-nonterm
	      (wisi-get-cache (nth 1 parse-result)))
	     ))))

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

	    (case (wisi-cache-nonterm cache)
	      (generic_formal_part
	       ;; name is after next statement keyword
	       (wisi-next-statement-cache cache)
	       (setq cache (wisi-get-cache (point))))
	      )

	    ;; add or delete 'body' as needed
	    (case (wisi-cache-nonterm cache)
	      (package_specification
	       (setq result (ada-wisi-which-function-1 0 "package" t)))

	      (protected_body
	       (setq result (ada-wisi-which-function-1 0 "protected" nil)))

	      (protected_type_declaration
	       (setq result (ada-wisi-which-function-1 1 "protected" t)))

	      (task_type_declaration
	       ;; FIXME: need test
	       (setq result (ada-wisi-which-function-1 1 "task" t)))

	      ((subprogram_body subprogram_specification)
	       (setq result (ada-wisi-which-function-1 0 (wisi-forward-token t) nil)))
	      ))))
      result)))

;;;; debugging
(defun ada-wisi-debug-keys ()
  "Add debug key definitions to `ada-mode-map'."
  (interactive)
  (define-key ada-mode-map "\M-j" 'wisi-show-cache)
  (define-key ada-mode-map "\M-k" 'wisi-show-token)
  (define-key ada-mode-map "\M-h"
    (lambda ()
      (interactive)
      (wisi-goto-statement-start (wisi-get-cache (point)) t)))
  )

(defun ada-wisi-debug-setup ()
  "Set up with debug grammar."
  (interactive)
  (wisi-setup '(ada-wisi-before-cache
		ada-wisi-after-cache)
	      debug-wy--keyword-table
	      debug-wy--token-table
	      debug-wy--parse-table)
  )

;;;###autoload
(defun ada-wisi-setup ()
  "Set up a buffer for parsing Ada files with wisi."
  (wisi-setup '(ada-wisi-comment
		ada-wisi-before-cache
		ada-wisi-after-cache)
	      ada-wisi-class-list
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
