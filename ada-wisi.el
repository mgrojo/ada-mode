;;; ada-wisi.el --- Indentation engine for Ada mode, using the wisi generalized LALR parser  -*- lexical-binding:t -*-
;;
;; [1] ISO/IEC 8652:2012(E); Ada 2012 reference manual
;;
;; Copyright (C) 2012 - 2019  Free Software Foundation, Inc.
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
;;;;

;; DO NOT add ada-core here; eventually merge this into that.
(require 'ada-indent-user-options)
(require 'cl-lib)
(require 'wisi)
(require 'wisi-process-parse)

;;;; ada-mode functions (alphabetical)

(defun ada-wisi-declarative-region-start-p (cache)
  "Return t if cache is a keyword starting a declarative region."
  (memq (wisi-cache-token cache) '(DECLARE IS PRIVATE))
  ;; IS has a cache only if start of declarative region
  )

(defun ada-wisi-context-clause ()
  "For `ada-fix-context-clause'."
  (wisi-validate-cache (point-min) (point-max) t 'navigate)
  (save-excursion
    (goto-char (point-min))
    (let ((begin nil)
	  (end nil)
	  cache)

      (while (not end)
	(setq cache (wisi-forward-cache))
	(cl-case (wisi-cache-nonterm cache)
	  (pragma_g (wisi-goto-end-1 cache))
	  (use_clause (wisi-goto-end-1 cache))
	  (with_clause
	   (when (not begin)
	     (setq begin (line-beginning-position)))
	   (wisi-goto-end-1 cache))
	  (t
	   ;; start of compilation unit
	   (setq end (line-beginning-position))
	   (unless begin
	     (setq begin end)))
	  ))
      (cons begin end)
    )))

(defun ada-wisi-on-context-clause ()
  "For `ada-on-context-clause'."
  (let (cache)
    (save-excursion
      ;; Don't require parse of large file just for ada-find-other-file
      (and (< (point-max) wisi-size-threshold)
	   (setq cache (wisi-goto-statement-start))
	   (memq (wisi-cache-nonterm cache) '(use_clause with_clause))
	   ))))

(defun ada-wisi-in-case-expression ()
  "For `ada-in-case-expression'."
  (save-excursion
    ;; Used by ada-align; we know we are in a paren.
    (ada-goto-open-paren 1)
    (while (forward-comment 1))
    (looking-at "case")))

(defun ada-wisi-goto-subunit-name ()
  "For `ada-goto-subunit-name'."
  (wisi-validate-cache (point-min) (point-max) t 'navigate)

  (let (cache
	(name-pos nil))
    (save-excursion
      ;; move to top declaration
      (goto-char (point-min))
      (setq cache (or (wisi-get-cache (point))
		      (wisi-forward-cache)))

      (when (eq (wisi-cache-nonterm cache) 'subunit)
	(setq name-pos (car (wisi-next-name-region))))
      )
    (when name-pos
      (goto-char name-pos))
    ))

(defun ada-wisi-goto-declaration-start (&optional include-type)
  "For `ada-goto-declaration-start', which see.
Also return cache at start."
  (wisi-validate-cache (point-min) (point-max) t 'navigate)
  (ada-wisi-goto-declaration-start-1 include-type))

(defun ada-wisi-goto-declaration-start-1 (include-type)
  "Subroutine of `ada-wisi-goto-declaration-start'."
  (let ((start (point))
	(cache (wisi-get-cache (point)))
	(done nil))
    (unless cache
      (setq cache (wisi-backward-cache)))
    ;; cache is null at bob
    (while (not done)
      (if cache
	  (progn
	    (setq done
		  (cl-case (wisi-cache-nonterm cache)
		    ((entry_body entry_declaration)
		     (eq (wisi-cache-token cache) 'ENTRY))

		    (full_type_declaration
		     (when include-type
		       (eq (wisi-cache-token cache) 'TYPE)))

		    ((generic_package_declaration generic_subprogram_declaration)
		     (eq (wisi-cache-token cache) 'GENERIC))

		    ((package_body package_declaration)
		     (eq (wisi-cache-token cache) 'PACKAGE))

		    ((protected_body protected_type_declaration single_protected_declaration)
		     (eq (wisi-cache-token cache) 'PROTECTED))

		    ((abstract_subprogram_declaration
		      expression_function_declaration
		      subprogram_body
		      subprogram_declaration
		      subprogram_renaming_declaration
		      null_procedure_declaration)
		     (memq (wisi-cache-token cache) '(NOT OVERRIDING FUNCTION PROCEDURE)))

		    ((single_task_declaration task_body task_type_declaration)
		     (eq (wisi-cache-token cache) 'TASK))

		    ))
	    (unless (< start (wisi-cache-end cache))
	      ;; found declaration does not include start; find containing one.
	      (setq done nil))
	    (unless done
	      (setq cache (wisi-goto-containing cache nil))))
	(setq done t))
	)
    cache))

(defun ada-wisi-goto-declaration-end ()
  "For `ada-goto-declaration-end', which see."
  ;; first goto-declaration-start, so we get the right end, not just
  ;; the current statement end.
  (wisi-goto-end-1 (ada-wisi-goto-declaration-start)))

(defun ada-wisi-goto-declarative-region-start ()
  "For `ada-goto-declarative-region-start', which see."
  (wisi-validate-cache (point-min) (point-max) t 'navigate)

  (let ((done nil)
	start-pos
	(in-package-spec nil)
	(cache (or (wisi-get-cache (point))
		   (wisi-backward-cache))))

    ;; We use backward-cache, not forward-cache, to handle the case
    ;; where point is in the whitespace or comment before a block; we
    ;; want the containing block, not the next block.

    (when cache ;; nil at bob
      ;; If this is called with point in a comment after 'is', then the
      ;; declarative region starts after the comment; don't hang in a
      ;; package spec.
      (setq start-pos (point))
      (while (not done)
	(if (and (or (not in-package-spec)
		     (< (point) start-pos))
		 (ada-wisi-declarative-region-start-p cache))
	    (progn
	      (forward-word);; past 'is'
	      (setq done t))
	  (cl-case (wisi-cache-class cache)
	    (motion
	     (setq cache (wisi-goto-containing cache)));; statement-start

	    (statement-end
	     (setq cache (wisi-goto-containing cache)) ;; statement-start
	     (cl-case (wisi-cache-nonterm cache)
	       ((generic_package_declaration
		 package_declaration
		 entry_body package_body package_declaration protected_body subprogram_body task_body
		 protected_type_declaration single_protected_declaration single_task_declaration task_type_declaration)
		;; This is a block scope before the starting point; we want the containing scope
		(setq cache (wisi-goto-containing cache)))

	       (t
		nil)
	       ))

	    (statement-start
	     (cl-case (wisi-cache-nonterm cache)
	       (generic_package_declaration
		(setq in-package-spec t)
		(setq cache (wisi-next-statement-cache cache)) ;; 'package'
		(setq cache (wisi-next-statement-cache cache))) ;; 'is'

	       (package_declaration
		(setq in-package-spec t)
		(setq cache (wisi-next-statement-cache cache))) ;; 'is'

	       ((entry_body package_body package_declaration protected_body subprogram_body task_body)
		(while (not (eq 'IS (wisi-cache-token cache)))
		  (setq cache (wisi-next-statement-cache cache))))

	       ((protected_type_declaration single_protected_declaration single_task_declaration task_type_declaration)
		(while (not (eq 'IS (wisi-cache-token cache)))
		  (setq cache (wisi-next-statement-cache cache)))
		(when (looking-at "\<new\>")
		  (while (not (eq 'WITH (wisi-cache-token cache)))
		    (setq cache (wisi-next-statement-cache cache)))))

	       (t
		(setq cache (wisi-goto-containing cache t)))
	       ))

	    (t
	     (setq cache (wisi-goto-containing cache t)))
	    )))

      ;; point is at start of first code statement after
      ;; declaration-start keyword and comment; move back to end of
      ;; keyword.
      (while (forward-comment -1))
      )))

(defun ada-wisi-in-paramlist-p (&optional parse-result)
  "For `ada-in-paramlist-p'."
  (wisi-validate-cache (point-min) (point-max) nil 'navigate)
  ;; (info "(elisp)Parser State" "*syntax-ppss*")
  (let ((parse-result (or parse-result (syntax-ppss)))
	 cache)
    (and (> (nth 0 parse-result) 0)
	 ;; cache is nil if the parse failed
	 (setq cache (wisi-get-cache (nth 1 parse-result)))
	 (eq 'formal_part (wisi-cache-nonterm cache)))
    ))

(defun ada-wisi-make-subprogram-body ()
  "For `ada-make-subprogram-body'."
  ;; point is at start of subprogram specification;
  ;; ada-wisi-expand-region will find the terminal semicolon.
  (wisi-validate-cache (point-min) (point-max) t 'navigate)

  (let* ((begin (point))
	 (end (wisi-cache-end (wisi-get-cache (point))))
	 (name (wisi-next-name)))
    (goto-char end)
    (newline)
    (insert " is begin\n\nend ");; legal syntax; parse does not fail
    (insert name)
    (forward-char 1)

    ;; newline after body to separate from next body
    (newline-and-indent)
    (indent-region begin (point))
    (forward-line -2)
    (back-to-indentation)
    ))

(defun ada-wisi-which-function-1 (keyword add-body)
  "Used in `ada-wisi-which-function'."
  (let* ((result (wisi-next-name)))

    ;; See comment in ada-mode.el ada-which-function on why we don't
    ;; overwrite ff-function-name.
    (when (not ff-function-name)
      (setq ff-function-name
	    (concat
	     keyword
	     (when add-body "\\s-+body")
	     "\\s-+"
	     result
	     "\\_>")))
    result))

(defun ada-wisi-which-function (include-type)
  "For `ada-which-function'."
  ;; Fail gracefully and silently, since this could be called from
  ;; which-function-mode.
  (let ((parse-begin (max (point-min) (- (point) (/ ada-which-func-parse-size 2))))
	(parse-end   (min (point-max) (+ (point) (/ ada-which-func-parse-size 2)))))
    (save-excursion
      (condition-case nil
	  ;; Throwing an error here disables which-function-mode, so don't do it.
	  (progn
	    (wisi-validate-cache parse-begin parse-end nil 'navigate)
	    (when (wisi-cache-covers-region parse-begin parse-end 'navigate)
	      (let ((result nil)
		    (cache (ada-wisi-goto-declaration-start-1 include-type)))
		(if (null cache)
		    ;; bob or failed parse
		    (setq result "")

		  (when (memq (wisi-cache-nonterm cache)
			      '(generic_package_declaration generic_subprogram_declaration))
		    ;; name is after next statement keyword
		    (setq cache (wisi-next-statement-cache cache)))

		  ;; add or delete 'body' as needed
		  (cl-ecase (wisi-cache-nonterm cache)
		    ((entry_body entry_declaration)
		     (setq result (ada-wisi-which-function-1 "entry" nil)))

		    (full_type_declaration
		     (setq result (ada-wisi-which-function-1 "type" nil)))

		    (package_body
		     (setq result (ada-wisi-which-function-1 "package" nil)))

		    ((package_declaration
		      package_specification) ;; after 'generic'
		     (setq result (ada-wisi-which-function-1 "package" t)))

		    (protected_body
		     (setq result (ada-wisi-which-function-1 "protected" nil)))

		    ((protected_type_declaration single_protected_declaration)
		     (setq result (ada-wisi-which-function-1 "protected" t)))

		    ((abstract_subprogram_declaration
		      expression_function_declaration
		      subprogram_declaration
		      subprogram_renaming_declaration
		      generic_subprogram_declaration ;; after 'generic'
		      null_procedure_declaration)
		     (setq result (ada-wisi-which-function-1
				   (progn (search-forward-regexp "function\\|procedure")(match-string 0))
				   nil))) ;; no 'body' keyword in subprogram bodies

		    (subprogram_body
		     (setq result (ada-wisi-which-function-1
				   (progn (search-forward-regexp "function\\|procedure")(match-string 0))
				   nil)))

		    ((single_task_declaration task_type_declaration)
		     (setq result (ada-wisi-which-function-1 "task" t)))


		    (task_body
		     (setq result (ada-wisi-which-function-1 "task" nil)))
		    ))
		result)))
	(error "")))
    ))

(setq ada-fix-context-clause 'ada-wisi-context-clause)
(setq ada-goto-declaration-end 'ada-wisi-goto-declaration-end)
(setq ada-goto-declaration-start 'ada-wisi-goto-declaration-start)
(setq ada-goto-declarative-region-start 'ada-wisi-goto-declarative-region-start)
(setq ada-goto-end 'wisi-goto-statement-end)
(setq ada-goto-subunit-name 'ada-wisi-goto-subunit-name)
(setq ada-in-case-expression 'ada-wisi-in-case-expression)
(setq ada-in-paramlist-p 'ada-wisi-in-paramlist-p)
(setq ada-indent-statement 'wisi-indent-statement)
(setq ada-make-subprogram-body 'ada-wisi-make-subprogram-body)
(setq ada-on-context-clause 'ada-wisi-on-context-clause)
(setq ada-reset-parser 'wisi-reset-parser)
(setq ada-show-parse-error 'wisi-show-parse-error)
(setq ada-which-function 'ada-wisi-which-function)

(provide 'ada-wisi)
;; end of file
