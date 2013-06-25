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
	    (start-indent (current-indentation)))
	(cond
	 ;; special cases
	 ;;
	 ((ada-in-paren-p)
	  ;; test/ada_mode-conditional_expressions.adb
	  ;; K2 : Integer := (if J > 42
          ;;                  then -1
   	  ;;   indenting 'then'; offset = 0
	  ;;
	  ;; test/ada_mode-parens.adb
	  ;; Local_2 : Integer := (1 + 2 +
	  ;;                         3);
	  ;;   indenting '3)'; 'offset' = ada-indent-broken
	  ;;
	  ;; test/ada_mode-parens.adb
	  ;; return Float (
	  ;;               Integer'Value
	  ;;                 (Local_6));
	  ;;   indenting '(local_6)'; 'offset' = ada-indent - 1
	  (+ (current-column) offset))

	 ;; all other structures
	 ((= (current-column) start-indent)
	  ;; start is at indentation
	  (cond
	   ((eq 'label_opt (wisi-cache-token start-cache))
	     (+ (current-column) (- ada-indent-label) offset))

	    (t
	     (+ start-indent offset))
	    ))

	 (t
	  ;; statement-start is preceded by something on same line. Handle common cases nicely.
	  (cond
	   ((eq 'select_alternative (wisi-cache-nonterm start-cache))
	    (wisi-goto-statement-start start-cache t)
	    (+ (current-column) offset ada-indent-when))

	   ((eq 'record_definition (wisi-cache-nonterm start-cache))
	    (wisi-goto-statement-start start-cache t) ;; type
	    (if (= (current-column) start-indent)
		;; 'record' on same line as 'type
		(+ (current-column) offset)
	      ;; 'record' on a line indented by ada-indent-broken
	      (+ (current-column) ada-indent-record-rel-type offset)))

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
		(+ start-indent offset))
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
  (let ((pos-0 (point))
	(cache (wisi-get-cache (point))))
    (when cache
      (ecase (wisi-cache-class cache)
	(block-start
	 (case (wisi-cache-token cache)
	   (IS ;; subprogram body
	    (ada-wisi-indent-statement-start 0 cache t t))

	   (RECORD
	    (ada-wisi-indent-statement-start ada-indent-record-rel-type cache t t))

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
	 (let ((prev-cache (save-excursion (wisi-backward-cache))))
	   (case (wisi-cache-token prev-cache)
	    (EQUAL_GREATER
	     ;; in aggregate; test/ada_mode-parens.adb
	     ;; (1 =>
	     ;;    (1 => 12,
	     ;;     2 => 13,
	     ;;     3 => 14),
	     ;;   indenting '(1 => 12'; statement-start is '1 =>'
	     (ada-wisi-indent-statement-start (1- ada-indent) prev-cache nil t))

	    (LEFT_PAREN
	     ;; nested aggregate: test/ada_mode-parens.adb
	     ;; ((1, 2, 3),
	     ;;  (4, 5, 6));
	     ;;   indenting '(4'; statement-start is '((1';
	     (ada-wisi-indent-statement-start 1 prev-cache nil t))

	    (t
	     ;; if in aggregate, statement-start is containing open-paren, and offset is ignored
	     ;; otherwise, indent 1 less than normal, so aggregate contents are indented normally
	     (ada-wisi-indent-statement-start (1- ada-indent) cache nil t))
	    )))

	(return-1;; parameter list
	 (let ((return-pos (point)))
	   (wisi-goto-statement-start cache nil) ;; matching 'function'
	   (cond
	    ((<= ada-indent-return 0)
	     ;; indent relative to "("
	     (wisi-forward-find-class 'open-paren return-pos)
	     (+ (current-column) (- ada-indent-return)))

	    (t
	     (+ (current-column) ada-indent-return))
	    )))

	(return-2;; no parameter list
	 (wisi-goto-statement-start cache nil) ;; matching 'function'
	 (+ (current-column) ada-indent-broken))

	(statement-end
	 (ada-wisi-indent-statement-start ada-indent-broken cache nil t))

	(statement-other
	 (save-excursion
	   (let ((start-cache (wisi-goto-statement-start cache nil)))
	     (case (wisi-cache-token cache)
	       (EQUAL_GREATER
		(+ (current-column) ada-indent-broken))

	       (RENAMES
		(wisi-forward-find-token '(FUNCTION PROCEDURE) pos-0)
		(let ((pos-subprogram (point))
		      (has-params
		       ;; FIXME: this is wrong for one return access
		       ;; function case: overriding function Foo
		       ;; return access Bar (...) renames ...;
		       (wisi-forward-find-token 'LEFT_PAREN pos-0 t)))
		  (if has-params
		   (if (<= ada-indent-renames 0)
		       ;; indent relative to paren
		       (+ (current-column) (- ada-indent-renames))
		     ;; else relative to line containing keyword
		     (goto-char pos-subprogram)
		     (+ (current-indentation) ada-indent-renames))

		   ;; no params
		   (goto-char pos-subprogram)
		   (+ (current-indentation) ada-indent-broken))
		  ))

	       (t
		(ecase (wisi-cache-nonterm start-cache)
		  (aggregate
		   ;; indenting 'with'
		   (+ (current-column) 1))

		  (asynchronous_select
		   ;; indenting 'abort'
		   (+ (current-column) ada-indent-broken))

		  (generic_renaming_declaration
		   ;; indenting keyword following 'generic'
		   (current-column))

		  (overriding_indicator_opt
		   (ecase (wisi-cache-token cache)
		     (OVERRIDING
		      ;; indenting 'overriding' following 'not'
		      (current-column))

		     ((PROCEDURE FUNCTION)
		      ;; indenting 'procedure' or 'function following 'overriding'
		      (current-column))
		     ))

		  (statement
		   (ecase (wisi-cache-token start-cache)
		     (label_opt
		      (+ (current-column) (- ada-indent-label)))
		     ))

		  )))))) ;; end statement-other

	(statement-start
	 (let ((prev-cache (save-excursion (wisi-backward-cache))))
	   (cond
	    ((eq 'access_definition (wisi-cache-nonterm cache))
	     (ada-wisi-indent-statement-start ada-indent-broken cache t t))

	    ((eq 'label_opt (wisi-cache-token cache))
	     (ada-wisi-indent-statement-start (+ ada-indent-label ada-indent) cache t t))

	    ((and prev-cache
		  (eq 'LEFT_PAREN (wisi-cache-token prev-cache)))
	     ;; test/ada_mode-parens.adb
	     ;; return Float (
	     ;;               Integer'Value
	     ;;   indenting 'Integer'
	     (wisi-indent-paren 1))

	    ((and prev-cache
		  (eq 'list-break (wisi-cache-class prev-cache)))
		  ;; test/ada_mode-generic_instantiation.ads
		  ;; function Function_1 is new Instance.Generic_Function
		  ;;   (Param_Type  => Integer,
		  ;;    Result_Type => Boolean,
		  ;;    Threshold   => 2);
		  ;;   indenting 'Result_Type'
		  (wisi-indent-paren 1))

	     (t
	      (let ((start-cache (wisi-statement-start-cache cache)))
		(case (wisi-cache-class start-cache)
		  (block-start
		   (ada-wisi-indent-statement-start ada-indent cache t t))

		  (t
		   ;; anything that looks like a procedure call is a statement-start
		   ;;
		   ;; test/ada_mode-generic_instatiation.ads
		   ;; procedure Procedure_6 is new
		   ;;   Instance.Generic_Procedure (Integer, Function_1);
		   ;;   indenting 'Instance'
		   (ada-wisi-indent-statement-start ada-indent-broken cache t t))
		  )))
	     )))

	(type
	 (ada-wisi-indent-statement-start ada-indent-broken cache t t))
	))
    ))

(defun ada-wisi-after-cache ()
  "Point is at indentation, not before a cached token. Find previous
cached token, return new indentation for point."
  (let ((start (point))
	(prev-token (save-excursion (wisi-backward-token)))
	(cache (wisi-backward-cache)))

    (cond
     ((not cache) ;; bob
	0)

     (t
      (while (memq (wisi-cache-class cache) '(name type))
	;; not useful for indenting
	(setq cache (wisi-backward-cache)))

      (ecase (wisi-cache-class cache)
	(block-end
	 ;; indenting block/subprogram name after 'end'
	 (wisi-indent-current ada-indent-broken))

	(block-middle
	 (case (wisi-cache-token cache)
	   (IS
	    (let ((start-cache (wisi-goto-statement-start cache nil)))
	      (case (wisi-cache-nonterm start-cache)
		(case_statement
		 ;; between 'case .. is' and first 'when'; most likely a comment
		 (current-column))

		(t
		 (+ (current-column) ada-indent))
		)))

	   ((THEN ELSE)
	    (let* ((start-cache (save-excursion (wisi-goto-statement-start cache nil)))
		   (indent
		    (ecase (wisi-cache-nonterm start-cache)
		      (asynchronous_select ada-indent)
		      (if_statement ada-indent)
		      (if_expression ada-indent-broken))))
	      (ada-wisi-indent-statement-start indent cache t)))

	   (WHEN
	    ;; between 'when' and '=>'
	    (+ (current-column) ada-indent-broken))

	   (t
	    ;; block-middle keyword may not be on separate line:
	    ;;       function Create (Model   : in Integer;
	    ;;                        Context : in String) return String is
	    (ada-wisi-indent-statement-start ada-indent cache nil))
	   ))

	(block-start
	 (case (wisi-cache-nonterm cache)
	   (exception_handler
	    ;; between 'when' and '=>'
	    (+ (current-column) ada-indent-broken))

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
	 (if (equal (point) (caddr prev-token))
	     ;; we are indenting the first token after the list-break; not hanging.
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
	      (association
	       ;; 2      =>
	       ;;   1 + 2 * 3,
	       ;;  indenting '1'
	       (+ (current-column) ada-indent-broken))

	      ((case_expression_alternative case_statement_alternative exception_handler)
	       ;; start is 'when'
	       (+ (current-column) ada-indent))

	      (generic_renaming_declaration
	       ;; not indenting keyword following 'generic'
	       (+ (current-column) ada-indent-broken))

	      ))

	   (WITH
	    ;; extension aggregate: test/ada_mode-nominal-child.adb
	    ;;      (Default_Parent with
	    ;;       10, 12.0, True);
	    ;;   indenting '10'; statement-start is '('
	    (ada-wisi-indent-statement-start 0 cache nil))

	   ;; otherwise just hanging
	   ((ACCEPT FUNCTION PROCEDURE RENAMES)
	    (back-to-indentation)
	    (+ (current-column) ada-indent-broken))

	  ))

	(statement-start
	 (case (wisi-cache-nonterm cache)
	   (with_clause
	    (+ (current-column) ada-indent-with))

	   (t
	    ;; hanging
	    (ada-wisi-indent-statement-start ada-indent-broken cache nil))))

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
	  (wisi-forward-cache)))
	start-pos)
    (if (ada-wisi-declarative-region-start-p cache)
	(wisi-forward-token t)
      (while (not done)
	(setq start-pos (point))
	(setq cache (wisi-goto-statement-start cache t t))
	(case (wisi-cache-nonterm cache)
	  (block_statement
	   (ecase (wisi-cache-token cache)
	     (DECLARE (wisi-forward-token t))
	     (BEGIN nil);; user will need to add a declare keyword
	     )
	   (setq done t))

	  (overriding_indicator_opt
	   (wisi-forward-find-token '(FUNCTION PROCEDURE) start-pos)
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
  (wisi-validate-cache (point))
  ;; (info "(elisp)Parser State" "*syntax-ppss*")
  (let ((parse-result (syntax-ppss)))
    (and (> (nth 0 parse-result) 0)
	 (eq 'formal_part
	     (wisi-cache-nonterm
	      (wisi-get-cache (nth 1 parse-result)))
	     ))))

(defun ada-wisi-scan-paramlist (begin end)
  "For `ada-scan-paramlist'."
  (wisi-validate-cache end)
  (goto-char begin)
  (let (token
	text
	identifiers
	(in-p nil)
	(out-p nil)
	(not-null-p nil)
	(access-p nil)
	(constant-p nil)
	(protected-p nil)
	(type nil)
	type-begin
	type-end
	(default nil)
	(default-begin nil)
	param
	paramlist
	(done nil))
    (while (not done)
      (let ((token-text (wisi-forward-token)))
	(setq token (nth 0 token-text))
	(setq text  (nth 1 token-text)))
      (cond
       ((equal token 'COMMA) nil);; multiple identifiers

       ((equal token 'COLON)
	;; identifiers done. find type-begin; there may be no mode
	(skip-syntax-forward " ")
	(setq type-begin (point))
	(save-excursion
	  (while (member (car (wisi-forward-token)) '(IN OUT NOT NULL ACCESS CONSTANT PROTECTED))
	    (skip-syntax-forward " ")
	    (setq type-begin (point)))))

       ((equal token 'IN) (setq in-p t))
       ((equal token 'OUT) (setq out-p t))
       ((and (not type-end)
	     (member token '(NOT NULL)))
	;; "not", "null" could be part of the default expression
	(setq not-null-p t))
       ((equal token 'ACCESS) (setq access-p t))
       ((equal token 'CONSTANT) (setq constant-p t))
       ((equal token 'PROTECTED) (setq protected-p t))

       ((equal token 'COLON_EQUAL)
	(setq type-end (save-excursion (backward-char 2) (skip-syntax-backward " ") (point)))
	(skip-syntax-forward " ")
	(setq default-begin (point))
	(wisi-forward-find-token 'SEMICOLON end t))

       ((member token '(SEMICOLON RIGHT_PAREN))
	(if (equal token 'RIGHT_PAREN)
	    ;; all done
	    (progn
	      (setq done t)
	      (when (not type-end) (setq type-end (1- (point))))
	      (when default-begin (setq default (buffer-substring-no-properties default-begin (1- (point)))))
	      )
	  ;; else semicolon - one param done
	  (when (not type-end) (setq type-end (1- (point))))
	  (when default-begin (setq default (buffer-substring-no-properties default-begin (1- (point)))))
	  )

	(setq type (buffer-substring-no-properties type-begin type-end))
	(setq param (list (reverse identifiers)
			  in-p out-p not-null-p access-p constant-p protected-p
			  type default))
	(if paramlist
	    (add-to-list 'paramlist param)
	  (setq paramlist (list param)))
	(setq identifiers nil
	      in-p nil
	      out-p nil
	      not-null-p nil
	      access-p nil
	      constant-p nil
	      protected-p nil
	      type nil
	      type-begin nil
	      type-end nil
	      default nil
	      default-begin nil))

       (t
	(when (not type-begin)
	  (if identifiers
	      (add-to-list 'identifiers text)
	    (setq identifiers (list text)))))
       ))
    paramlist))

(defun ada-wisi-which-function-1 (cache-skip keyword body)
  "used in `ada-wisi-which-function'."
  (let (region
	result
	(cache (wisi-forward-cache)))

    (while (< 0 cache-skip)
      (setq cache (wisi-forward-cache))
      (setq cache-skip (1- cache-skip)))

    (unless (eq 'name (wisi-cache-class cache))
      (error "%s is not a name" cache))

    (setq result (buffer-substring-no-properties (point) (+ (point) (wisi-cache-last cache))))

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
	(setq cache (wisi-backward-cache))
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
	      (overriding_indicator_opt
	       (wisi-forward-find-nonterm '(procedure_specification subprogram_specification) (point-max))
	       (setq result (ada-wisi-which-function-1 0 (wisi-forward-token t) nil)))

	      (package_specification
	       (setq result (ada-wisi-which-function-1 0 "package" t)))

	      (protected_body
	       (setq result (ada-wisi-which-function-1 0 "protected" nil)))

	      ((protected_type_declaration single_protected_declaration)
	       (setq result (ada-wisi-which-function-1 0 "protected" t)))

	      ((procedure_specification subprogram_body subprogram_specification)
	       (setq result (ada-wisi-which-function-1 0 (wisi-forward-token t) nil)))

	      (task_type_declaration
	       ;; FIXME: need test
	       (setq result (ada-wisi-which-function-1 1 "task" t)))

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
