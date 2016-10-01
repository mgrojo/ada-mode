;;; ada-wisi.el --- Indentation engine for Ada mode, using the wisi generalized LALR parser  -*- lexical-binding:t -*-
;;
;; [1] ISO/IEC 8652:2012(E); Ada 2012 reference manual
;;
;; Copyright (C) 2012 - 2016  Free Software Foundation, Inc.
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
;;; History:
;;
;; implementation started Jan 2013
;;
;;;;

(require 'ada-fix-error)
(require 'ada-grammar-wy)
(require 'ada-indent-user-options)
(require 'cl-lib)
(require 'wisi)

(defconst ada-wisi-class-list
  '(
    block-end
    block-middle ;; not start of statement
    block-start ;; start of block is start of statement
    close-paren
    expression-start
    label
    list-break
    name
    name-paren ;; anything that looks like a procedure call, since the grammar can't distinguish most of them
    open-paren
    return
    return-with-params
    return-without-params
    statement-end
    statement-other
    statement-start
    ))

;;;; indentation

(defun ada-wisi-current-indentation ()
  "Return indentation of current line, taking into account leading parens and labels."
  (let ((in-paren (ada-in-paren-p))
	cache)
    (save-excursion
      (back-to-indentation)
      (cond
       ((and (eq ?\( (char-after (point)))
	     in-paren)
	(forward-same-syntax) ;; skip all leading parens
	(current-column))

       ((and (setq cache (wisi-get-cache (point)))
	     (eq 'label (wisi-cache-class cache)))
	(- (current-column) ada-indent-label))

       (t
	(current-column))

       ))))

(defun ada-wisi-indent-containing (offset cache &optional before indenting)
  "Return indentation of OFFSET plus indentation of token containing CACHE.
point should be at CACHE.
BEFORE should be t when called from ada-wisi-before-cache, nil otherwise.
If may be in parens, INDENTING must be point at indenting token."
  (let ((in-paren (and indenting
		       (save-excursion (goto-char indenting) (ada-in-paren-p)))))
    ;; First move to the right containing token
    ;;
    ;; test/ada_mode-nominal.adb
    ;; package body Ada_Mode.Nominal
    ;; ...
    ;; is
    ;;    use Ada.Strings;
    (while (and (setq cache (wisi-goto-containing cache nil))
		(memq (wisi-cache-token cache)
		      '(IS))))

    (cond
     (cache
      (cond
       (in-paren
	;; ada_mode-conditional_expressions.adb
	;; K1 : Integer := (if J > 42 then -1
	;;                  else +1);
	;;
	;; test/ada_mode-parens.adb
	;; or else ((B.all
	;;             and then C)
	;;            or else
	(goto-char indenting)
	(wisi-indent-paren (1+ offset)))

       (t
	;; not in paren
	;; test/ada_mode-nominal.adb
	;; function Local_Function return Integer
	;; is -- target 3
	(+ (ada-wisi-current-indentation) offset))
       ))

     (t
      ;; at outermost containing statement. If called from
      ;; ada-wisi-before-cache, we want to ignore OFFSET (indenting
      ;; 'package' in a package spec). If called from
      ;; ada-wisi-after-cache, we want to include offset (indenting
      ;; first declaration in the package).
      (if before
	  ;; test/ada_mode-nominal.adb
	  ;; package body Ada_Mode.Nominal
	  0
	;; test/ada_mode-nominal.adb
	;;      use Ada.Strings;
	offset)
      ))))

(defun ada-wisi-indent-list-break (cache prev-token)
  "Return indentation for a token contained or preceeded by CACHE, which must be a list-break.
Point must be on CACHE. PREV-TOKEN is the token before the one being indented."
  (let ((break-point (point))
	(containing (wisi-goto-containing cache)))
    (cl-ecase (wisi-cache-token containing)
      (LEFT_PAREN
       (if (equal break-point (cadr prev-token))
	   ;; we are indenting the first token after the list-break; not hanging.
	   ;;
	   ;; test/parent.adb
	   ;; Append_To (Formals,
	   ;;            Make_Parameter_Specification (Loc,
	   ;;
	   ;; test/ada_mode-generic_instantiation.ads
	   ;; function Function_1 is new Instance.Generic_Function
	   ;;   (Param_Type  => Integer,
	   ;;    Result_Type => Boolean,
	   ;;
	   ;; test/ada_mode-parens.adb
	   ;; Local_14 : Local_14_Type :=
	   ;;   ("123",
	   ;;    "456" &
	   (+ (current-column) 1)

	 ;; else hanging
	 ;;
	 ;; test/ada_mode-parens.adb
	 ;; A :=
	 ;;   (1 |
	 ;;      2 => (1, 1, 1),
	 ;;    3 |
	 ;;      4 => (2, 2, 2));
	 (+ (current-column) 1 ada-indent-broken)))

      (IS
       ;; test/ada_mode-conditional_expressions.adb
       ;; L1 : Integer := (case J is
       ;;                     when 42 => -1,
       ;;                     -- comment aligned with 'when'
       (wisi-indent-paren (+ 1 ada-indent-when)))

      (WITH
       (cl-ecase (wisi-cache-nonterm containing)
	 (aggregate
	  ;; test/ada_mode-nominal-child.ads
	  ;; (Default_Parent with
	  ;;  Child_Element_1 => 10,
	  ;;  Child_Element_2 => 12.0,
	  (wisi-indent-paren 1))

	 (aspect_specification_opt
	  ;; test/aspects.ads:
	  ;; type Vector is tagged private
	  ;; with
	  ;;   Constant_Indexing => Constant_Reference,
	  ;;   Variable_Indexing => Reference,
	  (+ (current-indentation) ada-indent-broken))
	 ))
      )
    ))

(defun ada-wisi-before-cache ()
  "Point is at indentation, before a cached token. Return new indentation for point."
  (let ((start (point))
	(cache (wisi-get-cache (point)))
	(prev-token (save-excursion (wisi-backward-token)))
	)
    (when cache
      (cl-ecase (wisi-cache-class cache)
	(block-start
	 (cl-case (wisi-cache-token cache)
	   (IS ;; subprogram body
	    (ada-wisi-indent-containing 0 cache t))

	   (RECORD
	    ;; test/ada_mode-nominal.ads; ada-indent-record-rel-type = 3
	    ;; type Private_Type_2 is abstract tagged limited
	    ;;    record
	    ;;
	    ;; type Limited_Derived_Type_1d is
	    ;;   abstract limited new Private_Type_1 with
	    ;;    record
	    ;;
	    ;; for Record_Type_1 use
	    ;;   record
	    (let ((containing (wisi-goto-containing cache)))
	      (while (not (memq (wisi-cache-token containing) '(FOR TYPE)))
		(setq containing (wisi-goto-containing containing)))
	      (+ (current-column) ada-indent-record-rel-type)))

	   (t ;; other
	    (ada-wisi-indent-containing ada-indent cache t))))

	(block-end
	 (cl-case (wisi-cache-nonterm cache)
	   (record_definition
	    (save-excursion
	      (wisi-goto-containing cache);; now on 'record'
	      (current-indentation)))

	   (t
	    (ada-wisi-indent-containing 0 cache t))
	   ))

	(block-middle
	 (ada-wisi-indent-containing
	  (if (eq 'WHEN (wisi-cache-token cache)) ada-indent-when 0)
	  cache t start))

	(close-paren (wisi-indent-paren 0))

	(expression-start
	 ;; defer to after
	 nil)

	(keyword
	 ;; defer to after-cache)
	 nil)

	(label
	 ;; test/ada_mode-nominal.adb
	 ;;    <<Label_1>>
	 ;;
	 ;; Block_1:
	 ;;    declare -- label, no statements between begin, label
	 ;; indenting Block_1
	 (ada-wisi-indent-containing (+ ada-indent-label ada-indent) cache t))

	(list-break
	 ;; test/ada_mode-parens.adb
	 ;; Slice_1 (1
	 ;;        ,    --  used to get an error here; don't care about the actual indentation
	 ;;
	 ;; We don't actually care what the indentation is, since this
	 ;; should only occur while editing; defer to after-cache
	 ;; avoids an error and does something reasonable.
	 nil)

	(name
	 (cond
	  ((let ((temp (save-excursion (wisi-goto-containing cache))))
	     (and temp
		  (memq (wisi-cache-nonterm temp) '(subprogram_body subprogram_declaration))))
	   ;; test/ada_mode-nominal.ads
	   ;; not
	   ;; overriding
	   ;; procedure
	   ;;   Procedure_1c (Item  : in out Parent_Type_1);
	   ;;
	   ;; not overriding function
	   ;;   Function_2e (Param : in Parent_Type_1) return Float;
	   (ada-wisi-indent-containing ada-indent-broken cache t))

	  (t
	   ;; defer to ada-wisi-after-cache, for consistency
	   nil)
	  ))

	(name-paren
	 ;; defer to ada-wisi-after-cache, for consistency
	 nil)

	(open-paren
	 ;; In some cases, we indent the leading paren one less than
	 ;; normal, so the following lines look normal. However, when
	 ;; ada-indent-broken = 1- ada-indent, the distinction is
	 ;; moot.

	 (let ((content (save-excursion (wisi-forward-cache))))

	   (cond
	    ((or
	      (memq (wisi-cache-nonterm cache)
		    '(formal_part)
		    ;; test/ada_mode-nominal.adb
		    ;; entry E2
		    ;;   (X : Integer)
		    )
	      (memq (wisi-cache-nonterm content)
		    '(case_expression
		      ;; test/ada_mode-conditional_expressions.adb
		      ;; K :=
		      ;;   (case Bounded (K) is

		      if_expression
		      ;; test/ada_mode-conditional_expressions.adb
		      ;; when 1  =>
		      ;;   (if J > 42
		      )))

	     (ada-wisi-indent-containing (1- ada-indent) cache t start))

	    (t
	     (ada-wisi-indent-containing ada-indent-broken cache t start))
	    )))

	(return-with-params
	 ;; test/ada_mode-options-intent_return_1.ads, _2, _3
	 ;; indenting 'return' after ()
	 (let ((return-pos (point)))
	   (wisi-goto-containing cache nil) ;; matching 'function'
	   (cond
	    ((<= ada-indent-return 0)
	     ;; indent relative to "("
	     (wisi-forward-find-class 'open-paren return-pos)
	     (+ (current-column) (- ada-indent-return)))

	    (t
	     (+ (current-column) ada-indent-return))
	    )))

	(return-without-params;; no parameter list
	 ;; test/ada_mode-options-intent_return_1.ads, _2, _3
	 ;; indenting 'return' with no ()
	 (wisi-goto-containing cache nil) ;; matching 'function'
	 (+ (current-column) ada-indent-broken))

	(statement-end
	 (ada-wisi-indent-containing ada-indent-broken cache t))

	(statement-other
	 (save-excursion
	   (let ((containing (wisi-goto-containing cache nil)))
	     ;; FIXME: document or delete
	     (while (not (wisi-cache-nonterm containing))
	       ;; containing is a token; need a nonterminal
	       (setq containing (wisi-goto-containing containing)))

	     (cl-case (wisi-cache-token cache)
	       (EQUAL_GREATER
		;; test/ada_mode-nominal.adb
		;; when
		;;   A | -- continuation line; ada-indent-broken = 2
		;;   B |
		;;   C
		;;   => -- Ada mode 4.01 indentation
		;;
		;; Local_A := (1 => 1.0,
		;;             2
		;;              => 2.0,
		(+ (current-column) ada-indent-broken))

	       (ELSIF
		(if (ada-in-paren-p)
		    ;; test/ada_mode-conditional_expressions.adb
		    ;; K := (if K < 0 then 42
		    ;;       elsif K = 0 then
		    (wisi-indent-paren 1)

		  ;; not in paren
		  ;; test/g-comlin.adb
		  ;;   elsif Current_Argument < CL.Argument_Count then
		  (+ (ada-wisi-current-indentation) 0)))

	       (RENAMES
		(cl-ecase (wisi-cache-nonterm containing)
		  ((generic_renaming_declaration subprogram_renaming_declaration)
		   ;; test/ada_mode-options-indent_return_1.ads
		   ;; function BR
		   ;;    return Integer   --  from ada-indent-broken
		   ;;    renames B;  --  from ada-indent-broken
		   ;;
		   ;; test/ada_mode-generic_instantiation.ads
		   ;; generic function Gen_Function_2
		   ;;   renames Instance.Generic_Function;
		   (wisi-forward-find-token '(FUNCTION PROCEDURE) start)
		   (let ((pos-subprogram (point))
			 (has-params
			  ;; this is wrong for one return access
			  ;; function case: overriding function Foo
			  ;; return access Bar (...) renames ...;
			  (wisi-forward-find-token 'LEFT_PAREN start t)))
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

		  (object_renaming_declaration
		   ;; test/bug_5746.adb
		   ;; C : Integer
		   ;;   renames A;
		   (+ (current-indentation) ada-indent-broken))
		  ))

	       (t
		(cl-ecase (wisi-cache-nonterm containing)

		  ;; abstract_subprogram_declaration with subprogram_body

		  (component_declaration
		   ;; test/ada_mode-nominal.ads
		   ;; type Record_Type_3
		   ;; ...
		   ;;      Component_3
		   ;;        : Integer;
		   (+ (current-column) ada-indent-broken))

		  (entry_body
		   ;; test/ada_mode-nominal.adb
		   ;; entry E2
		   ;;   (X : Integer)
		   ;;   when Local_1 = 0 and not
		   (+ (current-column) ada-indent-broken))

		  ;; expression_function_declaration with subprogram_body

		  (expression_opt
		   ;; test/ada_mode-nominal-child.adb
		   ;; return (Parent_Type_1
		   ;;         with 1, 0.0, False);
		   (cl-ecase (wisi-cache-token containing)
		     (LEFT_PAREN
		      ;; in an aggregate
		      (1+ (current-column)))
		     ;; FIXME: others?
		     ))

		  (formal_package_declaration
		   ;; test/ada_mode-generic_package.ads
		   ;; with package A_Package_7 is
		   ;;   new Ada.Text_IO.Integer_IO (Num => Formal_Signed_Integer_Type);
		   ;; indenting 'new'; containing is 'with'
		   (+ (current-column) ada-indent-broken))

		  ((full_type_declaration

		    ;; shared code, but out of alphabetical order:
		    protected_type_declaration
		    single_protected_declaration
		    single_task_declaration
		    subtype_declaration
		    task_type_declaration)

		   ;; move to start of declaration
		   (while (not (memq (wisi-cache-token containing) '(PROTECTED SUBTYPE TASK TYPE)))
		     (setq containing (wisi-goto-containing containing)))

		   (cond
		    ((eq (wisi-cache-token cache) 'WITH)
		     (let ((type-col (current-column))
			   (null_private (save-excursion (wisi-goto-end-1 cache)
							 (eq 'WITH (wisi-cache-token (wisi-backward-cache))))))
		       (cond
			((eq 'aspect_specification_opt (wisi-cache-nonterm cache))
			 ;; test/aspects.ads
			 ;; subtype Integer_String is String
			 ;; with Dynamic_Predicate => Integer'Value (Integer_String) in Integer
			 ;;
			 ;; test/ada_mode.ads
			 ;; protected Separate_Protected_Body
			 ;; with
			 ;;
			 ;; test/ada_mode-nominal.ads
			 ;; task type Task_Type_1 (Name : access String)
			 ;; with
			 type-col)

			(null_private
			 ;; 'with null record;' or 'with private;'
			 ;; test/ada_mode-nominal.ads
			 ;; type Limited_Derived_Type_3 is abstract limited new Private_Type_1
			 ;;   with null record;
			 (+ type-col ada-indent-broken))

			(t
			 ;; test/ada_mode-nominal.ads
			 ;; type Limited_Derived_Type_2a is abstract limited new Private_Type_1
			 ;;   with record
			 (+ type-col ada-indent-record-rel-type)))))

		    (t
		     ;; test/ada_mode-nominal.ads
		     ;; type Unconstrained_Array_Type_3 is array (Integer range <>, Standard.Character range <>)
		     ;;   of Object_Access_Type_1;
		     ;;
		     ;; type Object_Access_Type_7
		     ;;   is access all Integer;
		     ;;
		     ;; test/access_in_record.ads
		     ;; type A
		     ;;    is new Ada.Streams.Root_Stream_Type with record
		     ;;
		     ;; test/adacore_9717_001.ads
		     ;; subtype A_Long_Name
		     ;;   is Ada.Text_Io.Count;
		     (+ (current-column) ada-indent-broken))
		    ))

		  (generic_instantiation
		   ;; test/ada_mode-generic_instantiation.ads
		   ;; procedure Procedure_7 is
		   ;;   new Instance.Generic_Procedure (Integer, Function_1);
		   (+ (current-column) ada-indent-broken))

		  (generic_renaming_declaration
		   ;; test/ada_mode-generic_instantiation.ads
		   ;; generic
		   ;; procedure
		   (current-column))

		  ;; null_procedure_declaration with subprogram_body

		  (object_declaration
		   (cl-ecase (wisi-cache-token containing)
		     (COLON
		      ;; test/ada_mode-nominal.ads
		      ;; Anon_Array_3 : array (1 .. 10)
		      ;;   of Integer;
		      (+ (current-indentation) ada-indent-broken))

		     (COLON_EQUAL
		      ;; FIXME: document or delete
		      (+ (current-indentation) ada-indent-broken))

		     (identifier_list
		      ;; test/ada_mode-nominal.adb
		      ;; Local_2 : constant Float
		      ;;   := Local_1;
		      (+ (current-indentation) ada-indent-broken))
		     ))

		  ((package_declaration
		    package_body)
		   ;; test/ada_mode-nominal.ads
		   ;; package Ada_Mode.Nominal
		   ;; with
		   ;;
		   ;; test/ada_mode-nominal.adb
		   ;; package body Ada_Mode.Nominal
		   ;; with
		   (current-column))

		  (private_extension_declaration
		   (cl-case (wisi-cache-token cache)
		     (WITH
		      ;; test/aspects.ads
		      ;; type Date_Set is tagged private
		      ;; with
		      (current-indentation))

		     (t
		      ;; test/ada_mode-nominal.ads
		      ;; type Limited_Derived_Type_3 is abstract limited
		      ;;   new Private_Type_1 with private;
		      (+ (current-indentation) ada-indent-broken))
		     ))

		  (private_type_declaration
		   ;; test/aspects.ads
		   ;; type Vector is tagged private
		   ;; with
		   (current-indentation))

		  ;; protected_type_declaration with full_type_declaration

		  (qualified_expression
		   ;; FIXME: document or delete (now expression-start)
		   (+ (ada-wisi-current-indentation) ada-indent-broken))

		  ;; single_protected_declaration with full_type_declaration
		  ;; single_task_declaration with full_type_declaration

		  (statement
		   ;; test/ada_mode-nominal.adb
		   ;; select
		   ;;    delay 1.0;
		   ;; then
		   ;;    -- ...
		   ;;   abort
		   (+ (ada-wisi-current-indentation) ada-indent-broken))

		  ((subprogram_body
		    subprogram_declaration
		    subprogram_specification

		    ;; shared code, but out of alphabetical order:
		    abstract_subprogram_declaration
		    expression_function_declaration
		    null_procedure_declaration)
		   (cl-ecase (wisi-cache-token cache)
		     (IS
		      ;; test/ada_mode-nominal.ads
		      ;; procedure Procedure_1d
		      ;;   (Item   : in out Parent_Type_1;
		      ;;    Item_1 : in     Character;
		      ;;    Item_2 : out    Character)
		      ;;   is null;
		      (+ (current-column) ada-indent-broken))

		     (OVERRIDING
		      ;; test/ada_mode-nominal.ads
		      ;; not
		      ;; overriding
		      (current-column))

		     ((PROCEDURE FUNCTION)
		      ;; test/ada_mode-nominal.ads
		      ;; not overriding
		      ;; procedure Procedure_1b
		      (current-column))

		     (WITH
		      ;; test/aspects.ads
		      ;; not overriding procedure Foo (X : Integer;
		      ;;                               Y : out Integer)
		      ;; with Pre => X > 10 and
		      (ada-wisi-current-indentation))
		     ))

		  ;; subtype_declaration, task_type_declaration with full_type_declaration

		  )))
	      ))) ;; end statement-other

	(statement-start
	 (let ((containing (wisi-goto-containing cache)))
	   (if (not containing)
	       ;; test/ada_mod-library_function.adb
	       ;; function Ada_Mode.Library_Function return Integer is
	       ;; at bob
	       0
	     ;; not at bob
	     (cl-case (wisi-cache-class containing)
	       ((block-start block-middle)
		;; test/ada_mode-nested_packages.adb
		;; function Create (Model   : in Integer;
		;;                  Context : in String) return String is
		;;    ...
		;;    Cache : array (1 .. 10) of Boolean := (True, False, others => False);
		;;
		;; indenting 'Cache'; containing is block-middle 'is';
		;; move to block-start, assume that is at start of
		;; line. See below for 'WHEN' exception.
		(while (and (not (eq 'WHEN (wisi-cache-token containing)))
			    (eq 'block-middle (wisi-cache-class containing)))
		  (setq containing (wisi-goto-containing containing)))

		(cl-case (wisi-cache-nonterm containing)
		  (record_definition
		   ;; FIXME: document or delete
		   (+ (current-indentation) ada-indent))

		  ((exception-handler
		    ;; test/ada_mode-nominal.adb
		    ;; exception
		    ;;    ...
		    ;;    when others =>
		    ;;       return 0.0;
		    ;; assume 'when' is at beginning of line
		    select_alternative)
		    ;; test/ada_mode-nominal.adb
		    ;; or when Started
		    ;;      =>
		    ;;       accept Finish;
		    ;; indent relative to 'when'

		   (+ (current-column) ada-indent))

		  (t
		   ;; test/ada_mode-nominal.adb
		   ;; Loop_4 : while not (Local_1 > 0) loop
		   ;;       Local_1 := Local_1 + 2;

		   ;; test/ada_mode-opentoken.ads
		   ;; private package GDS.Commands.Add_Statement is
		   ;;    type Instance is new Nonterminal.Instance with null record;
		   (+ (ada-wisi-current-indentation) ada-indent))
		  ))

	       (list-break
		;; FIXME: document or delete
		(ada-wisi-indent-list-break cache prev-token))

	       ))))
	))
    ))

;; FIXME: these are now handled by after-cache


		 ;; test/aspects.ads
		 ;;    function Wuff return Boolean with Pre =>
		 ;;      (for all x in U =>
		 ;; indenting '(for';  containing is '=>', 'with', 'function'

	      ;; test/ada_mode-nominal.adb
	      ;; function Function_Access_11
	      ;;   (A_Param : in Float)
	      ;;   --  EMACSCMD:(test-face "function" font-lock-keyword-face)
	      ;;   return access function
	      ;;     (A_Param : in Float)
	      ;;     return
	      ;;     Standard.Float -- Ada mode 4.01, GPS do this differently
	      ;; indenting second '(A_Param)

	      ;; test/ada_mode-parens.adb
	      ;; or else ((B.all
	      ;;             and then C)
	      ;;            or else
	      ;;            (D
	      ;; indenting (D

	      ;; test/ada_mode-nominal.adb
	      ;;
	      ;; when Local_1 = 0 and not
	      ;;   (Local_2 = 1)
	      ;; indenting (Local_2
	      ;;
	      ;; entry E3
	      ;;   (X : Integer) when Local_1 = 0 and not
	      ;;     (Local_2 = 1)

	      ;; test/indent.ads
	      ;; CSCL_Type'
	      ;;   (
	      ;; identing (
	      ;;
	      ;; test/ada_mode-parens.adb
	      ;; Check
	      ;;   ("foo bar",
	      ;;    A
	      ;;      (1),
	      ;;    A(2));
	      ;; indenting (1)
	      ;;
	      ;; test/ada_mode-parens.adb
	      ;; Local_11 : Local_11_Type := Local_11_Type'
	      ;;   (A => Integer
	      ;;      (1.0),
	      ;;    B => Integer
	      ;;      (2.0));
	      ;;
	      ;; test/ada_mode-parens.adb
	      ;; Local_12 : Local_11_Type
	      ;;   := Local_11_Type'(A => Integer
	      ;;     (1.0),
	      ;;
	      ;; test/ada_mode-generic_instantiation.ads
	      ;; function Function_1 is new Instance.Generic_Function
	      ;;   (Param_Type  => Integer,
	      ;;

		 ;; test/ada_mode-nominal.adb
		 ;; entry E2
		 ;;   (X : Integer)
		 ;; indenting (X

		 ;; Open paren in an expression.
		 ;;
		 ;; test/ada_mode-conditional_expressions.adb
		 ;; L0 : Integer :=
		 ;;   (case J is when 42 => -1, when Integer'First .. 41 => 0, when others => 1);
		 ;; indenting (case

		;; test/ada_mode-parens.adb
		;; Local_13 : Local_11_Type
		;;   := (Integer'(1),
		;;       Integer'(2));

;; test/indent.ads
;; C_S_Controls : constant
;;   CSCL_Type :=
;;     CSCL_Type'

		   ;; test/ada_mode-nominal-child.ads
		   ;; Child_Obj_5 : constant Child_Type_1 :=
		   ;;   (Parent_Type_1'
(defun ada-wisi-after-cache ()
  "Point is at indentation, not before a cached token. Find previous
cached token, return new indentation for point."
  (save-excursion
    (let ((start (point))
	  (prev-token (save-excursion (wisi-backward-token)))
	  (cache (wisi-backward-cache)))

      (cond
       ((not cache) ;; bob
	0)

       (t
	(while (memq (wisi-cache-class cache) '(keyword name name-paren type))
	  ;; not useful for indenting
	  (setq cache (wisi-backward-cache)))


	(cl-ecase (wisi-cache-class cache)
	  (block-end
	   ;; indenting block/subprogram name after 'end'
	   (wisi-indent-current ada-indent-broken))

	  (block-middle
	   (cl-case (wisi-cache-token cache)
	     (IS
	      (cl-case (wisi-cache-nonterm cache)
		(case_statement
		 ;; between 'case .. is' and first 'when'; most likely a comment
		 (ada-wisi-indent-containing 0 cache t))

		(t
		 (+ (ada-wisi-indent-containing ada-indent cache t)))
		))

	     ((THEN ELSE)
	      (let ((indent
		     (cl-ecase (wisi-cache-nonterm (wisi-get-containing-cache cache))
		       ((statement if_statement elsif_statement_item)
			;; FIXME: document or delete
			ada-indent)

		       ((if_expression elsif_expression_item)
			;; test/ada_mode-conditional_expressions.adb
			;; K3 : Integer := (if
			;;                    J > 42
			;;                  then
			;;                    -1
			;;                  else
			;;                    +1);
			;; indenting -1, +1
			;;
			;; K := (if K < 0 then 42
			;;       elsif K = 0 then
			;;         (case J is
			ada-indent-broken))))
		(ada-wisi-indent-containing indent cache nil start)))

	     (WHEN
	      (cl-ecase (wisi-cache-nonterm cache)
		(case_expression_alternative
		 ;; test/ada_mode-conditional_expressions.adb
		 ;; L2 : Integer := (case J is
		 ;;                     when
		 ;;                       42 => -1,
		 (+ (current-column) ada-indent-broken))

		((case_statement_alternative
		  ;; test/ada_mode-nominal.adb
		  ;; when C =>
		  ;;    --EMACSCMD:(progn (forward-line 2)(forward-word 1)(forward-char 1)(insert "   ")(ada-align))

		  exception_handler)
		 ;; test/ada_mode-nominal.adb
		 ;; when E : Constraint_Error =>
		 (+ (current-column) ada-indent))

		 ;; (case_statement_alternative
		 ;;  ;; FIXME: document
		 ;;  ;; comment between 'when' and '=>'
		 ;;  (+ (current-column) ada-indent-broken))
		))

	     (t
	      ;; block-middle keyword may not be on separate line:
	      ;;       function Create (Model   : in Integer;
	      ;;                        Context : in String) return String is
	      (ada-wisi-indent-containing ada-indent cache nil))
	     ))

	  (block-start
	   (cl-case (wisi-cache-nonterm cache)
	     (exception_handler
	      ;; between 'when' and '=>'
	      (+ (current-column) ada-indent-broken))

	     (if_expression
	      (ada-wisi-indent-containing ada-indent-broken cache nil))

	     (select_alternative
	      (ada-wisi-indent-containing (+ ada-indent-when ada-indent-broken) cache nil))

	     (t ;; other; normal block statement
	      (+ (ada-wisi-current-indentation) ada-indent))
	     ))

	  (close-paren
	   ;; actual_parameter_part: test/ada_mode-nominal.adb
	   ;; return 1.0 +
	   ;;   Foo (Bar) + -- multi-line expression that happens to have a cache at a line start
	   ;;   12;
	   ;; indenting '12'; don't indent relative to containing function name
	   ;;
	   ;; attribute_designator: test/ada_mode-nominal.adb
	   ;; raise Constraint_Error with Count'Image (Line (File)) &
	   ;;    "foo";
	   ;; indenting '"foo"'; relative to raise
	   ;;
	   ;; test/ada_mode-slices.adb
	   ;; Put_Line(Day'Image(D1) & " - " & Day'Image(D2) & " = " &
	   ;;            Integer'Image(N));
	   ;; indenting 'Integer'
	   (when (memq (wisi-cache-nonterm cache)
		       '(actual_parameter_part attribute_designator))
	     (setq cache (wisi-goto-containing cache)))

	   (cond
	    ((eq (wisi-cache-class cache)
		 'expression-start)
	     ;; test/ada_mode-conditional_expression.adb
	     ;; when B =>
	     ;;    Fun (E) = 0
	     ;;      or else M,
	     ;; indenting 'or else'
	     (+ (current-indentation) ada-indent-broken))

	    (t
	     (ada-wisi-indent-containing ada-indent-broken cache nil start))
	    ))

	  (expression-start
	   (cond
	    ((eq 'LEFT_PAREN (wisi-cache-token cache))
	     (if (= (point) (cadr prev-token))
		 ;; test/ada_mode-parens.adb
		 ;; Local_9 : String := (
		 ;;                      "123" &
		 (1+ (current-column))

	       ;; not at token preceding indenting token
	       ;; test/ada_mode-nominal.adb
	       ;; 2
	       ;;   => (others
	       ;;         => 2.0),
	       ;;
	       ;; test/ada_mode-parens.adb
	       ;; A :=
	       ;;   (1 |
	       ;;      2 => (0, 0, 0),
	       (+ 1 (current-column) ada-indent-broken)))

	    (t
	     (let ((containing (wisi-get-containing-cache cache)))
	       (cl-ecase (wisi-cache-class containing)
		 (expression-start
		  ;; test/ada_mode-parens.adb
		  ;; Local_2 : Integer := (1 + 2 +
		  ;;                         3);
		  (wisi-indent-paren (1+ ada-indent-broken)))

		 (open-paren
		  ;; test/ada_mode-parens.adb
		  ;; if A.all
		  ;;   or else (B.all
		  ;;              --EMACSCMD:(test-face "then" 'font-lock-keyword-face)
		  ;;              and then C
		  (wisi-indent-paren (1+ ada-indent-broken)))

		 (t
		  ;; test/ada_mode-conditional_expressions.adb
		  ;; L5 : Boolean :=
		  ;;   (case C is
		  ;;       when A =>
		  ;;          J = 4
		  ;;            or else M, --  test case from Piotr Trojanek
		  (+ (ada-wisi-current-indentation) ada-indent-broken))
		 )))
	     ))

	  (label
	   ;; comment after label
	   (+ (current-column) (- ada-indent-label)))

	  (list-break
	   (ada-wisi-indent-list-break cache prev-token))

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
	   ;;    aggregate, and there is whitespace between
	   ;;    ( and the first token:
	   ;;
	   ;; test/ada_mode-parens.adb
	   ;; Local_9 : String := (
	   ;;                      "123"
	   ;;
	   ;; 3) A parenthesized expression, or the first item in an
	   ;;    aggregate, and there is a comment between
	   ;;    ( and the first token:
	   ;;
	   ;; test/ada_mode-nominal.adb
	   ;; A :=
	   ;;   (
	   ;;    -- a comment between paren and first association
	   ;;    1 =>
	   ;;
	   ;; test/ada_mode-parens.adb
	   ;; return Float (
	   ;;               Integer'Value
	   ;; indenting 'Integer'
	   (let ((paren-column (current-column))
		 (start-is-comment (save-excursion (goto-char start) (looking-at comment-start-skip))))
	     (wisi-forward-token); point is now after paren
	     (if start-is-comment
		 (skip-syntax-forward " >"); point is now on comment
	       (forward-comment (point-max)); point is now on first token
	       )
	     (if (= (point) start)
		 ;; case 2) or 3)
		 (1+ paren-column)
	       ;; 1)
	       (+ paren-column 1 ada-indent-broken))))

	  ((return-with-params return-without-params)
	   ;; test/ada_mode-nominal.adb
	   ;; function Function_Access_1
	   ;;   (A_Param : in Float)
	   ;;   return
	   ;;     Standard.Float
	   ;; indenting 'Standard.Float'
	   ;;
	   ;; test/ada_mode-expression_functions.ads
	   ;; function Square (A : in Float) return Float
	   ;;   is (A * A);
	   ;; indenting 'is'
	   ;;
	   ;; test/ada_mode-nominal.ads
	   ;; function Function_2g
	   ;;   (Param : in Private_Type_1)
	   ;;   return Float
	   ;;   is abstract;
	   ;; indenting 'is'
	   (back-to-indentation)
	   (+ (current-column) ada-indent-broken))

	  (statement-end
	   (ada-wisi-indent-containing 0 cache nil))

	  (statement-other
	   (cl-ecase (wisi-cache-token cache)
	     (ABORT
	      ;; select
	      ;;    Please_Abort;
	      ;; then
	      ;;   abort
	      ;;    -- 'abort' indented with ada-indent-broken, since this is part
	      ;;    Titi;
	      (ada-wisi-indent-containing ada-indent cache))

	     ;; test/subdir/ada_mode-separate_task_body.adb
	     ((COLON COLON_EQUAL)
	      ;; Local_3 : constant Float :=
	      ;;   Local_2;
	      ;;
	      ;; test/ada_mode-nominal.ads
	      ;; type Record_Type_3 (Discriminant_1 : access Integer) is tagged record
	      ;;    Component_1 : Integer; -- end 2
	      ;;    Component_2 :
	      ;;      Integer;
	      ;; indenting 'Integer'; containing is ';'
	      (+ (ada-wisi-current-indentation) ada-indent-broken))

	     (COMMA
	      (cl-ecase (wisi-cache-nonterm cache)
		(name_list
		 (cl-ecase (wisi-cache-nonterm (wisi-get-containing-cache cache))
		   (use_clause
		    ;; test/with_use1.adb
		    (ada-wisi-indent-containing ada-indent-use cache))

		   (with_clause
		    ;; test/ada_mode-nominal.ads
		    ;; limited private with Ada.Strings.Bounded,
		    ;;   --EMACSCMD:(test-face "Ada.Containers" 'default)
		    ;;   Ada.Containers;
		    ;;
		    ;; test/with_use1.adb
		    (ada-wisi-indent-containing ada-indent-with cache))
		   ))
		))

	     (ELSIF
	      ;; test/g-comlin.adb
	      ;; elsif Index_Switches + Max_Length <= Switches'Last
	      ;;   and then Switches (Index_Switches + Max_Length) = '?'
	      (+ (ada-wisi-current-indentation) ada-indent-broken))

	     (EQUAL_GREATER
	      (let ((cache-col (current-column))
		    (cache-pos (point))
		    (line-end-pos (line-end-position))
		    (containing (wisi-goto-containing cache nil)))

		;; FIXME: document or delete
		(while (eq (wisi-cache-nonterm containing) 'association_list)
		  (setq containing (wisi-goto-containing containing nil)))

		(cl-ecase (wisi-cache-nonterm containing)
		  ((actual_parameter_part aggregate)
		   ;; test/ada_mode-generic_package.ads
		   ;; with package A_Package_2 is new Ada.Text_IO.Integer_IO (Num =>
		   ;;                                                           Formal_Signed_Integer_Type);
		   ;;  indenting 'Formal_Signed_...', point on '(Num'
		   ;;
		   ;; test/ada_mode-parens.adb
		   ;; (1      =>
		   ;;    1,
		   ;;  2      =>
		   ;;    1 + 2 * 3,
		   ;; indenting '1,' or '1 +'; point on '(1'
		   ;;
		   ;; test/ada_mode-parens.adb
		   ;; Local_13 : Local_11_Type
		   ;;   := (Integer'(1),
		   ;;       Integer'(2));
		   ;; indenting 'Integer'; point on '(Integer'
		   (+ (current-column) 1 ada-indent-broken))

		  (aspect_specification_opt
		   ;; test/aspects.ads
		   ;; with Pre => X > 10 and
		   ;;             X < 50 and
		   ;;             F (X),
		   ;;   Post =>
		   ;;     Y >= X and
		   ;; indenting 'X < 50' or 'Y >= X'; cache is '=>', point is on '=>'
		   ;; or indenting 'Post =>'; cache is ',', point is on 'with'
		   (cl-ecase (wisi-cache-token cache)
		     (COMMA
		      (+ (current-indentation) ada-indent-broken))

		     (EQUAL_GREATER
		      (if (= (+ 2 cache-pos) line-end-pos)
			  ;;   Post =>
			  ;;     Y >= X and
			  (progn
			    (goto-char cache-pos)
			    (+ (current-indentation) ada-indent-broken))
			;; with Pre => X > 10 and
			;;             X < 50 and
			(+ 3 cache-col)))
		     ))

		  (association_list
		   (cl-ecase (save-excursion (wisi-cache-token (wisi-goto-containing cache nil)))
		     (COMMA
		      (ada-wisi-indent-containing (* 2 ada-indent-broken) cache))
		     ))

		  (case_expression_alternative
		   ;; L3 : Integer := (case J is
		   ;;                     when 42 =>
		   ;;                        -1,
		   ;;                     when Integer'First .. 41 =>
		   ;;                        0,
		   ;; indenting -1, 0
		   (+ (current-column) ada-indent-broken))

		  ((case_statement_alternative exception_handler)
		   ;; FIXME: document
		   (+ (current-column) ada-indent))

		  (expression_opt
		   (cl-ecase (wisi-cache-token containing)
		     (LEFT_PAREN
		      ;; in an aggregate
		      ;; test/ada_mode-interactive_common.adb
		      ;; E := (1 =>                   --
		      ;;         'A');
		      (+ 1 (current-column) ada-indent-broken))
		     ;; FIXME: others?
		     ))

		  (generic_renaming_declaration
		   ;; not indenting keyword following 'generic'
		   (+ (current-column) ada-indent-broken))

                  (paren_expression
		   ;; test/ada_mode-expression_functions.ads
		   ;; (for some X of Y =>
		   ;;    Pred (X));
		   ;; indenting "Pred"
                   (+ (current-column) ada-indent))

		  (primary
		   ;; test/ada_mode-quantified_expressions.adb
		   ;; if (for some J in 1 .. 10 =>
		   ;;       J/2 = 0)
		   (ada-wisi-indent-containing ada-indent-broken cache))


		  (select_alternative
		   ;; test/ada_mode-nominal.adb
		   ;; or when Started
		   ;;      =>
		   ;;       accept Finish;
		   ;; indenting 'accept'; point is on 'when'
		   (+ (current-column) ada-indent))

		  (variant
		   ;; test/generic_param.adb
		   ;; case Item_Type is
		   ;;    when Fix | Airport =>
		   ;;       null;
		   ;; indenting 'null'
		   (+ (current-column) ada-indent))

		  )))

	     (IS
	      (setq cache (wisi-goto-containing cache))
	      (cl-ecase (wisi-cache-nonterm cache)
		(expression_function_declaration
		 ;; test/ada_mode-expression_functions.ads
		 ;; function Fun1 (Really_Really_Long_Argument_List : Boolean)
		 ;;               return Boolean -- a Really_Really_Long_Return_Type
		 ;;   is
		 ;;   (True) -- a Really_Really_Long_expression
		 (+ (ada-wisi-current-indentation) ada-indent-broken))

		(full_type_declaration
		 ;; ada_mode/nominal.ads
		 ;; type Limited_Derived_Type_1a is abstract limited new
		 ;;    Private_Type_1 with record
		 ;;       Component_1 : Integer;
		 ;; indenting 'Private_Type_1'; look for 'record'
		 (let ((type-column (current-column)))
		   (goto-char start)
		   (if (wisi-forward-find-token 'RECORD (line-end-position) t)
		       ;; 'record' on line being indented
		       (+ type-column ada-indent-record-rel-type)
		     ;; 'record' on later line
		     (+ type-column ada-indent-broken))))

		((formal_type_declaration
		  ;; test/ada_mode-generic_package.ads
		  ;; type Synchronized_Formal_Derived_Type is abstract synchronized new Formal_Private_Type and Interface_Type
		  ;;   with private;

		  subtype_declaration
		  ;; test/ada_mode-nominal.ads
		  ;;    subtype Subtype_2 is Signed_Integer_Type range 10 ..
		  ;;      20;

		  private_type_declaration
		  ;; type Private_Type_2 is abstract tagged limited
		  ;;  private;
		  )
		 (+ (current-column) ada-indent-broken))

		(null_procedure_declaration
		 ;; ada_mode-nominal.ads
		 ;; procedure Procedure_3b is
		 ;;   null;
		 ;; indenting null
		 (+ (current-column) ada-indent-broken))

		))

	     (LEFT_PAREN
	      ;; test/indent.ads
	      ;; C_S_Controls : constant
	      ;;   CSCL_Type :=
	      ;;     CSCL_Type'
	      ;;       (
	      ;;        1 =>
	      (+ (current-column) 1))

	     (NEW
	      ;; ada_mode-nominal.ads
	      ;; type Limited_Derived_Type_2 is abstract limited new Private_Type_1 with
	      ;;   private;
	      ;;
	      ;; test/ada_mode-generic_instantiation.ads
	      ;;   procedure Procedure_6 is new
	      ;;     Instance.Generic_Procedure (Integer, Function_1);
	      ;; indenting 'Instance'; containing is 'new'
	      (ada-wisi-indent-containing ada-indent-broken cache))

	     (OF
	      ;; ada_mode-nominal.ads
	      ;; Anon_Array_2 : array (1 .. 10) of
	      ;;   Integer;
	      (ada-wisi-indent-containing ada-indent-broken cache))

	     (USE
	      ;; test/foruse.adb
	      ;; for Command_Labels_Type use
	      ;;   (Noop              => 0,
	      (+ (ada-wisi-current-indentation) ada-indent-broken))

	     (WHEN
	      ;; test/ada_mode-parens.adb
	      ;; exit when A.all
	      ;;   or else B.all
	      (ada-wisi-indent-containing ada-indent-broken cache))

	     (WITH
	      (cl-ecase (wisi-cache-nonterm cache)
		(aggregate
		 ;; test/ada_mode-nominal-child.ads
		 ;;   (Default_Parent with
		 ;;    10, 12.0, True);
		 ;; indenting '10'; containing is '('
		 (wisi-indent-paren 1))

		(aspect_specification_opt
		 ;; test/aspects.ads
		 ;; type Vector is tagged private
		 ;; with
		 ;;   Constant_Indexing => Constant_Reference,
		 ;; indenting 'Constant_Indexing'; point is on 'with'
		 (+ (current-indentation) ada-indent-broken))

		(derived_type_definition
		 ;; test/ada_mode-nominal-child.ads
		 ;; type Child_Type_1 is new Parent_Type_1 with
		 ;;   -- comment between 'with' and 'record'
		 ;;    record
		 ;; indenting comment
		 (+ (current-indentation) ada-indent-broken))
		))

	     ;; otherwise just hanging
	     ((ACCEPT IDENTIFIER FUNCTION PROCEDURE RENAMES)
	      ;; test/ada_mode-long_paren.adb
	      ;; Packet := new Packet_Type'
	      ;;   (RT                            => RT,
	      (+ (ada-wisi-current-indentation) ada-indent-broken))
	     ))

	  (statement-start
	   (cl-case (wisi-cache-token cache)
	     (WITH ;; with_clause
	      (+ (current-column) ada-indent-with))

	     (t
	      (if (ada-in-paren-p)
		  ;; test/ada_mode-conditional_expressions.adb
		  ;; K3 : Integer := (if
		  ;;                    J > 42
		  (wisi-indent-paren (1+ ada-indent-broken))

		;; not in paren
		;; test/ada_mode-generic_instantiation.ads
		;; procedure Procedure_8
		;;   is new Instance.Generic_Procedure (Integer, Function_1);
		(+ (ada-wisi-current-indentation) ada-indent-broken)))
	     ))
	  )))
      )))

(defun ada-wisi-comment ()
  "Compute indentation of a comment. For `wisi-indent-calculate-functions'."
  ;; We know we are at the first token on a line. We check for comment
  ;; syntax, not comment-start, to accomodate gnatprep, skeleton
  ;; placeholders, etc.
  (when (and (not (= (point) (point-max))) ;; no char after EOB!
	     (= 11 (syntax-class (syntax-after (point)))))

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

      (let ((indent (ada-wisi-after-cache))
	    prev-indent next-indent)
	(if ada-indent-comment-gnat
	  ;; match the gnat comment indent style check; comments must
	  ;; be aligned to one of:
	  ;;
	  ;; - multiple of ada-indent
	  ;; - next non-blank line
	  ;; - previous non-blank line
	  ;;
	  ;; Note that we must indent the prev and next lines, in case
	  ;; they are not currently correct.
	  (cond
	   ((= 0 (% indent ada-indent))
	    ;; this will handle comments at bob and eob, so we don't
	    ;; need to worry about those positions in the next checks.
	    indent)

	   ((and (setq prev-indent
		       (save-excursion (forward-line -1)(indent-according-to-mode)(current-indentation)))
		 (= indent prev-indent))
	    indent)

	   ((and (setq next-indent
		       ;; we use forward-comment here, instead of
		       ;; forward-line, because consecutive comment
		       ;; lines are indented to the current one, which
		       ;; we don't know yet.
		       (save-excursion (forward-comment (point-max))(indent-according-to-mode)(current-indentation)))
		 (= indent next-indent))
	    indent)

	   (t
	    ;; prev-indent and next-indent are both set here;
	    ;; could add more checks to decide which one to use.
	     prev-indent)
	   )

	  ;; not forcing gnat style
	  indent)))

      (t
       ;; comment is after a comment
       (forward-comment -1)
       (current-column))
      )))

(defun ada-wisi-post-parse-fail ()
  "For `wisi-post-parse-fail-hook'."
  (save-excursion
    (let ((start-cache (wisi-goto-start (or (wisi-get-cache (point)) (wisi-backward-cache)))))
      (when start-cache
	;; nil when in a comment at point-min
	(indent-region (point) (wisi-cache-end start-cache)))
      ))
  (back-to-indentation))

;;;; ada-mode functions (alphabetical)

(defun ada-wisi-declarative-region-start-p (cache)
  "Return t if cache is a keyword starting a declarative region."
  (cl-case (wisi-cache-token cache)
   (DECLARE t)
   (IS
    (memq (wisi-cache-class cache) '(block-start block-middle)))
   (t nil)
   ))

(defun ada-wisi-context-clause ()
  "For `ada-fix-context-clause'."
  (wisi-validate-cache (point-max) t)
  (save-excursion
    (goto-char (point-min))
    (let ((begin nil)
	  (end nil)
	  cache)

      (while (not end)
	(setq cache (wisi-forward-cache))
	(cl-case (wisi-cache-nonterm cache)
	  (pragma (wisi-goto-end-1 cache))
	  (use_clause (wisi-goto-end-1 cache))
	  (with_clause
	   (when (not begin)
	     (setq begin (point-at-bol)))
	   (wisi-goto-end-1 cache))
	  (t
	   ;; start of compilation unit
	   (setq end (point-at-bol))
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
    ;; Used by ada-align, which does indent, which will require parse
    ;; We know we are in a paren.
    (ada-goto-open-paren 1)
    (let ((cache (wisi-get-cache (point))))
      (and cache
	   (eq (wisi-cache-nonterm cache) 'case_expression)))
    ))

(defun ada-wisi-goto-subunit-name ()
  "For `ada-goto-subunit-name'."
  (wisi-validate-cache (point-max) t)

  (let ((end nil)
	cache
	(name-pos nil))
    (save-excursion
      ;; move to top declaration
      (goto-char (point-min))
      (setq cache (or (wisi-get-cache (point))
		      (wisi-forward-cache)))
      (while (not end)
	(cl-case (wisi-cache-nonterm cache)
	  ((pragma use_clause with_clause)
	   (wisi-goto-end-1 cache)
	   (setq cache (wisi-forward-cache)))
	  (t
	   ;; start of compilation unit
	   (setq end t))
	  ))
      (when (eq (wisi-cache-nonterm cache) 'subunit)
	(wisi-forward-find-class 'name (point-max)) ;; parent name
	(wisi-forward-token)
	(wisi-forward-find-class 'name (point-max)) ;; subunit name
	(setq name-pos (point)))
      )
    (when name-pos
      (goto-char name-pos))
    ))

(defun ada-wisi-goto-declaration-start ()
  "For `ada-goto-declaration-start', which see.
Also return cache at start."
  (wisi-validate-cache (point) t)

  (let ((cache (wisi-get-cache (point)))
	(done nil))
    (unless cache
      (setq cache (wisi-backward-cache)))
    ;; cache is null at bob
    (while (not done)
      (if cache
	  (progn
	    (setq done
		  (cl-case (wisi-cache-nonterm cache)
		    ((generic_package_declaration generic_subprogram_declaration)
		     (eq (wisi-cache-token cache) 'GENERIC))

		    ((package_body package_declaration)
		     (eq (wisi-cache-token cache) 'PACKAGE))

		    ((protected_body protected_type_declaration single_protected_declaration)
		     (eq (wisi-cache-token cache) 'PROTECTED))

		    ((abstract_subprogram_declaration
		      subprogram_body
		      subprogram_declaration
		      null_procedure_declaration)
		     (memq (wisi-cache-token cache) '(NOT OVERRIDING FUNCTION PROCEDURE)))

		    (task_type_declaration
		     (eq (wisi-cache-token cache) 'TASK))

		    ))
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
  (wisi-validate-cache (point) t)

  (let ((done nil)
	(first t)
	(cache
	 (or
	  (wisi-get-cache (point))
	  ;; we use forward-cache here, to handle the case where point is after a subprogram declaration:
	  ;; declare
	  ;;     ...
	  ;;     function ... is ... end;
	  ;;     <point>
	  ;;     function ... is ... end;
	  (wisi-forward-cache))))
    (while (not done)
      (if (ada-wisi-declarative-region-start-p cache)
	  (progn
	    (wisi-forward-token)
	    (setq done t))
	(cl-case (wisi-cache-class cache)
	  ((block-middle block-end)
	   (setq cache (wisi-prev-statement-cache cache)))

	  (statement-start
	   ;; 1) test/ada_mode-nominal.adb
	   ;;    protected body Protected_1 is -- target 2
	   ;;        <point>
	   ;;    want target 2
	   ;;
	   ;; 2) test/ada_mode-nominal.adb
	   ;;    function Function_Access_1
	   ;;      (A_Param <point> : in Float)
	   ;;      return
	   ;;        Standard.Float
	   ;;    is -- target 1
	   ;;    want target 1
	   ;;
	   ;; 3) test/ada_mode-nominal-child.adb
	   ;;    overriding <point> function Function_2c (Param : in Child_Type_1)
	   ;;                                    return Float
	   ;;    is -- target Function_2c
	   ;;    want target

	   (if first
	       ;; case 1
	       (setq cache (wisi-goto-containing cache t))
	     ;; case 2, 3
	     (cl-case (wisi-cache-nonterm cache)
	       (subprogram_body
		(while (not (eq 'IS (wisi-cache-token cache)))
		  (setq cache (wisi-next-statement-cache cache))))
	       (t
		(setq cache (wisi-goto-containing cache t)))
	       )))
	  (t
	   (setq cache (wisi-goto-containing cache t)))
	  ))
      (when first (setq first nil)))
    ))

(defun ada-wisi-in-paramlist-p (&optional parse-result)
  "For `ada-in-paramlist-p'."
  (wisi-validate-cache (point))
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
  (wisi-validate-cache (point) t)

  (let* ((begin (point))
	 (end (save-excursion (wisi-forward-find-class 'statement-end (point-max)) (point)))
	 (cache (wisi-forward-find-class 'name end))
	 (name (buffer-substring-no-properties
		(point)
		(+ (point) (wisi-cache-last cache)))))
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

(defun ada-wisi-scan-paramlist (begin end)
  "For `ada-scan-paramlist'."
  (wisi-validate-cache end t)

  (goto-char begin)
  (let (token
	text
	identifiers
	(aliased-p nil)
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
	(setq text  (wisi-token-text token-text)))
      (cond
       ((equal token 'COMMA) nil);; multiple identifiers

       ((equal token 'COLON)
	;; identifiers done. find type-begin; there may be no mode
	(skip-syntax-forward " ")
	(setq type-begin (point))
	(save-excursion
	  (while (member (car (wisi-forward-token)) '(ALIASED IN OUT NOT NULL ACCESS CONSTANT PROTECTED))
	    (skip-syntax-forward " ")
	    (setq type-begin (point)))))

       ((equal token 'ALIASED) (setq aliased-p t))
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

       ((equal token 'LEFT_PAREN)
	;; anonymous access procedure type
	(goto-char (scan-sexps (1- (point)) 1)))

       ((member token '(SEMICOLON RIGHT_PAREN))
	(when (not type-end)
	  (setq type-end (save-excursion (backward-char 1) (skip-syntax-backward " ") (point))))

	(setq type (buffer-substring-no-properties type-begin type-end))

	(when default-begin
	  (setq default (buffer-substring-no-properties default-begin (1- (point)))))

	(when (equal token 'RIGHT_PAREN)
	  (setq done t))

	(setq param (list (reverse identifiers)
			  aliased-p in-p out-p not-null-p access-p constant-p protected-p
			  type default))
        (cl-pushnew param paramlist :test #'equal)
	(setq identifiers nil
	      aliased-p nil
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
          (cl-pushnew text identifiers :test #'equal)))
       ))
    paramlist))

(defun ada-wisi-which-function-1 (keyword add-body)
  "Used in `ada-wisi-which-function'."
  (let* ((cache (wisi-forward-find-class 'name (point-max)))
         (result (wisi-cache-text cache)))

    ;; See comment at ada-mode.el on why we don't overwrite ff-function-name.
    (when (not ff-function-name)
      (setq ff-function-name
	    (concat
	     keyword
	     (when add-body "\\s-+body")
	     "\\s-+"
	     result
	     ada-symbol-end)))
    result))

(defun ada-wisi-which-function ()
  "For `ada-which-function'."
  (wisi-validate-cache (point))
  ;; no message on parse fail, since this could be called from which-func-mode
  (when (> wisi-cache-max (point))
    (save-excursion
      (let ((result nil)
	    (cache (condition-case nil (ada-wisi-goto-declaration-start) (error nil))))
	(if (null cache)
	    ;; bob or failed parse
	    (setq result "")

	  (when (memq (wisi-cache-nonterm cache)
		      '(generic_package_declaration generic_subprogram_declaration))
	    ;; name is after next statement keyword
	    (wisi-next-statement-cache cache)
	    (setq cache (wisi-get-cache (point))))

	  ;; add or delete 'body' as needed
	  (cl-ecase (wisi-cache-nonterm cache)
	    (package_body
	     (setq result (ada-wisi-which-function-1 "package" nil)))

	    ((package_declaration
	      generic_package_declaration) ;; after 'generic'
	     (setq result (ada-wisi-which-function-1 "package" t)))

	    (protected_body
	     (setq result (ada-wisi-which-function-1 "protected" nil)))

	    ((protected_type_declaration single_protected_declaration)
	     (setq result (ada-wisi-which-function-1 "protected" t)))

	    ((abstract_subprogram_declaration
	      subprogram_declaration
	      generic_subprogram_declaration ;; after 'generic'
	      null_procedure_declaration)
	     (setq result (ada-wisi-which-function-1
			   (wisi-cache-text (wisi-forward-find-token '(FUNCTION PROCEDURE) (point-max)))
			   nil))) ;; no 'body' keyword in subprogram bodies

	    (subprogram_body
	     (setq result (ada-wisi-which-function-1
			   (wisi-cache-text (wisi-forward-find-token '(FUNCTION PROCEDURE) (point-max)))
			   nil)))

	    (task_type_declaration
	     (setq result (ada-wisi-which-function-1 "task" t)))

	    ))
	result))
    ))

;;;; debugging
(defun ada-wisi-debug-keys ()
  "Add debug key definitions to `ada-mode-map'."
  (interactive)
  (define-key ada-mode-map "\M-e" 'wisi-show-parse-error)
  (define-key ada-mode-map "\M-h" 'wisi-show-containing-or-previous-cache)
  (define-key ada-mode-map "\M-i" 'wisi-goto-statement-end)
  (define-key ada-mode-map "\M-j" 'wisi-show-cache)
  (define-key ada-mode-map "\M-k" 'wisi-show-token)
  )

(defun ada-wisi-number-p (token-text)
  "Return t if TOKEN-TEXT plus text after point matches the
syntax for a real literal; otherwise nil. point is after
TOKEN-TEXT; move point to just past token."
  ;; test in test/wisi/ada-number-literal.input
  ;;
  ;; starts with a simple integer
  (let ((end (point)))
    ;; this first test must be very fast; it is executed for every token
    (when (and (memq (aref token-text 0) '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
	       (string-match "^[0-9_]+$" token-text))
      (cond
       ((= (char-after) ?#)
	;; based number
	(forward-char 1)
	(if (not (looking-at "[0-9a-fA-F_]+"))
	    (progn (goto-char end) nil)

	  (goto-char (match-end 0))
	  (cond
	   ((= (char-after) ?#)
	    ;; based integer
	    (forward-char 1)
	    t)

	   ((= (char-after) ?.)
	    ;; based real?
	    (forward-char 1)
	    (if (not (looking-at "[0-9a-fA-F]+"))
		(progn (goto-char end) nil)

	      (goto-char (match-end 0))

	      (if (not (= (char-after) ?#))
		  (progn (goto-char end) nil)

		(forward-char 1)
		(setq end (point))

		(if (not (memq (char-after) '(?e ?E)))
		    ;; based real, no exponent
		    t

		  ;; exponent?
		  (forward-char 1)
		  (if (not (looking-at "[+-]?[0-9]+"))
		      (progn (goto-char end) t)

		    (goto-char (match-end 0))
		    t
		)))))

	   (t
	    ;; missing trailing #
	    (goto-char end) nil)
	   )))

       ((= (char-after) ?.)
	;; decimal real number?
	(forward-char 1)
	(if (not (looking-at "[0-9_]+"))
	    ;; decimal integer
	    (progn (goto-char end) t)

	  (setq end (goto-char (match-end 0)))

	  (if (not (memq (char-after) '(?e ?E)))
	      ;; decimal real, no exponent
	      t

	    ;; exponent?
	    (forward-char 1)
	    (if (not (looking-at "[+-]?[0-9]+"))
		(progn (goto-char end) t)

	      (goto-char (match-end 0))
	      t
	      ))))

       (t
	;; just an integer
	t)
       ))
    ))

(defun ada-wisi-setup ()
  "Set up a buffer for parsing Ada files with wisi."
  (wisi-setup '(ada-wisi-comment
		ada-wisi-before-cache
		ada-wisi-after-cache)
	      'ada-wisi-post-parse-fail
	      ada-wisi-class-list
	      ada-grammar-wy--keyword-table
	      ada-grammar-wy--token-table
	      ada-grammar-wy--parse-table)

  ;; Handle escaped quotes in strings
  (setq wisi-string-quote-escape-doubled t)

  (set (make-local-variable 'comment-indent-function) 'wisi-comment-indent)
  )

(add-hook 'ada-mode-hook 'ada-wisi-setup)

(setq ada-fix-context-clause 'ada-wisi-context-clause)
(setq ada-goto-declaration-end 'ada-wisi-goto-declaration-end)
(setq ada-goto-declaration-start 'ada-wisi-goto-declaration-start)
(setq ada-goto-declarative-region-start 'ada-wisi-goto-declarative-region-start)
(setq ada-goto-end 'wisi-goto-statement-end)
(setq ada-goto-subunit-name 'ada-wisi-goto-subunit-name)
(setq ada-in-paramlist-p 'ada-wisi-in-paramlist-p)
(setq ada-indent-statement 'wisi-indent-statement)
(setq ada-make-subprogram-body 'ada-wisi-make-subprogram-body)
(setq ada-next-statement-keyword 'wisi-forward-statement-keyword)
(setq ada-on-context-clause 'ada-wisi-on-context-clause)
(setq ada-in-case-expression 'ada-wisi-in-case-expression)
(setq ada-prev-statement-keyword 'wisi-backward-statement-keyword)
(setq ada-reset-parser 'wisi-invalidate-cache)
(setq ada-scan-paramlist 'ada-wisi-scan-paramlist)
(setq ada-show-parse-error 'wisi-show-parse-error)
(setq ada-which-function 'ada-wisi-which-function)

(provide 'ada-wisi)
(provide 'ada-indent-engine)

;; end of file
