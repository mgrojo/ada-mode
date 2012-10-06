;;; Ada mode indentation engine, based on SMIE
;;
;; FIXME: not using lexical-binding because we might port this back to Emacs 23
;;
;; [1] ISO/IEC 8652:201z (draft 18); Ada 2012 reference manual
;;
;; Copyright (C) 2012  Free Software Foundation, Inc.
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
;;; History: see ada_mode.el
;;
;;; code style
;;
;; I don't use 'pcase', because it gives _really_ confusing errors
;; when I forget a ')' somewhere. Even worse, the error message is
;; given when you use edebug on a defun, not when you eval it. This
;; code is hard enough to debug!
;;
;;; debugging hints:
;;
;; Put an edebug break in smie-next-sexp, just after 'toklevels' is
;; set. Then you can see which tokens are processed.
;;
;; Use the interactive functions in the debug section below to move
;; across small parts of the syntax.

(require 'ada-mode)
;; FIXME: maybe ada-mode should require the default indentation engine, provide a way for user to override?

(eval-when-compile (require 'cl)); 'case'
(require 'smie)

;;; user variables

(defgroup ada-indentation nil
  "Indentation options for Ada source."
  :group 'ada)

(defcustom ada-indent 3
  "*Size of Ada default indentation, when no other indentation is used.

An example is :
procedure Foo is
begin
>>>null;"
  :type 'integer  :group 'ada-indentation)

(defcustom ada-indent-broken 2
  "*Number of columns to indent the continuation of a broken line.

An example is :
   My_Var : My_Type :=
   >>(Field1 => Value);"
  :type 'integer :group 'ada-indentation)

(defalias 'ada-broken-indent 'ada-indent-broken)
(make-obsolete-variable
 'ada-broken-indent
 'ada-indent-broken
 "Emacs 24.4, Ada mode 5.0")

(defcustom ada-indent-record-rel-type 3
  "*Indentation for 'record' relative to 'type' or 'use'.

An example is:
   type A is
   >>>record"
  :type 'integer :group 'ada-indent)

(defcustom ada-indent-when 3
  "*Indentation for 'when' relative to 'exception' or 'case'.

An example is:
   case A is
   >>>when B =>"
  :type 'integer :group 'ada-indent)

(defalias 'ada-when-indent 'ada-indent-when)
(make-obsolete-variable
 'ada-when-indent
 'ada-indent-when
 "Emacs 24.4, Ada mode 5.0")

;;; grammar

(defconst ada-indent-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '(;; non-terminal syntax terms not otherwise expanded
      (identifier)
      (operator)

      ;; BNF from [1] appendix P, vastly simplified
      ;; (info "(aarm2012)Annex P" "*info Annex P*")
      ;;
      ;; We only need enough of the grammar to allow indentation to
      ;; work; see (info "(elisp)SMIE Grammar")
      ;;
      ;; The important operation in indentation is finding the
      ;; beginning of the current Ada statement or declaration. In
      ;; SMIE terms, that means finding the 'parent' of the current
      ;; sexp; the leftmost keyword.
      ;;
      ;; This is done using the `smie-backward-sexp' function. It
      ;; moves back thru a chain of keywords, matching the precedence
      ;; levels. For example, consider the following declaration:
      ;;
      ;;    type Type_1 (Discriminant_1 : Integer) is null record;
      ;;
      ;; When `smie-backward-sexp' moves backward thru this, starting
      ;; at ";", it sees the following grammar values:
      ;;
      ;; (; 23 22)
      ;; (null_record 0 (179))
      ;; (is-type 153 0)
      ;; nil
      ;; nil
      ;; (type (169) 153)
      ;;
      ;; The first 'nil' is from the parethesis (the discriminant);
      ;; they are skipped in one step. The second nil is the type
      ;; name.
      ;;
      ;; Starting with "null_record", the right level of the next
      ;; keyword must match the left token of the current
      ;; keyword. When we encounter a keyword with a list for the
      ;; right level, or a level that doesn't match, we've found the
      ;; parent.
      ;;
      ;; The primary work in building the grammar is finding ways to
      ;; avoid conflicts without breaking the ability to find
      ;; parents. One approach is to leave out as many keywords as
      ;; possible, another is to refine Ada keywords into several
      ;; different smie keywords.
      ;;
      ;; SMIE automatically allows balanced parens anywhere, so we
      ;; don't need to declare argument lists or discriminant lists
      ;; in the grammar.
      ;;
      ;; Ada keywords that are not needed to find parents, or
      ;; otherwise help in indentation, do not need to be present in
      ;; the grammar; they will be treated as identifiers.
      ;;
      ;; For example, most uses of "null" are not in the grammar;
      ;; "null record" is, in order to distinguish it from
      ;; "... record null; end record". Since "null record" occurs at
      ;; the end of a declaration, but "record null" in the middle,
      ;; they must be different keywords in the grammar, so they can
      ;; have different left and right levels. We can leave "null" as
      ;; an identifier, and refine "record" to "record-null" in this case.
      ;;
      ;; ';' has a similar problem; it is used in several different
      ;; constructs at different levels. We solve that by declaring
      ;; it as the separator for each of those constructs. (SMIE
      ;; grammars cannot properly represent statement terminators).
      ;;
      ;; Other keywords are similarly refined to avoid grammar
      ;; conflicts. Sometimes it is tempting to refine two adjacent
      ;; keywords into one token. But that causes problems; for
      ;; example, if a user breaks a line in the middle of that
      ;; combined token, we have to recognize that and move to the
      ;; start before calling the regular parsing logic. It is better
      ;; to either refine both tokens, or leave one as an identifier
      ;; and refine the other.
      ;;
      ;; For all ';' separated non-terminals, the first and last
      ;; tokens in the syntax should be a keyword; that reduces the
      ;; number of conflicts. An exception is when there is a single
      ;; keyword in the syntax; then a trailing name is ok, and can
      ;; reduce the number of refined keywords we need. There are a
      ;; couple of other exceptions, noted below.
      ;;
      ;; There is no need for sexps with only one keyword in the
      ;; grammar; finding the parent of a single keyword is trivial
      ;; :). On the other hand, keeping them, where the keyword is
      ;; already defined for other purposes, does no harm, and is
      ;; less jarring to read.

      ;; We list the non-terminals in alphabetical order, since there
      ;; isn't any more reasonable order. We use the same names as [1]
      ;; Annex P as much as possible.
      ;;
      ;; Refined token naming convention:
      ;;
      ;; If an Ada keyword is refined, all occurances of the keyword
      ;; in the smie grammar must be refined. Use "-op" for
      ;; operators, "-other" if no better name is available. That way
      ;; it is clear when a keyword is being left as an indentifier.

      (accept_statement
       ("accept" identifier "do" statements "end-block")
       ("accept" identifier))

      (aggregate
       ("(" association_list  ")"))
      ;; this also covers other parenthesized lists; enumeration type
      ;; declarations, subprogram calls.

      (aspect_specification
       ("with-aspect" aspect_list))

      (aspect_list
       (aspect_item)
       (aspect_list "," aspect_item))

      (aspect_item
       (name "=>-other" name))

      (association_list
       (association)
       (association_list "," association))

      (association
       (expression)
       (expression "=>-other" expression))

      (case_statement_alternative
       ("when-case" expression "=>-when" statements)); "|" is an identifier

      (context_clause
       (context_item)
       (context_item ";" context_item))

      (context_item
       ("with-context" name); need name so `smie-forward-sexp' from beginning of buffer works
       ("use" name))
      ;; no need to distinguish between "use" as a context clause and
      ;; "use" as a declaration.

      (declaration
       (entry_body)
       (exception_declaration)
       (formal_package_declaration)
       (formal_subprogram_declaration)
       (generic_package_declaration)
       (package_body)
       (package_specification)
       (package_body)
       (protected_body)
       (subprogram_declaration)
       (subprogram_body)
       (subtype_declaration)
       (task_body)
       (type_declaration)
       (object_declaration)
       )

      (declarations
       (declaration)
       (declaration ";" declaration))

      (entry_body
       ("entry" identifier "when-entry" expression "is-entry_body" declarations "begin-body" statements "end-block"))

      (exception_declaration
       (identifer ":-object" "exception-declare"))

      (exception_handler
       ("when-case" object_declaration "=>-when" statements)
       ("when-case" identifier "=>-when" statements)); "|" is an identifier
      ;; no need to distinguish between when-exception, when-case.

      (exit_statement
       ("exit-other"); from anywhere. leaving identifier out
       ("exit-when" "when-exit" expression))

      (expression
       ;; We don't need operators at all in the grammar; they do not
       ;; affect indentation.
       (name)
       (aggregate))

      ;; Formal generic parameters. Most formal_* are covered in this
      ;; grammar by the equivalent non-formal syntax.

      (formal_package_declaration
       ;; with package defining_identifier is new generic_package_name
       ;;    formal_package_actual_part [aspect_specification];
       ("with-formal" "package-formal" identifier "new" name))
      ;; leave "is" an identifier. name after "new" to match other
      ;; uses. formal_package_actual_part is an association list
      ;; FIXME: aspects not implemented yet

      (formal_subprogram_declaration
       ;; leaving "with" "function" "procedure" unrefined gives
       ;; conflicts with the non-formal use.

       ("with-formal" "function-formal" name "return-formal" name); trailing name same as non-formal
       ("with-formal" "procedure-formal" name); trailing name same as non-formal
       ;; We leave out [is name], because "is" is an identifier here.
       )

      (generic_package_declaration
       ;; No need to distinguish between 'declarations' and
       ;; 'generic_formal_parameter_declaration' for our purposes.
       ("generic" declarations
	"package-generic" identifier "is-package" declarations "private" declarations "end-block")
       ("generic" declarations
	"package-generic" identifier "is-package" declarations "end-block"))

      (interface_list
       ;; The Ada grammar sometimes has "name and interface_list".
       ;; We can't (and don't need to) distinguish that from "interface_list"
       (name)
       (interface_list "and-interface_list" name))

      (iteration_scheme
       ("for" identifier "in" name))
      ;; "reverse" is an identifer
      ;; FIXME: container iterators allow ":" here (not tested yet)

      (name
       (identifier)
       (name "." identifier) ; selected_component
       ;; Remember that parenthesis are simply skipped by SMIE
       ;; (unless we are indenting inside them; then they are
       ;; boundaries). So we don't need to represent subprogram
       ;; parameter lists, or array indices here (no aggregates in
       ;; 'expression').
       )

      (loop_statement
       (identifier ":-label" iteration_scheme "loop-body" statements "end-loop" "loop-end")
       (iteration_scheme "loop-body" statements "end-loop" "loop-end")
       ("loop-open" statements "end-loop" "loop-end")
       )

      (object_declaration
       (identifier ":-object" name)); same as ":-object" in extended return

      (package_specification
       ("package-plain" name "is-package" declarations "private-body" declarations "end-block")
       ("package-plain" name "is-package" declarations "end-block"))

      (package_body
       ;; Leaving 'package body' as separate tokens causes problems
       ;; in refine-is, so we leave "body" as an identifier.
       ("package-plain" name "is-package" declarations "begin-body" statements "end-block")
       ("package-plain" name "is-package" "separate"))

      (protected_body
       ("protected-body" identifier "is-protected_body" declarations "end-block")
       ("protected-body" name "is-protected-body" "separate"))

      (select_statement
       ;; accept_statement, delay_statement are covered here by
       ;; "statements". "terminate" looks like a procedure call, so
       ;; we leave it as an identifier.
       ("select-open" "when-select" expression "=>-when" statements
	"or-select" "when-select" expression "=>-when" statements
	"else" statements "end-select" "select-end")

       ("select-open" statements
	"or-select" statements
	"else" statements "end-select" "select-end"))

      (statement
       (expression); covers procedure calls

       ;; assignment_statement
       (name ":=" expression)

       (accept_statement)

       ;; block_statement
       (identifier ":-label" "declare-label" declarations "begin-body" statements
		   "exception-block" exception_handler "end-block")
       ("declare-open" declarations "begin-body" statements "end-block")
       ("begin-open" statements "end-block")
       ;; we don't need to repeat the exception handler in the other
       ;; cases; once is enough to establish the precendence of "exception-block".

       ;; case_statement
       ("case" name "is-case" case_statement_alternative "end-case" "case-end")

       ;; delay_statement
       ("delay" expression)

       (exit_statement)

       ;; if_statement
       ("if-open" expression "then" statements "end-if" "if-end")
       ("if-open" expression "then" statements "else" statements "end-if" "if-end")
       ("if-open" expression "then" statements
	"elsif" expression "then" statements
	"else" statements "end-if" "if-end")

       (loop_statement)
       ("return-stmt")

       ;; extended_return_statement
       ("return-ext" identifier ":-object" name); same as initialized object declaration
       ("return-ext" identifier ":-object" name "do" statements "end-return" "return-end")

       (select_statement)
       )

      (statements
       (statement)
       (statement ";" statement))

      (subprogram_body
       ;; access is an identifier
       ("function" name "return-spec" name "is-subprogram_body" declarations "begin-body" statements "end-block")
       ("function" name "return-spec" "is-subprogram_body" "separate")
       ("procedure" name "is-subprogram_body" declarations "begin-body" statements "end-block")
       ("procedure" name "is-subprogram_body" "separate"))

      (subprogram_declaration
       ("function" name "return-spec" name)
       ;; trailing name makes "return-spec" have the same binding as
       ;; in subprogram_body; that avoids recursion between refine-is
       ;; and refine-return

       ("overriding" "function-overriding" name "return-spec" name)
       ("overriding" "procedure-overriding" name)
       ;; We need "overriding" as a token, because the indentation
       ;; policy for it is an exception to the hanging policy:
       ;;
       ;;    overriding
       ;;    procedure (...);

       ("procedure" name); same as 'procedure name is-subprogram_body'
       ;; We keep this, because it is too jarring to not find it here :).
       ;;
       ;; We leave out ("procedure" name "is" "null") here; we
       ;; are treating a couple of occurences of "is", and most
       ;; occurences of "null", as identifiers.
       )

      (subtype_declaration
       ("subtype" identifier "is-subtype" name))

      (task_body
       ("task-body" identifier "is-task_body" declarations "begin-body" statements "end-block"))

      (type_declaration
       ;; access_type_definition
       ("type" identifier "is-type-access")
       ;; Any occurance of "is-type" as the last keyword in a
       ;; declaration must be refined; otherwise it is ambiguous
       ;; with several other declarations.

       ;; We don't include access-to-subprogram in the grammar,
       ;; because we want to identify 'function' and 'procedure' in
       ;; these types, so we can indent the parameter list relative
       ;; to them. So we allow them to be parents. This also greatly
       ;; simplifies refine-is.

       ;; array_type_definition; we leave "array" as an identifier
       ("type" identifier "is-type" expression "of")

       ;; derived_type_declaration
       ("type" identifier "is-type" "new" name); same as below
       ("type" identifier "is-type" "new" name "with-new" "private-with")
       ("type" identifier "is-type" "new" name "with-new" "record-null"); "null" is an identifier
       ("type" identifier "is-type" "new" name "with-record")
       ;; We refine "with" to "with-record" when it is followed
       ;; by "record", so that it is a closer to match
       ;; "type", since "record-open" is an opener.
       ;;
       ;; We don't include record-definition in
       ;; derived_type_definition, because we want to indent "end
       ;; record" relative to "record".

       ;; enumeration_type_definition
       ("type" identifier "is-type-enumeration")
       ;; enumeration literals are an aggregate, which is ignored.

       ;; {ordinary_ | decimal_} fixed_point_definition
       ;; "delta" and "digits" are left as identifiers
       ;; floating_point_definition, integer_type_definition; "range" is an identifier
       ("type" identifier "is-type-numeric")

       ;; incomplete_type_declaration ::= type defining_identifier [discriminant_part] [is tagged];
       ;;
       ;; We don't need the variant without "is tagged", since it has
       ;; only one keyword.  We need "is-type-record" when this is
       ;; followed by a record_definition; that's covered below. We don't need "tagged" as a keyword.
       ("type" identifier "is-type-tagged")

       ;; interface_type_definition, formal_interface_type_definition
       ("type" identifier "is-type" "interface-plain")
       ("type" identifier "is-type" "interface-and" "and-interface" interface_list)

       ;; modular_type_definition
       ("type" identifier "is-type" "mod-type")
       ;; "mod" is an operator, so it has to be in the grammar. We
       ;; also want something following "is-type", unless we treat
       ;; this occurence of "is" as an identifier. FIXME: On the
       ;; other hand, we don't really need "mod" as an operator. It
       ;; also occurs in other syntax; wait until we test those to
       ;; decide to leave it as an identifier.

       ;; private_extension_declaration
       ("type" identifier "is-type" "new" name "with-new" "private-with")
       ("type" identifier "is-type" "new" interface_list "with-new" "private-with")
       ;; leaving 'with' and 'private' as separate tokens causes conflicts

       ;; private_type_declaration
       ("type" identifier "is-type" "private-type-spec")

       ;; protected_type_declaration, single_protected_declaration
       ;;
       ;; We don't need "protected" in the grammar anywhere, so leave
       ;; it as an identifier; this simplifies access-to-subprogram
       ;; types, since we can just ignore "protected" there.  Note
       ;; that in a single_protected_declaration, we are refining
       ;; "protected" to "type". However, "is" in protected type
       ;; declaration is a block start keyword, while "is-type" in
       ;; general is not, so we need to make that a distinct
       ;; keyword. We use "is-type-block" instead of
       ;; "is-type-protected", because it covers task types as well.
       ("type" identifier "is-type-block" declarations "private-body" declarations "end-block")
       ("type" identifier "is-type-block" declarations "end-block"); task, or protected with no private part
       ("type" identifier "is-type" "new" interface_list "with-new" declarations
	"private-body" declarations "end-block")

       ;; record_type_definition
       ("type" identifier "is-type" "record-null")
       ("type" identifier "is-type-record")
       ;; We refine "is-type" to "is-type-record" when it is followed
       ;; by "record-open", so that it is a closer to match "type", since
       ;; "record-open" is an opener.

       ;; record_definition
       ("record-open" declarations "end-record" "record-end")
       ("record-open" declarations "end-record" "record-end-aspect" aspect_specification)
       ;; No need to distinguish between 'declarations' and
       ;; 'component_list'. We don't include record_definition in
       ;; record_type_definition or derived_type_definition, because
       ;; we want to indent "end record" relative to "record", not
       ;; "type".

       ;; task_type_declaration, single_task_declaration: as for
       ;; protected_type_declaration, we don't need "task" in the
       ;; grammar. Task entries can have entry families, but that's a
       ;; parenthesized expression, so we don't need it in the
       ;; grammar either. However, task_body includes "begin", while
       ;; protected_body doesn't.

       ); type_declaration
      )); smie-bnf->prec2
    ))

;;; utils for refine-*, forward/backward token

(defconst ada-indent-block-keywords
  '("=>-when"
    "begin-body"
    "begin-open"
    "declare-open"
    "declare-label"
    "do"
    "else"
    ;; "end-block" is never a block start; treated separately
    "exception-block"
    "generic"
    "is-entry_body"
    "is-package"
    "is-protected_body"
    "is-subprogram_body"
    "is-task_body"
    "is-type-block"
    "loop-body"
    "loop-open"
    "private-body"
    "record-open"
    "select-open"
    "then")
  ;; We don't split this into start and end lists, because most are
  ;; both. The keywords that are an end but never a start are in
  ;; ada-indent-block-end-keywords. Being a start but never end is not
  ;; a problem.
  ;;
  ;; This is not a subset of the open portion of the grammar
  ;; open/close list; that is restricted to keywords that don't bind
  ;; on the left.
  "Keywords that start or end indented blocks, excluding keywords that always end blocks.")

(defconst ada-indent-block-end-keywords
  '("elsif"
    "end-case"
    "end-block"
    "end-if"
    "end-loop"
    "end-record"
    "end-return"
    "end-select"
    "or-select"
    "package-generic"
    ;; "when-case" does not act like a block keyword; it is an opener
    )
  "Keywords that always end indented blocks.")

(defconst ada-indent-pre-begin-tokens
  '("declare-open"
    "declare-label"
    "is-subprogram_body"
    "is-package"
    "is-task_body"
    "is-entry_body")
  ;; found by searching [1] Annex P for "begin", then checking for
  ;; refinements. Thus in Annex P order.
  "All refined tokens that are followed by \"begin\" in an Ada declaration.")

(defconst ada-indent-labeled-keywords
  '("declare"
    "for"
    "loop"
    "while")
  "Unrefined keywords that can be preceded by a label.")

(defvar ada-indent-refine-all nil
  "Non-nil if parsing forward from beginning of buffer to refine
  keywords. Most tokens don't need to do this; those that do
  let-bind this to t.")

(defvar ada-indent-refine-forward-to nil
  "Position of token that initiated refine-all.")

(defun ada-indent-matching-end (keyword)
  "Return a list of keywords that could end the block started by KEYWORD.
This is found by searching the grammar; it will produce results
that are not allowed by Ada, but we don't care."
  ;; IMPROVEME: change to take the block start keyword and the current
  ;; keyword, and return t/nil
  (let ((prec (nth 2 (assoc keyword ada-indent-grammar)))
	(list (cdr ada-indent-grammar))
	(found nil)
	toklevels)
    (while (setq toklevels (pop list))
      (if (and
	   (integerp (nth 1 toklevels))
	   (= prec (nth 1 toklevels)))
	  (setq found (cons (nth 0 toklevels) found))
	))
    found))

(defun ada-indent-skip-param_list (forward)
  ;; While refining tokens, we don't want to call smie-next-sexp,
  ;; because it relies on refined tokens. So we call the C scanner
  ;; directly when we need to skip a parenthesis (see the lisp source
  ;; for forward-sexp).
  (let ((forward-sexp-function nil))
    (if forward
	(forward-sexp)
    (backward-sexp))))

(defun ada-indent-next-name (next-token forward)
  "Skip over a name using function NEXT-TOKEN. Here a 'name'
consists of identifiers, dots, anything that looks like a
parameter list (could be an array index), and identifiers.
Return the token that isn't part of the name (which may be found
before any name is seen).  Return empty string if encounter
beginning of buffer."
  (let (token)
    (catch 'quit
      (while
	  (progn
	    (setq token (funcall next-token))
	    (if (equal "" token)
		;; We hit a paren or bob
		(progn
		  (when (bobp) (throw 'quit nil))
		  (if forward
		      (when (eq (char-after) ?\)) (throw 'quit ")"))
		    (when (eq (char-before) ?\() (throw 'quit "(")))
		  (ada-indent-skip-param_list forward)
		  ;; the next token might be another paren, so we loop
		  t)
	      ;; not a paren or bob
	      (setq token (nth 0 (assoc token smie-grammar)))
	      (or (not token); not a keyword, so it must be an identifier
		  (equal token ".")))))
      )
    token))

(defun ada-indent-backward-name ()
  (ada-indent-next-name 'ada-indent-backward-token nil))

;; (defun ada-indent-forward-name ()
;;   (ada-indent-next-name 'ada-indent-forward-token))

(defun ada-indent-next-token-unrefined (next-token forward)
  "Move to the next token using function NEXT-TOKEN. Skips parentheses.
Return the token, or wrong paren, or empty string if encounter beginning of
buffer."
  (let (token)
    (while
	(progn
	  (setq token (funcall next-token))
	  (if (equal "" token)
	      ;; We hit a parenthesis or bob
	      (progn
		(when (bobp) (throw 'quit nil))
		(if forward
		    (when (eq (char-after) ?\)) (throw 'quit ")"))
		  (when (eq (char-before) ?\() (throw 'quit "(")))
		(ada-indent-skip-param_list next-token forward)))))
    token))

(defun ada-indent-backward-token-unrefined ()
  (ada-indent-next-token-unrefined 'smie-default-backward-token nil))

(defconst ada-indent-type-modifiers '("abstract" "tagged" "limited"))

(defun ada-indent-skip-type-modifiers ()
  "Skip forward tokens that are in `ada-indent-type-modifiers', return the following token."
  (let (result)
    (while (member (setq result (smie-default-forward-token)) ada-indent-type-modifiers))
    result))

(defun ada-indent-refine-error (msg)
  (error
   (concat msg " %d : "
	   (buffer-substring-no-properties
	    (progn (beginning-of-line) (point))
	    (progn (end-of-line) (point))))
   (point)))

;;; refine-*

;; ada-indent-forward-token calls refine-* with point
;; after token; ada-indent-backward with point before token.
;;
;; refine-* must not move point.
;;
;; So each refine defun must take a 'forward' arg, and in general
;; start with:
;;
;; (save-excursion
;;  (when forward (smie-default-backward-token))
;;
;; If at all possible, these functions should not use
;; smie-backward-sexp, especially if the starting token is a guess,
;; because it is often too greedy; it leads to horrible recursive
;; parsing with wrong guesses, and ends up reporting no match. It also
;; relies on refined tokens, which can lead to recursion.
;;
;; We also try to avoid parsing forward, since that won't work while
;; the user is typing code. But sometimes it can't be helped.

(defun ada-indent-refine-: (token forward)
  (let ((token (save-excursion
		 (when (not forward) (smie-default-forward-token))
		 (smie-default-forward-token))))
    (if (member token ada-indent-labeled-keywords)
	":-label"
      ":-object")))

(defun ada-indent-refine-=> (token forward)
  (let ((token (save-excursion
		 (when forward (smie-default-backward-token))
		 (ada-indent-backward-name))))

    (if (member token '(":-object" ; in exception handler
			"when-case"
			"when-select"))
	"=>-when"
      "=>-other")))

(defun ada-indent-refine-and (token forward)
  ;; 'and' occurs in interface types and logical expressions
  ;; (search for interface_list in [1] annex P):
  ;;
  ;; 1) [formal_]derived_type_definition ::=
  ;;    type defining_identifier [discriminant_part] is
  ;;       [abstract] [limited] new parent_subtype_indication
  ;;       [[and interface_list] record_extension_part]
  ;;       [aspect_specification];
  ;;
  ;;    preceding refined keyword: "new"
  ;;    skip: name
  ;;    keyword: "and-interface_list"
  ;;
  ;; 2) interface_type_definition ::=
  ;;    (type identifier is) [limited | task | protected | synchronized] interface [and interface_list]
  ;;
  ;;    preceding unrefined keyword: "interface"
  ;;    preceding refined keyword: "interface-and"
  ;;    skip: nothing
  ;;    keyword: "and-interface"
  ;;
  ;; 3) interface_list ::= interface_subtype_mark {and interface_subtype_mark}
  ;;
  ;;    preceding refined keyword: "and-interface_list"
  ;;    skip: name
  ;;    keyword: "and-interface_list"
  ;;
  ;; 4) private_extension_declaration ::=
  ;;    type defining_identifier [discriminant_part] is
  ;;       [abstract] [limited | synchronized] new ancestor_subtype_indication
  ;;       [and interface_list] with private
  ;;       [aspect_specification];
  ;;
  ;;    preceding keyword: "new"
  ;;    skip: name
  ;;    keyword: "and-interface_list"
  ;;
  ;; 5) task_type_declaration, single_task_declaration ::=
  ;;       task [type] defining_identifier [known_discriminant_part] [aspect_specification]
  ;;       [is [new interface_list with] task_definition];
  ;;
  ;;    preceding keyword: "new"
  ;;    skip: name
  ;;    keyword: "and-interface_list"
  ;;
  ;; 6) protected_type_declaration ::=
  ;;    protected type defining_identifier [known_discriminant_part] [aspect_specification] is
  ;;       [new interface_list with] protected_definition;
  ;;
  ;;    preceding keyword: "new"
  ;;    skip: name
  ;;    keyword: "and-interface_list"
  ;;
  ;; 7) single_protected_declaration ::=
  ;;    protected defining_identifier [aspect_specification] is
  ;;    [new interface_list with] protected_definition;
  ;;
  ;;    preceding keyword: "new"
  ;;    skip: name
  ;;    keyword: "and-interface_list"
  ;;
  ;; All other occurences are logical expressions, returning "and-op".
  (save-excursion
    (when forward (smie-default-backward-token))

    (or
     (when (equal "interface" (save-excursion (smie-default-backward-token)))
       "and-interface"); 2

     (let ((token (ada-indent-backward-name)))
       (cond
	((or (equal token "and-interface_list"); 3
	     (equal token "new")); 1, 4, 5, 6, 7
	   "and-interface_list")
	(t "and"))); operator identifier
     )))

(defun ada-indent-refine-begin (token forward)
  ;; If "begin" follows "declare" or "is", it is not an opener. Otherwise it is an opener.
  ;;
  ;; Consider this code:
  ;;
  ;;   package body Ada_Mode.Nominal is
  ;;
  ;;	  function Function_1b return Float
  ;;	  is
  ;;	     Local_1 : constant := 3.0;
  ;;	  begin
  ;;	     declare -- no label, zero statements between begin, declare
  ;; 	     begin
  ;;		return Local_1;
  ;;	     end;
  ;; 	  end Function_1b;
  ;;   begin
  ;;	  null;
  ;;   end Ada_Mode.Nominal;
  ;;
  ;; To refine the final "begin", we need to look all the way back to
  ;; "package". We can't do that while the intervening "begin"s are
  ;; unrefined.
  ;;
  ;; So we enter a special mode; start smie-forward-sexp at the
  ;; beginning of the buffer, and when `ada-indent-refine-begin' is
  ;; called, examine `smie--levels' (which holds the token stack used
  ;; by `smie-forward-sexp) to see how to refine each "begin". When
  ;; we've reached the "begin" we are refining, throw
  ;; `ada-indent-refine-all-quit', and resume normal operation.
  ;;
  (catch `local-quit
    (if ada-indent-refine-all
	;; Parsing from beginning of buffer; examine stack
	(let ((stack smie--levels)
	      stack-token
	      (token nil))
	  (while (null token)
	    (setq stack-token (nth 0 (rassoc (pop stack) ada-indent-grammar)))
	    (cond
	     ((equal stack-token ";") nil)
	     ((member stack-token ada-indent-pre-begin-tokens)
	      (setq token "begin-body"))
	     (t
	      (setq token "begin-open"))))

	  (if (>= (point) ada-indent-refine-forward-to)
	      (throw 'ada-indent-refine-all-quit token)
	    (throw 'local-quit token)))

      ;; not refining-all
      (when forward (smie-default-backward-token))

      (let ((ada-indent-refine-all t)
	    (ada-indent-refine-forward-to (point))
	    toklevels)
	(save-excursion
	  (catch 'ada-indent-refine-all-quit
	    (goto-char (point-min))
	    ;; skip context clauses, parse compilation-unit.
	    (while (equal ";" (nth 2 (setq toklevels (smie-forward-sexp))))
	      (goto-char (nth 1 toklevels))
	      ;; loop should exit on 'ada-indent-refine-all-quit,
	      ;; but if we have a bug, we don't want to loop forever
	      ;; here.
	      ))))
      )))

(defun ada-indent-refine-case (token forward)
  (let ((token (save-excursion
		 (when forward (smie-default-backward-token))
		 (smie-default-backward-token))))

    (if (equal token "end")
	"case-end"
      "case")))

(defun ada-indent-refine-declare (token forward)
  (let ((token (save-excursion
		 (when forward (smie-default-backward-token))
		 (smie-default-backward-token))))

    (if (equal token ":")
	"declare-label"
      "declare-open")))

(defun ada-indent-refine-end (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    (let ((token (save-excursion (smie-default-forward-token) (smie-default-forward-token))))
      (cond
       ((equal "case" token) "end-case")
       ((equal "if" token) "end-if")
       ((equal "loop" token) "end-loop")
       ((equal "record" token) "end-record")
       ((equal "return" token) "end-return")
       ((equal "select" token) "end-select")
       (t "end-block"))
      )))

(defun ada-indent-refine-exception (token forward)
  ;; identifier : exception;
  ;;
  ;; begin
  ;;    statements;
  ;; exception
  ;; when =>
  (let ((token (save-excursion
		 (when forward (smie-default-backward-token))
		 (smie-default-backward-token))))
    (cond
     ((equal token ":") "exception-declare")
     ((equal token ";") "exception-block")
     (t (ada-indent-refine-error "unrecognized 'exception'"))
     )))

(defun ada-indent-refine-exit (token forward)
  (save-excursion
    (let ((token
	   (progn
	     (when (not forward) (smie-default-backward-token))
	     (smie-default-forward-token))))
      (cond
       ((equal "when" token) "exit-when")
       ((equal "when" (smie-default-forward-token)); loop label
	"exit-when")
       (t "exit-other")
       ))
    ))

(defun ada-indent-refine-if (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    (let ((token (save-excursion (smie-default-backward-token))))
      (cond
       ((equal "end" token) "if-end")
       (t "if-open"))
      )))

(defun ada-indent-refine-interface (token forward)
  ;; see `ada-indent-refine-and' for Ada syntax cases
  (let ((token (save-excursion
		 (when (not forward) (smie-default-forward-token))
		 (smie-default-forward-token))))

    (if (equal token "and")
	"interface-and"
      "interface-plain")))

(defun ada-indent-refine-is (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    ;; too many occurences to document them all at once.

    (or
     ;; First try simple, common constructs.

     (save-excursion
       ;; This is a special case because "protected" and "task" are
       ;; not keywords, so ada-indent-backward-name doesn't find
       ;; them. Fortunately, single tasks and protected objects cannot
       ;; have discriminants. FIXME: they can have aspect specs.
       (let ((token (progn
		      (smie-default-backward-token); identifier
		      (smie-default-backward-token)))) ; "protected", "task", "body", "type", "subtype"
	 (cond
	  ((member token '("protected" "task")) "is-type-block")
	  ((equal token "body")
	    (setq token (smie-default-backward-token))
	    (cond
	     ((equal token "protected") "is-protected_body")
	     ((equal token "task") "is-task_body")
	     ))
	  ((equal token "subtype") "is-subtype")
	  ((equal token "type")
	    (setq token (smie-default-backward-token))
	    (when (member token '("protected" "task")) "is-type-block"))
	 )))

     (let* (pos
	    (token (save-excursion (prog1 (ada-indent-backward-name) (setq pos (point))))))
       (cond
	((equal token "case") "is-case")

	((member token '("package-plain" "package-generic")) "is-package")

	((equal token "package-formal") "is"); identifier

	((equal token "procedure-formal") "is"); identifier

	((member token '("procedure" "procedure-overriding"))
	 ;;  procedure name is abstract;
	 ;;  procedure name is null;
	 ;;  procedure name is declarations begin statements end;
	 (let ((token (save-excursion
			(smie-default-forward-token); is
			(smie-default-forward-token))))
	   (cond
	    ((member token '("abstract" "null")) "is")
	    (t "is-subprogram_body"))))

	((equal token "return-formal")
	 ;; with function identifier return name is name
	 "is")

	((equal token "return-spec")
	 ;; function identifier return name is declarations begin
	 "is-subprogram_body")

	((equal token "type")
	 (or
	  (let ((token (save-excursion (goto-char pos) (smie-default-backward-token))))
	    (when (member token '("protected" "task")) "is-type-block"))

	  (let* (pos
		(token
		 (save-excursion
		   (smie-default-forward-token); is
		   (prog1 (smie-default-forward-token)
		     (setq pos (point))))))
	    (cond
	     ((equal token ""); paren or end of buffer; assume paren
	      "is-type-enumeration");; type identifier is (...)

	     ((member token '("not" "access")) "is-type-access")

	     ((equal token "record") "is-type-record")

	     ((and
	       (equal token "tagged")
	       (let ((token (save-excursion (goto-char pos) (smie-default-forward-token))))
		 (cond
		  ((equal token ";")
		   ;; type Incomplete_Type_1 (Discriminant_1 : Integer) is tagged; -- in spec
		   ;; type Incomplete_Type_1 (Discriminant_1 : Integer) is tagged null record; -- in body
		   "is-type-tagged")
		  ((equal token "record") "is-type-record")
		  (t nil))
		 )))

	     ((member token ada-indent-type-modifiers)
	      ;; we could have:
	      ;;
	      ;;    type Derived_Type_2 is abstract tagged limited new ...
	      ;;    type Private_Type_1 is abstract tagged limited null record;
	      ;;    type Private_Type_2 is abstract tagged limited record ...
	      (let ((token (save-excursion (goto-char pos) (ada-indent-skip-type-modifiers))))
		(cond
		 ((equal token "record") "is-type-record")
		 (t "is-type"))))

	     ;; numeric types
	     ;;
	     ;; signed_integer_type_definition ::= [type identifier is] range expression .. expression
	     ;;
	     ;; modular_type_definition ::= [type identifier is] mod static_expression
	     ;;
	     ;; floating_point_definition ::= [type identifier is] digits static_expression
	     ;;    [range expression .. expression]
	     ;;
	     ;; ordinary_fixed_point_definition ::= [type identifier is] delta expression
	     ;;    range expression .. expression
	     ;;
	     ;; decimal_fixed_point_definition ::= [type identifier is] delta expression digits expression
	     ;;    [   range expression .. expression]
	     ((equal token "range") "is-type-numeric")
	     ((equal token "mod") "is-type")
	     ((equal token "digits") "is-type-numeric")
	     ((equal token "delta") "is-type-numeric")

	     (t "is-type")); all others
	    )))))

     ;; now more complicated things
     (save-excursion
       ;; entry body with params: "entry" identifier "("...")" "when" exp "is"
       ;;
       ;; If we can be guessing wrong here, we can't use
       ;; smie-backward-sexp (because it will just get confused). So
       ;; far, this is the only possibility at this point, so we don't
       ;; really need to check, but we want to identify missing
       ;; cases. FIXME: use ada-indent-refine-all
       (if (equal "entry" (nth 2 (smie-backward-sexp "is-entry_body"))) "is-entry_body"))

     (ada-indent-refine-error "unrecognized 'is'")
     ))
  )

(defun ada-indent-refine-loop (token forward)
  (let ((token (save-excursion
		 (when forward (smie-default-backward-token))
		 (smie-default-backward-token))))
    ;; "loop" occurs in:
    ;;
    ;; 1) loop_statement ::=
    ;;        [loop_statement_identifier:]
    ;;           [iteration_scheme] loop
    ;;              sequence_of_statements
    ;;            end loop [loop_identifier];
    ;;
    ;; The second "loop" is "loop-end".
    ;;
    ;; If there is no loop label or iteration scheme, the first "loop"
    ;; is "loop-open"; otherwise it is "loop-body".
    ;;
    ;;     iteration_scheme ::= while condition
    ;;        | for loop_parameter_specification
    ;;        | for iterator_specification
    ;;
    ;;     loop_parameter_specification ::=
    ;;        defining_identifier in [reverse] discrete_subtype_definition
    ;;
    ;;     iterator_specification ::=
    ;;         defining_identifier in [reverse] iterator_name
    ;;       | defining_identifier [: subtype_indication] of [reverse] iterable_name

    (if (equal token "end")
	"loop-end"

      (setq token
	    (save-excursion
	      (smie-default-backward-token); loop
	      (ada-indent-backward-name)))

      (if (member token '(":-label" "in" "while" "of"))
	  ;; FIXME: iterators not tested yet; "of" will probably be refined.
	  "loop-body"
	"loop-open"))))

(defun ada-indent-refine-mod (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    ;; "mod" occurs in:
    ;;
    ;; 1) modular_type_definition as part of a full_type_declaration ::=
    ;;
    ;;    type identifier [known_discriminant_part] is mod expression;
    ;;
    ;;    preceding refined keyword: "is-type"
    ;;    preceding unrefined keyword: "is"
    ;;    skip: nothing
    ;;    keyword: "mod-type"
    ;;
    ;; 2) multiplying_operator ::= * | / | mod | rem
    ;;
    ;;    preceding keyword: none, this must be the default.
    ;;    keyword: "mod-op"
    ;;
    ;; 3) formal_modular_type_definition ::= mod <>
    ;;
    ;;    not implemented yet
    ;;
    ;; 4) mod_clause in a record_representation_clause ::=
    ;;
    ;;    for first_subtype_local_name use
    ;;    record at mod static_expression;
    ;;       {component_clause}
    ;;    end record;
    ;;
    ;;    not implemented yet.
    ;;
    (if (equal "is" (save-excursion (smie-default-backward-token)))
	"mod-type"
      "mod"))); operator identifier

(defun ada-indent-refine-or (token forward)
  (let ((token (save-excursion
		 (when (not forward) (smie-default-forward-token))
		 (smie-default-forward-token))))
    (cond
     ((member token '("accept" "when" "terminate")) "or-select")
     (t "or")))); operator identifier

(defun ada-indent-refine-package (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    (or
     (let ((token (save-excursion (smie-default-backward-token))))
       (cond
	((equal token "") "package-plain");; beginning of buffer
	((equal token "access") "package-access")
	((equal token "with") "package-formal")
	))

     ;; FIXME: this is ok for a library level [generic] package alone
     ;; in a file. But it could be a problem for a nested [generic]
     ;; package. Idea: "end" can't occur in generic formal parameters;
     ;; search for "end|generic". Or use ada-indent-refine-all
     ;; approach.
     (if (equal "generic" (save-excursion (nth 2 (smie-backward-sexp "package-generic"))))
	 "package-generic")

     "package-plain")
    ))

(defun ada-indent-refine-private (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    ;; 'private' occurs in:
    ;;
    ;; 1) [formal_]private_type_declaration
    ;;
    ;;    type defining_identifier [discriminant_part] is [[abstract] tagged] [limited] private
    ;;       [with aspect_mark];
    ;;
    ;;    succeeding unrefined token: "with" or ";"
    ;;    skip: nothing
    ;;    token: private-type-spec
    ;;
    ;; 2) [non]limited_with_clause
    ;; 3) formal_derived_type_definition
    ;; 4) library_item
    ;; 5) package_specification
    ;;
    ;;    package defining_program_unit_name [aspect_specification] is
    ;;       {basic_declarative_item}
    ;;    [private
    ;;       {basic_declarative_item}]
    ;;    end [[parent_unit_name.]identifier]
    ;;
    ;;    need ada-indent-refine-all
    ;;    token: private-body
    ;;
    ;; 6) private_extension_declaration
    ;;
    ;;    type defining_identifier [discriminant_part] is
    ;;       [abstract] [limited | synchronized] new ancestor_subtype_indication
    ;;       [and interface_list] with private
    ;;       [aspect_specification];
    ;;
    ;;    preceding unrefined token: "with" or ";"
    ;;    skip: nothing
    ;;    token: private-with
    ;;
    ;; 7) protected_definition
    ;; 8) task_definition
    ;;
    (cond
     ((equal "with" (save-excursion (smie-default-backward-token)))
      "private-with"); 6

     ((member (save-excursion
		(smie-default-forward-token); private
		(smie-default-forward-token))
	      '("with" ";"))
      "private-type-spec"); 1

     (t "private-body"))); all others
  )

(defun ada-indent-refine-protected (token forward)
      ;; 'protected' occurs in:
      ;;
      ;; 1) interface_type_definition
      ;;
      ;;    not implemented
      ;;
      ;; 2) access_to_subprogram_definition
      ;;
      ;;    handled by maybe-refine-is
      ;;
      ;; 3) access_definition in an object_declaration
      ;;
      ;;    not implemented
      ;;
      ;; 4) protected_type_declaration
      ;;
      ;;    protected type defining_identifier [known_discriminant_part]
      ;;       [aspect_specification] is [new interface_list with] protected_definition;
      ;;
      ;; 5) single_protected_declaration
      ;;
      ;;    same as protected_type_declaration, without "type"
      ;;    not implemented
      ;;
      ;; 6) protected_body :=
      ;;
      ;;    protected body defining_identifier ...
      ;;
      ;; 7) protected_body_stub
      ;;
      ;;    not implemented
      ;;
  (let ((token (save-excursion
		 (when (not forward) (smie-default-backward-token))
		 (smie-default-forward-token))))
    (cond
     ((equal token "body") "protected-body")
     (t "protected")); an identifier
    ))

(defun ada-indent-refine-record (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    (let ((token (smie-default-backward-token)))
      (cond
       ((equal token "end")
	(if (equal "with" (save-excursion
			    (smie-default-backward-token); record
			    (smie-default-backward-token)))
	    "record-end-aspect";
	  "record-end"))
       ((equal token "null") "record-null")
       (t "record-open")
       ))
    ))

(defun ada-indent-refine-return (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    ;; 'return' occurs in:
    ;;
    ;; 1) a function subprogram_declaration or formal_subprogram_declaration:
    ;;
    ;;    1a) function_specification ::= function defining_designator parameter_and_result_profile
    ;;
    ;;    preceding token: "function", "function-overriding"
    ;;    token: "return-spec"
    ;;
    ;;    1b) formal_concrete_subprogram_declaration ::= with subprogram_specification ...
    ;;
    ;;    preceding token: "function-formal"
    ;;    token: "return-formal"
    ;;
    ;; 2) a function specification in function body :
    ;;
    ;;    function identifier (...) return [access] name is
    ;;
    ;;    preceding token: "function", "function-overriding"
    ;;    token: "return-spec"
    ;;
    ;; 3) a return statement:
    ;;
    ;;    3a) return;
    ;;    3b) return exp;
    ;;
    ;;    token: "return-stmt"
    ;;
    ;; 4) an extended return statement:
    ;;
    ;;    return identifier : name;
    ;;
    ;;    token: "return-ext" (4a)
    ;;
    ;;    return identifier : name do statements end return;
    ;;
    ;;    token: "return-ext" (4b), "return-end" (4c)
    ;;
    ;; 5) an access function type declaration:
    ;;
    ;;    type name is access [protected] function identifier (...) return [access] name;
    ;;
    ;;    preceding token: "function"  _not_ overriding or generic
    ;;    token: "return-spec"
    ;;
    ;; So we have to look both forward and backward to resolve this.
    (or
     (if (equal "end" (save-excursion (smie-default-backward-token))) "return-end"); 4c

     ;; Do this now, otherwise we can't distinguish between:
     ;;
     ;; function F1 return Integer;
     ;; return 0;
     ;;
     (let ((token (save-excursion (ada-indent-backward-name))))
       (cond
	((member token '("function" "function-overriding"))
	 "return-spec"); 1a, 2, 5

	((equal token "function-formal")
	 "return-formal"); 1b
	))

     (save-excursion
       ;; FIXME: test this at end of buffer (not very
       ;; likely to happen, but possible while entering code)
       (let ((token (progn
		      (smie-default-forward-token); return
		      (smie-default-forward-token))))
	 (cond
	  ((equal token ";") "return-stmt"); 3a
	  (t
	   (setq token (smie-default-forward-token)); identifier
	   (cond
	    ((member token '(":" ":-do")) "return-ext"); 4a, 4b
	    (t "return-stmt"); 3b
	    ))))))
    ))

(defun ada-indent-refine-select (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    (let ((prev-token (smie-default-backward-token)))
      (cond
       ((equal prev-token "end") "select-end")
       (t "select-open"))
  )))

(defun ada-indent-refine-subprogram (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    (let ((prev-token (smie-default-backward-token)))
      (cond
       ((equal prev-token "with")	(concat token "-formal"))
       ((equal prev-token "overriding") (concat token "-overriding"))
       (t token)
  ))))

(defun ada-indent-refine-task (token forward)
  (let ((token (save-excursion
		 (when (not forward) (smie-default-backward-token))
		 (smie-default-forward-token))))
    (cond
     ((equal token "body") "task-body")
     (t "task")); an identifier
    ))

(defun ada-indent-refine-when (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    ;; "when" occurs in:
    ;;
    ;; 1) record variant part ::=
    ;;
    ;;       case discriminant_direct_name is
    ;;       when discrete_choice_list => component;
    ;;       when discrete_choice_list => component;
    ;;
    ;;   token: "when-case"
    ;;
    ;; 2) case_expression_alternative ::=
    ;;       (case selecting_expression is
    ;;        when discrete_choice_list => dependent_expression,
    ;;        when discrete_choice_list => dependent_expression)
    ;;
    ;;   token: "when-TBD"
    ;;
    ;; 3) case_statement_alternative ::=
    ;;       case selecting_expression is
    ;;       when discrete_choice_list => statement;
    ;;       when discrete_choice_list => statement;
    ;;
    ;;   token: "when-case"
    ;;
    ;; 4) exit_statement ::=
    ;;       exit [loop_name] [when condition];
    ;;
    ;;   preceding unrefined keyword: exit
    ;;   skip: nothing or identifier
    ;;   token: "when-exit"
    ;;
    ;; 5) entry_barrier in an entry_body ::=
    ;;       entry identifier (...) when condition
    ;;
    ;;   preceding refined token: "entry"
    ;;   skip: name
    ;;   token: "when-entry"
    ;;
    ;; 6) guard in a selective_accept :
    ;;
    ;;       select
    ;;          [when expression => ]
    ;;             select_alternative
    ;;       {or
    ;;          [when expression => ]
    ;;             select_alternative }
    ;;
    ;;   preceding unrefined token: "select", "or"
    ;;   skip: nothing
    ;;   token: "when-select"
    ;;
    ;; 7) exception_handler ::=
    ;;
    ;;       exception
    ;;       when [choice_parameter_specification:] exception_choice {| exception_choice} =>
    ;;          statement;
    ;;       when [choice_parameter_specification:] exception_choice {| exception_choice} =>
    ;;          statement;
    ;;
    ;;   token: "when-TBD"
    ;;

    (or
     (if (member (save-excursion (smie-default-backward-token)) '("select" "or"))
	 "when-select")

     (save-excursion
       (if (equal (smie-default-backward-token) "exit")
	   "when-exit"
	 (if (equal (smie-default-backward-token) "exit"); loop label
	     "when-exit")))

     (if (equal "entry" (ada-indent-backward-name))
	 "when-entry"; 5
       "when-case"); 1
     )))

(defun ada-indent-refine-with (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    ;; "with" occurs in:
    ;;
    ;; 1) record_extension_part in a [formal_]derived_type_definition in a type_declaration
    ;;
    ;;    type ... is ... new parent_subtype_indication [[and interface_list] with record_definition]
    ;;
    ;;    succeeding unrefined token: "record"
    ;;    skip: nothing
    ;;    keyword: "with-record"
    ;;
    ;; 2) extension_aggregate ::=
    ;;       (ancestor_part with record_component_association_list)
    ;;
    ;;    not implemented yet
    ;;
    ;; 3) private_extension_declaration ::=
    ;;       type defining_identifier [discriminant_part] is
    ;;       [abstract] [limited | synchronized] new ancestor_subtype_indication
    ;;       [and interface_list] with private
    ;;       [aspect_specification];
    ;;
    ;;    succeeding unrefined token: "private"
    ;;    skip: nothing
    ;;    keyword: "with-new"
    ;;
    ;; 4) task_type_declaration, single_task_declaration,
    ;;    protected_type_declaration, single_protected_declaration ::=
    ;;       {protected | task} [type] defining_identifier [known_discriminant_part]
    ;;       [aspect_specification] [is
    ;;       [new interface_list with]
    ;;       {protected | task}_definition];
    ;;
    ;;    preceding refined token: "new", "and-interface_list"
    ;;    skip: name
    ;;    succeeding unrefined token: "protected", "task"
    ;;    keyword: "with-new"
    ;;
    ;; 5) requeue_statement ::= requeue procedure_or_entry_name [with abort];
    ;;
    ;;    not implemented yet
    ;;
    ;; 6) [non]limited_with_clause in a context_clause ::=
    ;;
    ;;    [limited] [private] with library_unit_name {, library_unit_name};
    ;;
    ;;    preceding unrefined token: ["limited"] ["private"] ";" bob
    ;;    keyword: "with-context"
    ;;
    ;; 7) raise_statement ::= raise;
    ;;       | raise exception_name [with string_expression];
    ;;
    ;;    not implemented yet
    ;;
    ;; 8) formal_concrete_subprogram_declaration ::=
    ;;       with subprogram_specification [is subprogram_default]
    ;;       [aspect_specification];
    ;;
    ;;    succeeding unrefined token: "function", "procedure"
    ;;    skip: none
    ;;    keyword: with-formal
    ;;
    ;; 9) formal_abstract_subprogram_declaration ::=
    ;;       with subprogram_specification is abstract [subprogram_default]
    ;;       [aspect_specification];
    ;;
    ;;    same as formal_concrete
    ;;
    ;; 10) formal_package_declaration ::=
    ;;       with package defining_identifier is new generic_package_name  formal_package_actual_part
    ;;       [aspect_specification];
    ;;
    ;;    succeeding unrefined token: "package"
    ;;    skip: none
    ;;    keyword: with-formal
    ;;
    ;; 11) aspect_specification ::=
    ;;       with aspect_mark [=> aspect_definition] {, aspect_mark [=> aspect_definition] }
    ;;
    ;;    succeeding refined token: "=>", ",", ";"
    ;;    skip: name
    ;;
    ;;    preceding unrefined token: "record", FIXME: others?
    ;;    skip: nothing
    ;;    token: with-aspect
    ;;
    (or
     (let ((token (save-excursion
		    (smie-default-forward-token); with
		    (smie-default-forward-token))))
       (cond
	((member token '("function" "package" "procedure")) "with-formal"); 8
	((equal token "record") "with-record"); 1
       ))

     ;; this checks for preceding ";", so it has to be after the
     ;; above; with-formal also has a preceding ";"
     (let ((token (save-excursion (smie-default-backward-token))))
       (cond
	((equal token "record") "with-aspect"); 11
	((or
	  (equal token ""); bob
	  (member token '("limited" "private" ";")))
	 "with-context"); 6
	(t nil)))

     "with-new")
    ))

;;; forward/backward token

;; ada-indent-forward-token must produce the same results on a string
;; of tokens as ada-indent-backward-token. To ensure that, we use an
;; alist to specify the refining functions.

(defconst ada-indent-next-token-alist
  ;; Alphabetical order.
  '(("<>;" ";")
    ;; These three chars all have punctuation syntax. We need that
    ;; for expressions, so we don't change it. "<>" is an identifier
    ;; in the grammar, so we can just refine this to ";"

    (":" 	 ada-indent-refine-:)
    ("=>" 	 ada-indent-refine-=>)
    ("and" 	 ada-indent-refine-and)
    ("begin" 	 ada-indent-refine-begin)
    ("case" 	 ada-indent-refine-case)
    ("declare" 	 ada-indent-refine-declare)
    ("end" 	 ada-indent-refine-end)
    ("exception" ada-indent-refine-exception)
    ("exit" 	 ada-indent-refine-exit)
    ("function"  ada-indent-refine-subprogram)
    ("interface" ada-indent-refine-interface)
    ("if" 	 ada-indent-refine-if)
    ("is" 	 ada-indent-refine-is)
    ("loop" 	 ada-indent-refine-loop)
    ("mod" 	 ada-indent-refine-mod)
    ("or" 	 ada-indent-refine-or)
    ("package" 	 ada-indent-refine-package)
    ("private" 	 ada-indent-refine-private)
    ("procedure" ada-indent-refine-subprogram)
    ("protected" ada-indent-refine-protected)
    ("record" 	 ada-indent-refine-record)
    ("return" 	 ada-indent-refine-return)
    ("select" 	 ada-indent-refine-select)
    ("task" 	 ada-indent-refine-task)
    ("when" 	 ada-indent-refine-when)
    ("with" 	 ada-indent-refine-with)
    )
  "Alist of (token keyword) or (token refine-defun), used by
`ada-indent-next-token' to refine tokens.
`token' is the string returned by smie-default-[forward|backward]-token.
If `keyword' is a string, it is returned.
If `keyword' is a defun, it is called with two args (token forward);
forward is t if moving forward (point at right end of token), nil if
moving backward (point at left end of token). It must return the
refined token or nil.
If a token is not in the alist, it is returned unrefined.")

;; cache operations

(defvar ada-indent-cache-max 0
  "Maximimum position in buffer where ada-indent token refinement cache is valid.")
(make-variable-buffer-local 'ada-indent-cache-max)

(defun ada-indent-invalidate-cache()
  "Invalidate the ada-indent token cache for the current buffer."
  (interactive)
  (setq ada-indent-cache-max 0))

(defun ada-indent-get-cache (pos)
  "Return refined token string from the `ada-indent-cache' text property at POS."
  (get-text-property pos 'ada-indent-cache))

(defun ada-indent-put-cache (pos token)
  "Set TOKEN as the refined token string in the `ada-indent-cache' text property at POS.
Return TOKEN."
  ;; IMPROVEME: we could store `smie--levels' as well, and resume
  ;; `smie-forward-sexp' from the previous cached token, instead of
  ;; from (point-min). But so far things are fast enough (and finding
  ;; "the previous cached token" is not trivial, unless it is at
  ;; ada-indent-cache-max).
  (put-text-property pos (+ 1 pos) 'ada-indent-cache token)
  (setq ada-indent-cache-max (max ada-indent-cache-max pos))
  token)

(defun ada-indent-after-change (begin end length)
  (setq ada-indent-cache-max (min ada-indent-cache-max begin)))

(defun ada-indent-next-token (forward)
  "Move to the next token; forward if FORWARD non-nil, backward otherwise.
Return the token text or a refinement of it. Manage the refinement cache."
  ;; We only need the cache for a couple tokens, but since we have the
  ;; mechanism, it doesn't hurt to use it for all of them. So we
  ;; implement it here.

  (let* (cache-pos
	 (token (if forward
		    (progn
		      (forward-comment (point-max))
		      (setq cache-pos (point))
		      (smie-default-forward-token))
		  (prog1
		      (smie-default-backward-token)
		    (setq cache-pos (point)))))
	 (refine (cadr (assoc token ada-indent-next-token-alist))))
    (cond
     ((stringp refine) refine)

     ((functionp refine)
      (or (and
	   (<= cache-pos ada-indent-cache-max)
	   (ada-indent-get-cache cache-pos))
	  (ada-indent-put-cache cache-pos (funcall refine token forward))))

     (t token))
    ))

(defun ada-indent-forward-token () (ada-indent-next-token t))
(defun ada-indent-backward-token () (ada-indent-next-token nil))

;;; indent rules

(defun ada-indent-rule-current (offset)
  "Indent relative to the current line"
  (cons 'column (+ (save-excursion (back-to-indentation) (current-column)) offset)))

(defun ada-indent-keyword-p (token)
  (assoc token ada-indent-grammar))

(defun ada-indent-opener-p (token)
  (listp (nth 1 (assoc token ada-indent-grammar))))

(defun ada-indent-goto-parent (child up)
  "Goto a parent (defined by where smie-backward-sexp stops).
If CHILD is non-nil and a smie keyword, find its parent (which may be itself, if it is a parent and UP is 1).
Otherwise, find the previous keyword, start over.
UP is an integer; find that many parents.
Point must be on the start of `child', or on the start of the
preceding unrefined token (if resuming from a previous call).
Return same struct as `smie-backward-sexp'.
Point is left on the start of the result token if it is a closer,
otherwise on the following token."
  (let ((count up)
	(token
	 (cond
	  ((ada-indent-keyword-p child)
	   child)
	  (t (ada-indent-backward-name)))))

    (setq token
	  (list (nth 1 (assoc token ada-indent-grammar)) (point) token))
    (while (> count 0)
      (when (listp (nth 0 token))
	;; token is an opener; found a parent
	(setq count (- count 1)))
      (if (> count 0)
	  (progn
	    (cond
	     ((listp (nth 0 token))
	      ;; When smie-backward-sexp stops because the found
	      ;; token is an opener, it leaves point on the
	      ;; opener. We can't call smie-backward-sexp again
	      ;; with that token, so we have to move to the next
	      ;; keyword first. That might also be an opener, so
	      ;; we loop again before calling smie-backward-sexp
	      (progn
		(setq token (ada-indent-backward-name))
		(setq token
		      (list (nth 1 (assoc token ada-indent-grammar)) (point) token))))

	     (t
	      ;; When smie-backward-sexp stops because the found token
	      ;; is has a lower precedence level, it leaves point on
	      ;; the following token. If we call smie-backward-sexp
	      ;; again with point there, we won't move. So we go to
	      ;; the found token first.
	      (if (not (= (point) (nth 1 token)))
		  (goto-char (nth 1 token)))
	      (setq token (smie-backward-sexp (nth 2 token)))
	      (setq count (- count 1)))))
	))

    ;; If we stopped because we found an opener, point is on token,
    ;; which is where we want it. If we stopped because the next token
    ;; has a lower precedence level, point is on the following token;
    ;; (nth 1 token) gives the position at the start of the lower
    ;; precendence token.

    token))

(defun ada-indent-rule-parent (offset child)
  "Find the parent of CHILD (using `ada-indent-goto-parent'),
return an indent by OFFSET relevant to it. Does not stop on
CHILD. Preserves point.  If CHILD must be non-nil and a keyword
or \"(\", and point must be at the start of CHILD."
  (save-excursion
    (let
	((parent
	  (ada-indent-goto-parent
	   child
	   (if (and child
		    (or
		     (equal child "(")
		     (ada-indent-opener-p child)))
	       2
	     1))))
      (cond
       ((equal (nth 2 parent) "") nil); stopped due to "("; indent to the (, not the line it is on
       (t (back-to-indentation)))
      (cons 'column (+ (current-column) offset))
    )))

(defun ada-indent-goto-statement-start (child)
  "Move point to the start of the statement/declaration
containing point. If point is in a parenthesized list, move to
the start of the current list element. Return the found parent
token, or nil. If point is at statement or list element start,
does nothing. If CHILD is non-nil, it must be a keyword, and
point must be at the start of CHILD."
  ;; See ada-indent-show-statement-start for interactive call of
  ;; this.
  ;;
  ;; Because we did not include access-to-subprogram in the grammar,
  ;; we have to consider whether the first parent we find is the
  ;; "right" one.
  ;;
  ;; Consider an access-to-function type declaration, with a return
  ;; type that is access to function:
  ;;
  ;;    type Function_Access_Type_8 is access
  ;;       protected function
  ;;          (Param_1 : in Integer)
  ;;          return access function
  ;;             (Param_2 : in Float)
  ;;             return Standard.Float;
  ;;
  ;;    type Another_Type is ...;
  ;;
  ;; Obviously, this could be repeated to any level. Here each
  ;; "function" keyword is a smie parent; ada-indent-goto-parent will
  ;; stop there. So we have to loop:
  ;;
  ;;    loop
  ;;       ada-indent-goto-parent
  ;;       if parent not in ("procedure" "function") then
  ;;          exit loop
  ;;       end if
  ;;    end loop
  ;;
  ;; The same loop works for access to procedure, although it will
  ;; only be executed once.
  ;;
  ;; FIXME: need a similar loop for "when-case"
  ;; (case_statement_alternative, exception_handler). Waiting until we
  ;; finish more "when" uses. At the moment,
  ;; case_statement_alternative and exception_handler are treated as
  ;; separate statements, not part of a larger statement.
  ;;
  ;; 'overriding' and 'protected' present another problem; the parent
  ;; is not at the beginning of the statement:
  ;;
  ;;    overriding procedure Name;
  ;;
  ;;    protected type Name is ...
  ;;
  ;; The same is true for any other leading Ada keyword that we leave
  ;; as an identifier. To handle this, we do `back-to-indentation'
  ;; once we find the right parent.
  ;;
  ;; We do _not_ need a similar loop for assignment statements:
  ;;
  ;;    A := B + C * D;
  ;;
  ;; because we set all operators to the same precedence
  ;; level. However, to find the start of A (which could be a complex
  ;; name), we must use ada-indent-backward-name.
  ;;
  ;; Object declarations can declare multiple objects:
  ;;
  ;;   A,
  ;;    B : type;
  ;;
  ;; The first keyword is ":"; we use ada-indent-backward-name.
  ;;
  ;; If we are inside parens:
  ;;
  ;;    procedure Procedure_1d
  ;;       (Item  : in out Parent_Type_1;
  ;;        Item_1 : in Character;
  ;;        Item_2 : out Character)
  ;;    is null;
  ;;
  ;;    procedure Procedure_1e (Item  : in out Parent_Type_1;
  ;;                            Item_1 : in Character;
  ;;                            Item_2 : out Character)
  ;;    is null;
  ;;
  ;; We want to align on the column following "(". On the line
  ;; containing "(", we want to leave point after the paren, not
  ;; before it. The other lines have no special considerations.
  ;;
  (let (parent-count parent pos)

    (catch 'done

      ;; We have to check the previous keyword for procedure calls,
      ;; "procedure" and "function", and a few other cases.  Many
      ;; statements have non-keyword tokens before the first token
      ;; (assignent), so we have to use use ada-indent-backward-name
      ;; to find the previous keyword.
      (save-excursion
	(let ((prev-token (ada-indent-backward-name)))
	  (if (or
	       (equal prev-token ";")
	       (member prev-token ada-indent-block-keywords))
	      (progn
		(smie-default-forward-token); ";" or block start
		(forward-comment (point-max)); also skips final whitespace
		(setq pos (point))))))

      ;; Make sure going to pos would be motion. In this case:
      ;;
      ;;    is begin
      ;;       return Integer (Function_1a);
      ;;
      ;; intending "return", goto-statement-start is called from
      ;; indent-after-keyword with point on "begin". backward-name
      ;; finds "is", which is a block keyword. Then we move back to
      ;; "begin" and set pos. No other blocks may be empty.
      (if (and pos (not (= pos (point)))) (progn (goto-char pos) (throw 'done nil))); we didn't find a parent

      (setq parent-count
	    (if (member child '("." "(" "record-open"))
		2
	      1))
      ;; If child is "." we are in the middle of an Ada name; the
      ;; first parent is the start of the name.
      ;;
      ;; If child is "(" the first run thru smie-backward-sexp
      ;; produces no motion; that causes the second run to FIXME: how
      ;; does this work?
      ;;
      ;; If child is record, that is it's own parent, and we want the
      ;; next one up.

      (setq parent (ada-indent-goto-parent child parent-count))

      (if (or
	   (equal (nth 2 parent) "(")
	   (equal (nth 2 parent) ";")
	   (member (nth 2 parent) ada-indent-block-keywords)
	   )
	  ;; goto-parent left point on token following parent; point is at statement start
	  (throw 'done (nth 2 parent)))

      (if (not (= (point) (nth 1 parent)))
	  ;; goto-parent stopped because of a lower-precedence token,
	  ;; not a closer; that might be because:
	  ;;
	  ;; 1) we are traversing a list of identifiers in an object declaration:
	  ;;
	  ;;    Integer_D, Integer_E, Integer_F :
	  ;;      Integer;
	  ;;
	  ;; 2) probably other situations.
	  ;;
	  ;; In case 1 we could call ada-indent-goto-parent again to
	  ;; get to the statement start. But this is also handled by
	  ;; the other code below, so we just go to the parent
	  ;; keyword.
	  (goto-char (nth 1 parent)))

      ;; handle access-to-subprogram and record types.
      (while (or (equal (nth 2 parent) "record-open")
		 (and (member (nth 2 parent) `("procedure" "function")) ; _not -overriding -generic
		      (member (save-excursion (smie-default-backward-token)) '("access" "protected"))))
	(setq parent (ada-indent-goto-parent parent 2)))

      (if (member (nth 2 parent) '(":" ":="))
	  (progn
	    (ada-indent-backward-name)
	    ;; we are now on the keyword before statement start; get back.
	    (smie-default-forward-token); ";" or block start
	    (forward-comment (point-max)))
	)
      (nth 2 parent))))

(defun ada-indent-rule-statement (offset child)
  "Find the start of the statement/declaration containing point (using
`ada-indent-goto-statement-start'), return an indent by OFFSET relevant
to it. Preserves point.  If CHILD is non-nil, point must be at
the start of CHILD, which must be a keyword."
  (save-excursion
    (let ((parent (ada-indent-goto-statement-start child)))
      (cond
       ((equal parent "(") nil); really indent to the (, not the line it is on
       (t (back-to-indentation)))
      (cons 'column (+ (current-column) offset)))))

;;;
(defun ada-indent-rules (method arg)
  ;; This is called from ada-indent-{before|after}-keyword; it must
  ;; not move point, and must return nil or ('column n).
  (case method
    (:elem
     ;; see comments at smie-rules-function
     (case arg
       (basic ada-indent)
       (args
	;; offset for function arguments.
	0))
     )

    (:before
     ;; arg is a smie keyword, at the beginning of a line. point is at
     ;; the start of the keyword.
     ;;
     ;; The general rule is to indent relative to the start of the
     ;; statement containing arg; there are some exceptions that we
     ;; check for first.
     ;;
     ;; If arg is at the start of the statement, we return nil; :after
     ;; then indents relative to the previous statement start.

     (cond
      ((or (equal arg "(")
	   (equal arg "return-spec"))
       ;; parenthesis occur in expressions, and after names, as array
       ;; indices, subprogram parameters, type constraints.
       ;;
       ;; "(" is in the smie grammar, but smie-forward-token
       ;; handles it specially, returning ("" nil 0). Then
       ;; ada-indent-before-keyword passes "(" here.
       ;;
       ;; We don't want ada-indent-rule-statement here; if this is the
       ;; parameter list for an anonymous subprogram in an access
       ;; type, we want to indent relative to "procedure" or
       ;; "function", not the type declaration start. Similarly for
       ;; the "return" in an access to function.
       (ada-indent-rule-parent ada-indent-broken arg))

      ((equal arg ")")
       ;; find the open paren
       (save-excursion
	 (forward-char 1)
	 (backward-sexp)
	 (cons 'column (current-column))))

      ((equal token "end-record")
       ;; goto-parent leaves point on "record-open".
       (save-excursion
	 (ada-indent-goto-parent arg 1)
	 (back-to-indentation)
	 (cons 'column (current-column))))

      ((equal token "record-open")
       ;; This indents the first line. The components are indented
       ;; relative to the line containing "record-open"; see "record-open" in
       ;; :after.
       (ada-indent-rule-statement ada-indent-record-rel-type arg))

      ((member arg '("procedure-overriding" "function-overriding"))
       (save-excursion
	 (smie-default-backward-token)
	 (cons 'column (current-column))))

      ((equal arg "when-case")
       ;; We want to indent relative to the statement start; "case",
       ;; "exception", etc.  We assume the previous lines are properly
       ;; indented, so just looking at the previous token works, and
       ;; simpler than parsing back thru lots of "with"s.
       (save-excursion
	 (let ((token (smie-default-backward-token)))
	   (cond
	    ((equal token ";")
	     ;; in the middle of a sequence of "when".
	     (back-to-indentation)
	     (cons 'column (- (current-column) ada-indent)))

	    ((equal token "is")
	     ;; First "when" in the statement. We need to give
	     ;; ada-indent-rule-statement the refined token
	     (ada-indent-rule-statement ada-indent-when (save-excursion (ada-indent-forward-token))))
	    ))))

      ((equal arg "with-context")
       (cons 'column 0))

      ((or (member arg ada-indent-block-end-keywords)
	   (member arg ada-indent-block-keywords))
       ;; Indenting a block keyword that closely follows a containing
       ;; block keyword is a complex special case.  In addition, the
       ;; first keyword of a block behaves differently because it is a
       ;; closer. See a complete set of patterns for declare/begin
       ;; blocks in test/ada_mode-nominal.adb. Here we summarize:
       ;;
       ;; 1) no statements between begin, declare
       ;;
       ;;    function Function_1b return Float
       ;;    is
       ;;    begin
       ;;       declare
       ;;       begin
       ;;
       ;;    Indenting "declare": goto-parent returns "function", with point on "function"
       ;;    Indenting "begin"  : goto-parent returns "declare", with point on "declare"
       ;;    Indenting "end"    : goto-parent returns "declare", with point on "declare"
       ;;
       ;; 2) one statement
       ;;
       ;;    function Function_1b return Float
       ;;    is
       ;;    begin
       ;;       P1;
       ;;       declare
       ;;       begin
       ;;
       ;;    Indenting "declare": goto-parent returns "begin", with point on P1
       ;;    Indenting "begin"  : goto-parent returns "declare", with point on "declare"
       ;;    Indenting "end"    : goto-parent returns "declare", with point on "declare"
       ;;
       ;; 3) two or more statements
       ;;
       ;;    function Function_1c return Float
       ;;    is
       ;;    begin
       ;;       P1;
       ;;       P2;
       ;;       declare
       ;;       begin
       ;;
       ;;    Indenting "declare": goto-parent returns ";", with point on P2
       ;;    Indenting "begin"  : goto-parent returns "declare", with point on "declare"
       ;;    Indenting "end"    : goto-parent returns "declare", with point on "declare"
       ;;
       ;; 4) Function 2a: Label before declare, no statements
       ;;    a) indenting label  : goto-parent returns "function", with point on "function"
       ;;    b) indenting declare: goto-parent returns "begin", with point on label
       ;;    c) indenting begin, end: goto-parent returns function "begin", with point on label
       ;;
       ;; 5) Function_2b: Label before declare, one statement
       ;;    a) indenting label  : goto-parent returns "function", with point on "function"
       ;;    b) indenting declare: goto-parent returns ";", with point on label
       ;;    c) indenting begin, end: ""
       ;;
       ;; 6) Function_2c: Label before declare, two statements
       ;;    a) indenting label  : goto-parent returns "begin", with point on P1
       ;;    b) indenting declare: goto-parent returns ";", with point on label
       ;;    c) indenting begin, end: ""
       ;;
       ;; 7) Function_2d: begin is first keyword, no statements
       ;;    a) indenting begin: goto-parent returns "function", with point on "function"
       ;;    b) indenting end  : goto-parent returns "begin", with point on "begin"
       ;;
       ;; 8) Function_2e: begin is first keyword, one statement
       ;;    a) indenting begin: goto-parent returns previous "begin", with point on "P1"
       ;;    b) indenting end  : goto-parent returns "begin", with point on "begin"
       ;;
       ;; 9) Function_2f: begin is first keyword, two statements
       ;;    a) indenting begin: goto-parent returns ";", with point on "P2"
       ;;    b) indenting end  : goto-parent returns "begin", with point on "begin"
       ;;
       ;; Note that we don't get here if indenting a block label;
       ;; that's handled in :after block-keyword, ";".
       ;;
       (save-excursion
	 (let (parent offset)
	   (cond
	    ((ada-indent-opener-p arg)
	     (setq parent (ada-indent-goto-parent arg 2))

	     (goto-char (nth 1 parent))
	     (if (equal (nth 2 parent) ";")
		 (setq offset 0)
	       (setq offset ada-indent)))

	    (t
	     (setq parent (ada-indent-goto-parent arg 1))
	     (setq offset 0))
	    )
	   (back-to-indentation)
	   (cons 'column (+ (current-column) offset)))
	 ))

      ((let (pos)
	 (setq pos (save-excursion (ada-indent-goto-statement-start arg) (point)))
	 (if (not (= pos (point)))
	     ;; Hanging; we are not at the start of a statement/declaration.
	     ;; Indent relative to the line the statement start is on.
	     (cons
	      'column
	      (+ (save-excursion
		   (goto-char pos)
		   (back-to-indentation)
		   (current-column))
		 ada-indent-broken)))
	 ))
      ))

;;; :after
    (:after
     ;; `arg' is a keyword at the end of a line, point is at start of
     ;; the keyword; we are indenting the following token, which may
     ;; not be a keyword.  :before is checked first, so we don't need
     ;; to consider those cases here.

     (cond
      ((equal arg "(")
       ;; Something like this:
       ;;
       ;;    type Correct_Indentation is
       ;;      (
       ;;       Value_1,
       ;;       Value_2
       ;;      );
       ;;
       ;; In this case, point is after the paren
       (cons 'column (current-column)))

      ((equal arg ")")
       ;; Find the open paren, then the parent. Not the statement
       ;; start; this could be the discriminant list of a record, or
       ;; the parameter list of an access-to-subprogram.
       (save-excursion
	 (backward-sexp)
	 (ada-indent-rule-parent ada-indent-broken "(")))

      ((member arg '("," ":-label" ";"))
       (ada-indent-rule-statement 0 arg))

      ((equal arg "record-end")
       ;; We are indenting the aspect specification for the record.
       (back-to-indentation)
       (cons 'column (current-column)))

      ((equal arg "record-open")
       ;; We are indenting the first component in a record_definition:
       ;;
       ;; type Private_Type_1 is abstract tagged limited
       ;;    record
       ;;       Private_1 : Integer;
       ;;       Private_2 : Integer;
       ;;    end record;
       ;;
       ;; type Derived_Type_2 is new Parent_Type
       ;;    with record
       ;;       Derived_1 : Integer;
       ;;       Derived_2 : Integer;
       ;;    end record
       ;;    with (Packed => True);
       ;;
       ;; point is on "record".
       ;;
       ;; Indenting "record" was handled in :before.  Component_2 will
       ;; be indented relative to Component_1.
       ;;
       ;; "end record" will be indented relative to "type".
       ;;
       ;; aspect clause will be indented relative to "record-end".
       ;;
       (back-to-indentation)
       (cons 'column (+ (current-column) ada-indent)))

      ((equal arg "return-spec")
       ;; see comments in :before "("
       (ada-indent-rule-parent ada-indent-broken arg))

      ((member arg ada-indent-block-keywords)
       (ada-indent-rule-statement ada-indent arg))

     (t (ada-indent-rule-statement ada-indent-broken arg))
      ))
    ))

;;; smie-indent-functions
;;
;; each must not move point, and must return a column (as an integer) or nil.
;;
;; declared in order called

(defun ada-indent-comment ()
  "Compute indentation of a comment."
  ;; Check to see if we are at a comment
  (and (smie-indent--bolp)
       (let ((pos (point)))
         (save-excursion
           (beginning-of-line)
           (and (re-search-forward comment-start-skip (line-end-position) t)
                (eq pos (or (match-end 1) (match-beginning 0))))))

       ;; yes, we are at a comment; indent to previous code
       (save-excursion
         (forward-comment (- (point)))
	 ;; indent-before-keyword will find the keyword _after_ the
	 ;; comment, which could be 'private' for example, and that
	 ;; would align the comment with 'private', which is wrong. So
	 ;; we call a subset of the indentation functions.
	 ;; ada-indent-default handles this case:
	 ;;
	 ;;     procedure Incorrect_Sub
	 ;;       --  comment
	 ;;       (

	 (if debug-on-error
	     (or
	      (ada-indent-wrapper 'smie-indent-bob)
	      (ada-indent-wrapper 'ada-indent-after-keyword)
	      (ada-indent-wrapper 'ada-indent-default)
	      )
	   (or
	    (smie-indent-bob)
	    (ada-indent-after-keyword)
	    (ada-indent-default)
	    )))
       ))

(defun ada-indent-record()
  "Indent a line containing the \"record\" that starts a record component list."
  (save-excursion
    (if (not (equal "type" (smie-default-forward-token)))
	(let*
	    ((token (progn
		      (goto-char (+ 1 (line-end-position)))
		      ;; forward-comment doesn't work from within a comment
		      (forward-comment -1)
		      (ada-indent-backward-token)))
	     (indent
	      (if (equal token "record-open")
		  (ada-indent-rule-statement ada-indent-record-rel-type token))))

	  (cond
	   ((null indent) nil)
	   ((eq (car-safe indent) 'column) (cdr indent)))
	  ))))

(defun ada-indent-before-keyword()
  "Replacement for `smie-indent-keyword', tailored to Ada.
It requires `ada-indent-rule' to return nil or ('column column),
never just an offset (since we would not know what the offset was
relative to)."
  (let*
      ((token (save-excursion (ada-indent-forward-token)))
       char
       (indent
	(or
	 (and
	  (equal token ""); ( ) "
	  ;; " is handled by ada-indent-default
	  (setq char
		(case (char-after)
		  (?\( "(")
		  (?\) ")")
		  (?\" nil)))
	  (ada-indent-rules :before char))

	 (and
	  (ada-indent-keyword-p token)
	  (ada-indent-rules :before token)))))

      ;; Here we replace smie-indent--rules, so ada-indent-rules
      ;; cannot use smie-rule-parent; we use ada-indent-rule-parent
      ;; We do _not_ call smie-indent-virtual.
      (cond
       ((null indent) nil)
       ((eq (car-safe indent) 'column) (cdr indent))
       (t (error "Invalid `ada-indent-rules' result %s" indent)))
      ))

(defun ada-indent-after-keyword()
  "Replacement for `smie-indent-after-keyword', tailored to Ada.
It requires `ada-indent-rule' to return nil or ('column column),
never just an offset (since we would not know what the offset was
relative to)."
  (let*
      ((pos (point));; ada-indent-rules wants point at the start of its arg
       (token (ada-indent-backward-token))
       (indent
	(or
	 (and
	  (equal token ""); ( ) "
	  ;; ) " are handled by ada-indent-default; we only need to handle ( here.
	  (case (char-before)
	    (?\( t)
	    (?\) nil)
	    (?\" nil))
	  (ada-indent-rules :after "("))

	 (and
	  (ada-indent-keyword-p token)
	  (ada-indent-rules :after token)))))

      (goto-char pos)

      ;; Here we replace smie-indent--rules, so ada-indent-rules
      ;; cannot use smie-rule-parent; we use ada-indent-rule-parent
      ;; We do _not_ call smie-indent-virtual.
      (cond
       ((null indent) nil)
       ((eq (car-safe indent) 'column) (cdr indent))
       (t (error "Invalid `ada-indent-rules' result %s" indent)))
      ))

(defun ada-indent-default ()
  "Unconditionally indent as `ada-indent' from the previous
parent keyword. Intended to be the last item in `smie-indent-functions',
used when no indentation decision was made."
  (cdr (ada-indent-rule-parent ada-indent-broken nil)))

;;; debug
(defvar ada-indent-debug-refine t
  "When non-nil, `ada-indent-show-keyword-forward' and
`ada-indent-show-keyword-backward' invalidate cache first, so
they always run the refine algorithm.")

(defun ada-indent-show-keyword-forward ()
  "Show the grammar info for word following point, and move across it."
  (interactive)
  (when ada-indent-debug-refine (ada-indent-invalidate-cache))
  (message "%s" (assoc (ada-indent-forward-token) smie-grammar)))

(defun ada-indent-show-keyword-backward ()
  "Show the grammar info for word preceding point, and move across it."
  (interactive)
  (when ada-indent-debug-refine (ada-indent-invalidate-cache))
  (message "%s" (assoc (ada-indent-backward-token) smie-grammar)))

(defun ada-indent-show-parent ()
  "Move backward to the parent of the word following point, and show its refined keyword and grammar levels."
  (interactive)
  (let* ((token (save-excursion (ada-indent-forward-token)))
	 (count (if (ada-indent-opener-p token) 2 1))
	 (toklevels (ada-indent-goto-parent token count)))
    (message "%s => %s" (assoc token ada-indent-grammar) toklevels)))

(defun ada-indent-show-child ()
  "Move forward to the child of the word following point, and show its refined keyword and grammar levels."
  (interactive)
  (let* ((token (save-excursion (ada-indent-forward-token)))
	 (toklevels (smie-forward-sexp token)))
    (message "%s => %s" (assoc token ada-indent-grammar) toklevels)))

(defun ada-indent-show-statement-start ()
  "Move to the start of the current statement."
  (interactive)
  ;; this is the way ada-indent-goto-statement-start is called during indentation; better for testing
  (let ((token (save-excursion (ada-indent-forward-token))))
    (ada-indent-goto-statement-start (if (equal token "") "(" token))))

(defun ada-indent-wrapper (indent-function)
  "Call INDENT-FUNCTION, check for errors, report non-nil."
  (let ((pos (point))
	(res (funcall indent-function)))

    (when (not (= pos (point)))
      (error "indent-function %s moved point" indent-function))

    (unless (or (null res)
		(integerp res))
      (error "indent-function %s returned invalid value %s" indent-function res))

    (when res
      (message "indent-function %s returned indentation %s" indent-function res)
      res)
    ))

(defmacro ada-indent-wrap (func)
  `(lambda() (ada-indent-wrapper ',func)))

(defun ada-indent-wrap-indent-functions ()
  "Replace contents of `smie-indent-functions' with functions wrapped in `ada-indent-wrapper'.
This lets us know which indentation function succeeded."
  (let (res func)
    (while (setq func (pop smie-indent-functions))
      (let ((newfunc (cadr (macroexpand `(ada-indent-wrap ,func)))))
	(setq res (cons newfunc res))))
    (setq smie-indent-functions (reverse res))))

;;; setup

(defun ada-indent-setup ()

  ;; We don't need most of the functions in the default value for
  ;; smie-indent-functions, so we specify it directly here.
  ;;
  ;; smie-indent-comment lines up comments with following code. That
  ;; means they line up with 'end', which is wrong.
  ;;
  ;; There are times (like at 'end') when it is very simple to figure
  ;; out the indent when looking at a keyword, and much harder when
  ;; looking at the previous keyword, so we do
  ;; ada-indent-before-keyword before ada-indent-after-keyword.
  ;;
  ;; We started out trying to use smie-indent-keyword and
  ;; smie-indent-after-keyword. However, there are too many cases when
  ;; they do the wrong thing for Ada. We keep the same overall
  ;; structure of the code; ada-indent-before-keyword calls
  ;; ada-indent-rules in the same way, except it assumes
  ;; ada-indent-rules returns a column, not an offset.
  ;;
  ;; ada-indent-default works better for Ada code than
  ;; smie-indent-exps.

  (make-local-variable 'smie-indent-functions)

  (setq smie-indent-functions
	'(smie-indent-bob; handle first non-comment line in buffer
	  ada-indent-comment
	  ada-indent-record
	  ada-indent-before-keyword
	  ada-indent-after-keyword
	  ada-indent-default)
	)
  (if debug-on-error (ada-indent-wrap-indent-functions))

  (smie-setup ada-indent-grammar #'ada-indent-rules
	      :forward-token #'ada-indent-forward-token
	      :backward-token #'ada-indent-backward-token)

  (add-hook 'after-change-functions 'ada-indent-after-change))

(add-hook 'ada-mode-hook 'ada-indent-setup)

(define-key ada-mode-map "\t" 'indent-for-tab-command)
;; TAB will now use smie indentation in Ada mode buffers

(provide 'ada-indent)

;;; end of file
