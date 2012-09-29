;;; Ada mode indentation engine, based on SMIE
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

(require 'smie)
(eval-when-compile (require 'cl)); 'case'

;;; user variables

(defcustom ada-indent 3
  "*Size of Ada default indentation, when no other indentation is used.

An example is :
procedure Foo is
begin
>>>null;"
  :type 'integer  :group 'ada)

;;; grammar

(defconst ada-indent-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '(;; non-terminal syntax terms not otherwise expanded
       (identifier)

       ;; BNF from [1] appendix P, vastly simplified
       ;; (info "(aarm2012)Annex P")
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
       ;; at "'", it sees the following grammar values:
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
       ;; Now consider the following declaration:
       ;;
       ;;    type Type_2 (Discriminant_1 : Integer) is tagged null record;
       ;;
       ;; the grammar values in an early version of this grammar were:
       ;;
       ;; (; 23 22)
       ;; (null_record 0 (179))
       ;; nil
       ;; (is-type_tagged 153 (185))
       ;; nil [2 times]
       ;; (type (169) 153)
       ;;
       ;; This chain is broken; 0 from "null_record" does not match
       ;; (185) from "is-type_tagged". So starting from "record",
       ;; `smie-backward-sexp' will find "is" as the parent of
       ;; this declaration, which is wrong.
       ;;
       ;; The two Ada keywords "is" and "tagged" were combined into
       ;; one SMIE keyword in order to avoid conflicts while building
       ;; the grammar. However, this particular choice of conflict
       ;; resolution breaks the purpose of the grammar, so we must
       ;; avoid it. That is the primary work in building the grammar;
       ;; finding ways to avoid conflicts without breaking the ability
       ;; to find parents. One approach is to leave out as many
       ;; keywords as possible; that's how we now handle "tagged".
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
       ;; to either refine both tokens, or leave one out and refine
       ;; the other.
       ;;
       ;; We don't include any tokens after "end" in the grammar, so
       ;; it is always a closer. Since, in "end record", the trailing
       ;; "record" is already a SMIE keyword, we must refine that
       ;; into "end-record record-end".
       ;;
       ;; For all ';' separated non-terminals, the first and last
       ;; tokens in the syntax should be a keyword, so the
       ;; non-terminal is a single sexp when parsing forwards or
       ;; backwards. An exception is when there is a single keyword in
       ;; the syntax; then a trailing name is ok, and can reduce the
       ;; number of refined keywords we need. There are a couple of
       ;; other exceptions, noted below.
       ;;
       ;; There is no need for sexps with only one keyword in the
       ;; grammar; finding the parent of a single keyword is trivial
       ;; :). On the other hand, keeping them, where the keyword is
       ;; already defined for other purposes, does no harm, and is
       ;; less jarring to read.

       ;; We list the non-terminals in alphabetical order, since there
       ;; isn't any more reasonable order. We use the same names as [1]
       ;; Annex P as much as possible.

       (context_clause
	(context_item)
	(context_item ";" context_item))

       (context_item
	("with-context")
	("use"))

       (declaration
	;; FIXME: (package_specification), (package_body); not tested yet.
	(protected_body)
	(type_declaration)
	(subprogram_declaration)
	(subprogram_body)

	;; object declarations
	(identifier ":" name); same as ":" in extended return

	;; FIXME: anonymous array ...
	)

       (declarations
	(declaration)
	(declaration ";" declaration))

       (entry_body
	("entry" identifier "when" expression "is-entry_body" declarations "begin" statements "end"))

       (expression
	;; The expression syntax rules in [1] mostly serve to express
	;; the operator precedence; we do that in the precedence table
	;; below.
	;;
	;; Here "-operator-" is a fake operator that ties this BNF
	;; grammar to that precedence table (stole this idea from the
	;; modula2 smie grammar).
	;;
	;; Note that we declare := to be an operator; that way this
	;; covers assignment statements as well. We are not enforcing
	;; Ada legality rules, just computing indentation. We arrange
	;; the precendence so that ":=" is the parent of an assignment
	;; statement.
	(name "-operator-" name)
	("(" expression ")"))

       ;; Formal generic parameters. Most formal_* are covered in this
       ;; grammar by the equivalent non-formal syntax.
       (formal_subprogram_declaration
	;; leaving "with" "function" "procedure" unrefined gives
	;; conflicts with the non-formal use.

	("with-generic" "function-generic" name "return-spec" name); trailing name same as non-formal
	("with-generic" "procedure-generic" name); trailing name same as non-formal
	)

       (generic_package_declaration
	;; No need to distinguish between 'declarations' and
	;; 'generic_formal_parameter_declaration' for our purposes.
	("generic" declarations
	 "package-generic" identifier "is-package" declarations "private" declarations "end")
	("generic" declarations
	 "package-generic" identifier "is-package" declarations "end"))

       (interface_list
	;; The Ada grammar sometimes has "name and interface_list".
	;; We can't (and don't need to) distinguish that from "interface_list"
	(name)
	(interface_list "and-interface_list" name))

       (name
	(identifier)
	(name "." identifier) ; selected_component
	;; Remember that parenthesis are simply skipped by SMIE
	;; (unless we are indenting inside them; then they are
	;; boundaries). So we don't need to represent subprogram
	;; parameter lists, or array indices here (no aggregates in
	;; 'expression'). FIXME: need to handle "," in aggregates
	)

       (package_specification
	("package" name "is-package" declarations "private" declarations "end")
	("package" name "is-package" declarations "end"))

       (package_body
	;; Leaving 'package body' as separate tokens causes problems
	;; in refine-is, so we leave "body" as an identifier.
	("package-body" name "is-package-body" declarations "begin" statements "end"))

       (protected_body
	("protected-body" identifier "is-protected-body" declarations "end"))

       (statement
	(expression); matches procedure calls, assignment

	("return-stmt")
	;; extended_return_statement
	("return-ext" identifier ":" name); same as initialized object declaration
	("return-ext" identifier ":" name "do" statements "end-return" "return-end")
	;; FIXME: if/then/else, loop, declare/begin/end, ...
	)

       (statements
	(statement)
	(statement ";" statement))

       (subprogram_body
	;; access is an identifier
	("function" name "return-spec" name "is-subprogram_body" declarations "begin" statements "end")
	("procedure" name "is-subprogram_body" declarations "begin" statements "end"))

       (subprogram_declaration
	("function" name "return-spec" name)
	;; trailing name makes "return-spec" have the same binding as
	;; in subprogram_body; that avoids recursion between refine-is
	;; and refine-return

	("procedure" name); same as 'procedure name is-subprogram_body'
	;; We keep this, because it is too jarring to not find it here :).
	;;
	;; We leave out ("procedure" name "is" "null") here; we
	;; are treating a couple of occurences of "is", and most
	;; occurences of "null", as identifiers. See
	;; incomplete_type_declaration below for more.
	)

       (type_declaration
	;; access_type_definition
	("type" identifier "is-type-access")
	;; any occurance of "is-type" as the last keyword in a
	;; declaration must be refined; otherwise it is ambiguous
	;; with several other declarations. See "is-type-tagged" below.

	;; We don't include access-to-subprogram in the grammar,
	;; because we want to identify 'function' and 'procedure' in
	;; these types, so we can indent the parameter list relative
	;; to them. So we allow them to be parents. This also greatly
	;; simplifies refine-is.

	;; array_type_definition; we leave "array" as an identifier
	("type" identifier "is-type" expression "of")

	;; derived_type_declaration
	("type" identifier "is-type" "new" name); same as below
	("type" identifier "is-type" "new" name "with" "private-with")
	("type" identifier "is-type" "new" name "with" "record-null"); "null" is an identifier
	("type" identifier "is-type" "new" name "with" "record" declarations "end-record" "record-end")
	("type" identifier "is-type" "new" interface_list "with" "record" declarations "end-record" "record-end")

	;; enumeration_type_definition
	("type" identifier "is-type-enumeration")
	;; enumeration literals are an aggregate, which is ignored.

	;; {ordinary_ | decimal_} fixed_point_definition
	;; "delta" and "digits" are left as identifiers
	;; floating_point_definition, integer_type_definition; "range" is an identifier
	("type" identifier "is-type-numeric")

	;; incomplete_type_declaration
	;;
	;; The full syntax for incomplete_type_declaration is:
	;;
	;; incomplete_type_declaration ::= type defining_identifier [discriminant_part] [is tagged];
	;;
	;; We don't need the variant without "is tagged", since it has only one keyword.
	;;
	;; If we leave this out entirely, then refine-is will return
	;; "is-type", and since "is-type" matches "end", in this code:
	;;
	;;    type Type_2 (<>) is tagged;
	;;
	;; end Ada_Mode.Nominal;
	;;
	;; the parent of "end" is "type", instead of "package".
	;;
	;; Leaving "is-type" and "tagged" present as separate keywords
	;; causes the "tagged" in all the other types to be seen as a
	;; keyword instead of an identifier, breaking the sexp keyword
	;; chain.
	;;
	;; So we really want to leave out the "tagged"; adding it to
	;; the other sexps is likely to cause conflicts.
	;;
	;; Refining this to "is-type_tagged" breaks the corresponding
	;; complete type declaration:
	;;
	;;    type Type_2 (Discriminant_1 : Integer) is tagged null record;
	;;
	;; since "is-type_tagged" does not match "null record", the parent of
	;; 'record' here is "is tagged".
	;;
	;; The solution is to have refine-is treat this occurence of
	;; "is", and the one in 'procedure name is null', as an
	;; identifier, returning plain "is". All other occurrences of
	;; "is" are refined into one of the "is-*" keywords.

	;; interface_type_definition, formal_interface_type_definition
	("type" identifier "is-type" "interface")
	("type" identifier "is-type" "interface_and" interface_list)

	;; modular_type_definition
	("type" identifier "is-type" "mod-type")
	;; "mod" is an operator, so it has to be in the grammar. We
	;; also want something following "is-type", unless we treat
	;; this occurence of "is" as an identifier. FIXME: On the
	;; other hand, we don't really need "mod" as an operator. It
	;; also occurs in other syntax; wait until we test those to
	;; decide to leave it as an identifier.

	;; private_extension_declaration
	("type" identifier "is-type" "new" name "with" "private-with")
	("type" identifier "is-type" "new" interface_list "with" "private-with")
	;; leaving 'with' and 'private' as separate tokens causes conflicts

	;; private_type_declaration
	("type" identifier "is-type" "private-type")

	;; protected_type_declaration, single_protected_declaration
	;;
	;; We don't need "protected" in the grammar anywhere, so leave
	;; it as an identifier; this simplifies access-to-subprogram
	;; types, since we can just ignore "protected" there.  Note
	;; that in a single_protected_declaration, we are refining
	;; "protected" to "type". However, "is" in protected type
	;; declaration is a block start keyword, while "is-type" in
	;; general is not, so we need to make that a keyword.
	("type" identifier "is-type" declarations "private" declarations "end")
	("type" identifier "is-type-protected" declarations "private" declarations "end")
	("type" identifier "is-type" "new" interface_list "with" declarations
	 "private" declarations "end")

	;; record_type_definition
	("type" identifier "is-type" "record-null")
	("type" identifier "is-type" "record" declarations "end-record" "record-end")
	;; no need to distinguish between 'declarations' and 'component_list'

	); type_declaration
       )); smie-bnf->prec2

    ;; operators and similar things
    (smie-precs->prec2
     '((nonassoc "-operator-")
       ;; We can merge the relational, math, and other operators in
       ;; these levels, because we don't care about legality or
       ;; evaluation order precedence. Nor do we care about associativity.
       ;;
       ;; So we set the precedence hierarchy to help with indentation;
       ;; := and => are the highest level parents, and all other
       ;; operators are at the same level, so we can always find the
       ;; start of an Ada assignment or association with one call to
       ;; ada-indent-goto-parent.
       ;;
       (nonassoc ":=" "=>")
       (nonassoc
	"=" "/=" "<" "<=" ">" ">=" "in"
	"or" "or_else" "xor" "+" "-" "&"
	"and" "and_then" "mod" "rem" "*" "/"
	"abs"; "not" leave "not" as identifier so it is not confused with "not null"
	"'" "." "**" "..")
       ))
    )))

;;; utils for refine-*, forward/backward token

(defconst ada-indent-block-keywords
  '("begin"
    ;; FIXME: declare
    "do"
    ;; "end" is never a block start; treated separately
    "generic"
    "is-entry_body"
    "is-package-body"
    "is-package"
    "is-protected-body"
    "is-subprogram_body"
    "is-type-protected"
    "private"
    "record"
    "when")
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
  '("end"
    "end-record"
    "end-return")
  "Keywords that always end indented blocks.")

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

(defun ada-indent-skip-param_list (next-token direction)
  ;; While refining tokens, we don't want to call smie-next-sexp,
  ;; because it relies on refined tokens. So we call the C scanner
  ;; directly when we need to skip a parenthesis (see the lisp source
  ;; for forward-sexp).
  (let ((forward-sexp-function nil))
    (if (eq direction 'ada-indent-backward-name)
	(backward-sexp)
    (forward-sexp))))

(defun ada-indent-next-name (next-token)
  "Skip over a name using NEXT-TOKEN. Here a 'name' consists of
identifiers, dots, and anything that looks like a parameter
list (could be an array index). Note that Ada 2012 keywords that
don't appear in the grammar look like identifiers, so this also
skips them.  Return the token that isn't part of the name (which
may be found before any name is seen).  Return empty string if
encounter beginning of buffer."
  (let (token)
    (catch 'quit
      (while
	  (progn
	    (setq token (funcall next-token))
	    (if (equal "" token)
		;; We hit a parameter list or something similar
		(progn
		  (when (bobp) (throw 'quit nil))
		  (when (eq (char-before) ?\() (throw 'quit "("))
		  (ada-indent-skip-param_list next-token 'ada-indent-backward-name)
		  (setq token (funcall next-token))))
	    (setq token (nth 0 (assoc token smie-grammar)))
	    (or (not token); not a keyword, so it must be an identifier
		(equal token "."))))
      )
    token))

(defun ada-indent-backward-name ()
  (ada-indent-next-name 'ada-indent-backward-token))

(defun ada-indent-forward-name ()
  (ada-indent-next-name 'ada-indent-forward-token))

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

(defun ada-indent-refine-error (msg)
  ;; When running from the Makefile, we'd like to report the line
  ;; number here, but there is no 'current-line' function. So we show
  ;; the text of the current line.
  (error
   (concat msg ": "
	   (buffer-substring-no-properties
	    (progn (beginning-of-line) (point))
	    (progn (end-of-line) (point))))))

(defun ada-indent-refine-and (forward)
  ;; 'and' occurs in interface types and logical expressions
  ;; (search for interface_list in [1] annex P):
  ;;
  ;; 1) [formal_]derived_type_definition ::=
  ;;    type defining_identifier [discriminant_part] is
  ;;       [abstract] [limited] new parent_subtype_indication
  ;;       [[and interface_list] record_extension_part]
  ;;       [aspect_specification];
  ;;
  ;;    preceding keyword: "new"
  ;;    skip: name
  ;;    keyword: "and-interface_list"
  ;;
  ;; 2) interface_type_definition ::=
  ;;    (type identifier is) [limited | task | protected | synchronized] interface [and interface_list]
  ;;
  ;;    preceding lower level keyword: "interface"
  ;;    skip: nothing
  ;;    keyword: "interface_and"
  ;;
  ;; 3) interface_list ::= interface_subtype_mark {and interface_subtype_mark}
  ;;
  ;;    preceding keyword: "interface_and", "and-interface_list"
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
  ;; All other occurences are logical expressions, returning "and".
  (or
   (and forward
	(equal "interface" (save-excursion (smie-default-backward-token); and
					   (smie-default-backward-token)))
	(smie-default-backward-token)
	"interface_and"); 2

   (and (not forward)
	(equal "interface" (save-excursion (smie-default-backward-token)))
     (smie-default-backward-token)
     "interface_and"); 2

   (save-excursion
     (when forward (smie-default-backward-token))

     (let ((token (ada-indent-backward-name)))
       (cond
	((or (equal token "interface_and"); 3
	     (equal token "and-interface_list"); 3
	     (equal token "new")); 1, 4, 5, 6, 7
	   "and-interface_list")
	(t "and")))))
  )

(defconst ada-indent-type-modifiers '("abstract" "tagged" "limited"))

(defun ada-indent-skip-type-modifiers ()
  "Skip forward tokens that are in `ada-indent-type-modifiers', return the following token."
  (let (result)
    (while (member (setq result (smie-default-forward-token)) ada-indent-type-modifiers))
    result))

(defun ada-indent-refine-is (forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    ;; too many occurences to document them all.

    (or
     ;; First try simple, common constructs.

     (save-excursion
       ;; This is a special case because "protected" not followed
       ;; by "body" is not a keyword, so ada-indent-backward-name
       ;; doesn't find it.
       (let ((token (progn
		      (smie-default-backward-token); identifier
		      (smie-default-backward-token)))) ; "protected", "body", "type"
	 (cond
	  ((equal token "protected") "is-type-protected")
	  ((and
	    (equal token "body")
	    (setq token (smie-default-backward-token))
	    (equal token "protected") "is-protected-body"))
	  ((and
	    (equal token "type")
	    (setq token (smie-default-backward-token))
	    (equal token "protected") "is-type-protected"))
	 )))

     (let ((token (save-excursion (ada-indent-backward-name))))
       (cond
	((equal token "package") "is-package")
	;; "package" name ^ "is"

	((equal token "package") "is-package-body")
	;; "package" "body" name ^ "is"; "body" is an identifier

	((member token '("procedure" "procedure-generic"))
	 ;;  procedure name is abstract;
	 ;;  procedure name is null;
	 ;;  procedure name is declarations begin statements end;
	 (let ((token (save-excursion
			(smie-default-forward-token); is
			(smie-default-forward-token))))
	   (cond
	    ((member token '("abstract" "null")) "is")
	    (t "is-subprogram_body"))))

	((equal token "return-spec")
	 ;; "function" identifier "return" name ^ "is" declarations "begin"
	 "is-subprogram_body")

	((equal token "type")
	 (let ((token
		(progn
		  (smie-default-forward-token); is
		  (smie-default-forward-token))))
	   (cond
	    ((and (equal token "")
		  (looking-at "("))
	     "is-type-enumeration");; type identifier is (...)

	    ((member token '("not" "access")) "is-type-access")

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

	    ((equal token "tagged")
	     (if (equal ";" (smie-default-forward-token))
		 ;; type Incomplete_Type_1 (Discriminant_1 : Integer) is tagged; -- in spec
		 ;; type Incomplete_Type_1 (Discriminant_1 : Integer) is tagged null record; -- in body
		 ;; type name is tagged ...; other types
		 "is"; an identifier
	       "is-type"))

	    (t "is-type")); all others
	   ))))

     ;; now more complicated things
     (save-excursion
       ;; entry body with params: "entry" identifier "("...")" "when" exp "is"
       ;;
       ;; If we can be guessing wrong here, we can't use
       ;; smie-backward-sexp (because it will just get confused). So
       ;; far, this is the only possibility at this point, so we don't
       ;; really need to check, but we want to identify missing cases.
       (if (equal "entry" (nth 2 (smie-backward-sexp "is-entry_body"))) "is-entry_body"))

     (ada-indent-refine-error "unrecognized 'is'")
     ))
  )

(defun ada-indent-refine-mod (forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    ;; "mod" occurs in:
    ;;
    ;; 1) modular_type_definition as part of a full_type_declaration ::=
    ;;
    ;;    type identifier [known_discriminant_part] is mod expression;
    ;;
    ;;    preceding keyword: "is-type"
    ;;    skip: nothing
    ;;    keyword: "mod-type"
    ;;
    ;; 2) multiplying_operator ::= * | / | mod | rem
    ;;
    ;;    preceding keyword: none, this must be the default.
    ;;    keyword: "mod"
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
      "mod")))

(defun ada-indent-refine-private (forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    ;; 'private' occurs in:
    ;;
    ;; 1) [formal_]private_type_declaration
    ;;
    ;;   type defining_identifier [discriminant_part] is [[abstract] tagged] [limited] private
    ;;      [with aspect_mark];
    ;;
    ;;   preceding token: "is-type"
    ;;      but that is recursive with forward-token refining "is"
    ;;   following token: "with" or ";"
    ;;   skip: nothing
    ;;   token: private-type
    ;;
    ;; 2) [non]limited_with_clause
    ;; 3) formal_derived_type_definition
    ;; 4) library_item
    ;; 5) package_specification
    ;; 6) private_extension_declaration
    ;;
    ;;    type defining_identifier [discriminant_part] is
    ;;       [abstract] [limited | synchronized] new ancestor_subtype_indication
    ;;       [and interface_list] with private
    ;;       [aspect_specification];
    ;;
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
      "private-type"); 1

     (t "private"))); all others
  )

(defun ada-indent-refine-return (forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    ;; 'return' occurs in:
    ;;
    ;; 1) a function subprogram_declaration or formal_subprogram_declaration:
    ;;
    ;;    function_specification ::= function defining_designator parameter_and_result_profile
    ;;
    ;;    preceding token: "function", "function-generic"
    ;;    token: "return-spec"
    ;;
    ;; 2) a function specification in function body :
    ;;
    ;;    function identifier (...) return [access] name is
    ;;
    ;;    preceding token: "function"
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
    ;;    preceding token: "function"
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
     (if (member (save-excursion (ada-indent-backward-name))
		 '("function" "function-generic"))
	 "return-spec"); 1, 2, 5

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

(defun ada-indent-refine-subprogram (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    (if (equal "with" (smie-default-backward-token))
	(concat token "-generic")
      token)
  ))

(defun ada-indent-refine-with (forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    ;; "with" occurs in:
    ;;
    ;; 1) record_extension_part in a [formal_]derived_type_definition in a type_declaration
    ;;
    ;;    type ... is ... new parent_subtype_indication [[and interface_list] with record_definition]
    ;;
    ;;    preceding keyword: "new", "and-interface_list"
    ;;    skip: name
    ;;    keyword: "with"
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
    ;;    preceding keyword: "new", "and-interface_list"
    ;;    succeeding keyword: "private"
    ;;    skip: name
    ;;    keyword: "with"
    ;;
    ;; 4) task_type_declaration, single_task_declaration,
    ;;    protected_type_declaration, single_protected_declaration ::=
    ;;       {protected | task} [type] defining_identifier [known_discriminant_part]
    ;;       [aspect_specification] [is
    ;;       [new interface_list with]
    ;;       {protected | task}_definition];
    ;;
    ;;    preceding keyword: "new", "and-interface_list"
    ;;    skip: name
    ;;    keyword: "with"
    ;;
    ;; 5) requeue_statement ::= requeue procedure_or_entry_name [with abort];
    ;;
    ;;    not implemented yet
    ;;
    ;; 6) [non]limited_with_clause in a context_clause ::=
    ;;
    ;;    [limited] [private] with library_unit_name {, library_unit_name};
    ;;
    ;;    previous token: ["limited"] ["private"] ";" bob
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
    ;;    following token: "function", "procedure"
    ;;    skip: none
    ;;    keyword: with-generic
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
    ;;    not implemented yet
    ;;    followed by "package"
    ;;
    ;; 11) aspect_specification ::=
    ;;       with aspect_mark [=> aspect_definition] {, aspect_mark [=> aspect_definition] }
    ;;
    ;;    not implemented yet
    ;;    followed by "=>", ",", ";"
    ;;
    (or
     (let ((token (save-excursion
		    (smie-default-forward-token); with
		     (smie-default-forward-token))))
       (if (member token '("procedure" "function"))
	   "with-generic"); 8
       )

     ;; this checks for preceding ";", so it has to be after the above
     (let ((token (save-excursion (smie-default-backward-token))))
       (if (or
	    (equal token ""); bob
	    (member token '("limited" "private" ";")))
	   "with-context"); 6
       )

     "with")
    ))

;;; forward/backward token

(defun ada-indent-forward-token ()
  ;; This must be a complete inverse of ada-indent-backward-token;
  ;; smie-indent-keyword parses forward one token, potentially at
  ;; every keyword
  (let ((token (smie-default-forward-token)))
    (cond
     ;; Alphabetical order.

     ((equal token "<>;")
      ;; These three chars all have punctuation syntax. We need that
      ;; for expressions, so we don't change it. "<>" is an identifier
      ;; in the grammar, so we can just refine this to ";"
      ";")

     ((equal token "and") (ada-indent-refine-and t))

     ((equal token "end")
      (let ((token (save-excursion (smie-default-forward-token))))
	(cond
	 ((equal "record" token)
	  "end-record")

	 ((equal "return" token)
	  "end-return")

	 (t "end")); all others
	))

     ((equal token "function") (ada-indent-refine-subprogram token t))

     ((equal token "is") (ada-indent-refine-is t))

     ((equal token "mod") (ada-indent-refine-mod t))

     ((equal token "package")
      (if (save-excursion (equal "access" (smie-default-backward-token)))
	  "package-access"
	(if (equal "body" (save-excursion (smie-default-forward-token)))
	      "package-body"
	  ;; FIXME: package-generic?
	  "package")))

     ((equal token "private") (ada-indent-refine-private t))

     ((equal token "procedure") (ada-indent-refine-subprogram token t))

     ((equal token "protected")
      (if (equal "body" (save-excursion (smie-default-forward-token)))
	    "protected-body"
	;; "body" is an identifier. We don't check forward for "type",
	;; because we are leaving "protected" as an identifier.
	"protected"))

     ((equal token "record")
      (let ((token (save-excursion (smie-default-backward-token) (smie-default-backward-token))))
	(cond
	 ((equal "end" token)
	  "record-end")

	 (t "record")); all others
	))

     ((equal token "return") (ada-indent-refine-return t))

     ((equal token "with") (ada-indent-refine-with t))

     (t token))))

(defun ada-indent-backward-token ()
  (let ((token (smie-default-backward-token))
	pos)
    (cond
     ;; Alphabetical order.

     ((equal token "<>;")
      ;; These three chars all have punctuation syntax. We need that
      ;; for expressions, so we don't change it. "<>" is an identifier
      ;; in the grammar, so we can just refine this to ";"
      ";")

     ((equal token "and") (ada-indent-refine-and nil))

     ((equal token "end")
      (let ((token (save-excursion
		     (smie-default-forward-token); "end"
		     (smie-default-forward-token))))
	(cond
	 ((equal token "record") "end-record")
	 ((equal token "return") "end-return")
	 (t "end"))))

     ((equal token "function") (ada-indent-refine-subprogram token nil))

     ((equal token "is") (ada-indent-refine-is nil))

     ((equal token "mod") (ada-indent-refine-mod nil))

     ((equal token "package")
      ;; FIXME: this is ok for a library level [generic] package alone
      ;; in a file. But it could be a problem for a nested [generic]
      ;; package. Idea: "end" can't occur in generic formal
      ;; parameters; search for "end|generic".
      (if (equal "generic" (save-excursion (smie-backward-sexp "package-generic")))
	  "package-generic"
	"package"))

     ((equal token "private") (ada-indent-refine-private nil))

     ((equal token "procedure") (ada-indent-refine-subprogram token nil))

     ((equal token "protected")
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
      (let ((token (save-excursion (smie-default-forward-token))))
	(cond
	 ((equal token "body") "protected-body")
	 ((equal token "type") "protected"); an identifier
	 (t "protected"))
	))

     ((equal token "record")
      (let ((token (save-excursion (smie-default-backward-token))))
	(cond
	 ((equal token "end") "record-end")
	 ((equal token "null") "record-null")
	 (t "record")
	 )))

     ((equal token "return") (ada-indent-refine-return nil))

     ((equal token "with") (ada-indent-refine-with nil))

     (t token))))

;;; indent rules

(defun ada-indent-rule-current (offset)
  "Indent relative to the current line"
  (cons 'column (+ (save-excursion (back-to-indentation) (current-column)) offset)))

(defun ada-indent-keyword-p (token)
  (assoc token ada-indent-grammar))

(defun ada-indent-openerp (token)
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
	      ;; is has a higher precedence level, it leaves point on
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
    ;; has a higher precedence level, point is on the lower precedence
    ;; token; (nth 1 token) gives the position at the start of the
    ;; higher precendence token.
    ;;
    ;; See cases in ada-indent-rule parent. Since what to do depends
    ;; on what we are indenting, we return token, leaving point alone,
    ;; and let the caller sort it out.

    token))

(defun ada-indent-rule-parent (offset child)
  "Find the parent of CHILD (using `ada-indent-goto-parent'),
return an indent by OFFSET relevant to it. Preserves point.  If
CHILD must be non-nil and a keyword or \"(\", and point must be
at the start of CHILD."
  (save-excursion
    (ada-indent-goto-parent child (or (if (equal child "(") 2) 1))
    (back-to-indentation)
    (cons 'column (+ (current-column) offset))
    ))

(defun ada-indent-goto-statement-start (&optional child)
  "Move point to the start of the statement/declaration
containing point. If point is at statement start, does
nothing. If CHILD is non-nil, it must be a keyword or \"(\",
and point must be at the start of CHILD."
  (interactive)
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
  (let (parent pos)

    (catch 'done

      ;; We have to check the previous keyword for procedure calls,
      ;; "procedure" and "function", and a few other cases.  Many
      ;; statements have non-keyword tokens before the first token
      ;; (assignent, subprogram declarations including "overriding"),
      ;; so we have to use use ada-indent-backward-name to find the
      ;; previous keyword.
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
      (if (and pos (not (= pos (point)))) (progn (goto-char pos) (throw 'done nil)))

      (setq parent (ada-indent-goto-parent child (or (if (member child '("." "(")) 2) 1)))
      ;; If child is "." we are in the middle of an Ada name; the
      ;; first parent is the start of the name.
      ;;
      ;; If child is "(" the first run thru smie-backward-sexp produces no motion; that causes the second run

      (if (equal (nth 2 parent) "(") (throw 'done nil))

      (if (or
	   (equal (nth 2 parent) ";")
	   (and
	    (not (member child ada-indent-block-keywords))
	    (not (member child ada-indent-block-end-keywords))
	    (member (nth 2 parent) ada-indent-block-keywords)))
	  ;; point is at statement start
	  (throw 'done nil))

      (goto-char (nth 1 parent))

      ;; handle access-to-subprogram types
      (while (and (member (nth 2 parent) `("procedure" "function"))
		  (member (save-excursion (smie-default-backward-token)) '("access" "protected")))
	(setq parent (ada-indent-goto-parent parent 2)))

      (if (member (nth 2 parent) '(":" ":="))
	  (progn
	    (ada-indent-backward-name)
	    ;; we are now on the keyword before statement start; get back.
	    (smie-default-forward-token); ";" or block start
	    (forward-comment (point-max)))
	)

      (back-to-indentation)
      )))

(defun ada-indent-rule-statement (offset child)
  "Find the start of the statement/declaration containing point (using
`ada-indent-goto-statement-start'), return an indent by OFFSET relevant
to it. Preserves point.  If CHILD is non-nil, point must be at
the start of CHILD, which must be a keyword."

  (save-excursion
    (ada-indent-goto-statement-start child)

    (cons 'column (+ (current-column) offset))))

;;;
(defun ada-indent-rules (method arg)
  ;; If this returns a number, smie-indent--rule will do some
  ;; complicated logic with it. So it is best to return ('column n)
  ;; when we can.
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
      ;; we could check for a list of keywords that are known to be
      ;; always at the start of a statement, but that would be
      ;; premature optimization.

      ((equal arg "(")
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
       ;; "function", not the type declaration start.
       (ada-indent-rule-parent ada-indent arg))

      ((equal arg "with-context")
       (cons 'column 0))

      ((or (member arg ada-indent-block-end-keywords )
	   (member arg ada-indent-block-keywords))
       ;; Example:
       ;;
       ;;    procedure
       ;;       (Param_1 : Integer)
       ;;    is
       ;;    begin
       ;;    end;
       ;;
       ;;    entry E2
       ;;       (X : Integer)
       ;;    when Local_1 = 0 and not
       ;;       (Local_2 = 1)
       ;;    is
       ;;       Tmp : Integer := 0;
       ;;    begin
       ;;       Local_2 := Tmp;
       ;;    end E2;
       ;;
       ;; We are indenting "is", "begin", etc. Indent at the same level as the
       ;; parent.
       (ada-indent-rule-statement 0 arg))

      ((let ((pos (progn (save-excursion (ada-indent-goto-statement-start arg)) (point))))
	 (if (not (= pos (point)))
	     ;; Hanging; we are not at the start of a statement/declaration.
	     ;; Indent relative to the statement start.
	     (cons 'column (+ pos offset)))))
      ))

;;; :after
    (:after
     ;; `arg' is a keyword at the end of a line, point is at start of
     ;; the keyword; we are indenting the following token, which may
     ;; not be a keyword.  :before is checked first, so we don't need
     ;; to consider those cases here.

     (cond
      ((equal arg ";")
       (ada-indent-rule-statement 0 arg))

      (t (ada-indent-rule-statement ada-indent arg))
      ))
    ))

;;; smie-indent-functions
;;
;; each must not move point, and must return a column (as an integer) or nil.

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
	 (if debug-on-error
	     (or
	      (ada-indent-wrapper 'smie-indent-bob)
	      (ada-indent-wrapper 'ada-indent-after-keyword)
	      )
	   (or
	    (smie-indent-bob)
	    (ada-indent-after-keyword)
	    )))
       ))

(defun ada-indent-before-keyword()
  "Replacement for `smie-indent-keyword', tailored to Ada.
It requires `ada-indent-rule' to return nil or ('column column),
never just an offset (since we would not know what the offset was
relative to)."
  (let*
      ((token (save-excursion (ada-indent-forward-token)))
       (indent
	(or
	 (and
	  (equal token "")
	  (ada-indent-rules :before "("))

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
	;; we don't check for paren here (we may need to at some point)
	(and
	 (ada-indent-keyword-p token)
	 (ada-indent-rules :after token))))

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
  (cdr (ada-indent-rule-parent ada-indent nil)))

;;; debug
(defun ada-indent-show-keyword-forward ()
  "Show the grammar info for word following point, and move across it."
  (interactive)
  (message "%s" (assoc (ada-indent-forward-token) smie-grammar)))

(defun ada-indent-show-keyword-backward ()
  "Show the grammar info for word preceding point, and move across it."
  (interactive)
  (message "%s" (assoc (ada-indent-backward-token) smie-grammar)))

(defun ada-indent-show-parent (count)
  "Move to the parent of the word following point, and show its refined keyword and grammar levels."
  (interactive "p")
  (when (= count 0) (setq count 't))
  (let ((toklevels (ada-indent-goto-parent (save-excursion (ada-indent-forward-token)) count)))
    (message "%s; %s" toklevels (assoc (save-excursion (ada-indent-forward-token)) ada-indent-grammar))))

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
	  ada-indent-before-keyword
	  ada-indent-after-keyword
	  ada-indent-default)
	)
  (if debug-on-error (ada-indent-wrap-indent-functions))

  (smie-setup ada-indent-grammar #'ada-indent-rules
	      :forward-token #'ada-indent-forward-token
	      :backward-token #'ada-indent-backward-token))

(add-hook 'ada-mode-hook 'ada-indent-setup)

(define-key ada-mode-map "\t" 'indent-for-tab-command)
;; TAB will now use smie indentation in Ada mode buffers

(provide 'ada-indent)

;;; end of file
