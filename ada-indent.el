;;; Ada mode indentation engine, based on SMIE
;;
;; [1] ISO/IEC 8652:201z (draft 18); Ada 2012 reference manual
;;
;; Copyright (C) 2012  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
;; Keywords: languages ada

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; History: see ada_mode.el

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
       ;; "record" is already a SMIE keyword, we must combine that
       ;; into one SMIE keyword "end_record".
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
	(identifier ":") ; object_declaration FIXME: anonymous array ...
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

       (generic_package_declaration
	;; no need to distinguish between 'declarations' and
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
	("package_body" name "is-package_body" declarations "begin" statements "end"))

       (protected_body
	("protected-body" identifier "is-protected-body" declarations "end"))

       (statement
	(expression); matches procedure calls, assignment

	;; extended_return_statement
	("return")
	("return-exp"); FIXME: don't need this
	("return-do" identifier ":")
	("return-do" identifier ":-do" name "do" statements "end_return")
	;; FIXME: if/then/else, loop, declare/begin/end, ...
	)

       (statements
	(statement)
	(statement ";" statement))

       (subprogram_body
	;; factoring out subprogram_specification here breaks something.
	("function" name "return-spec" name "is-subprogram_body" declarations "begin" statements "end")
	("function" name "return_access" name "is-subprogram_body" declarations "begin" statements "end")
	("procedure" name "is-subprogram_body" declarations "begin" statements "end"))

       (subprogram_declaration
	;; factoring out subprogram_specification here breaks something.

	("function" name "return-spec" name)
	("function" name "return+access" name)
	;; second case is returning an anonymous access type
	;; (return_access is too hard to distinguish from
	;; return-access)
	;;
	;; trailing name makes these tokens the same as same as
	;; 'function name return* name is-subprogram-body'; that
	;; avoids recursion between refine-is and refine-return

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

	;; We don't include access-to-subprogram in the grammar,
	;; because we want to identify 'function' and 'procedure' in
	;; these types, so we can indent the parameter list relative
	;; to them. So we allow them to be parents. This also greatly
	;; simplifies refine-is.

	;; array_type_definition; we leave "array" as an identifier
	("type" identifier "is-type" expression "of")

	;; derived_type_declaration
	("type" identifier "is-type" "new" name); same as below
	("type" identifier "is-type" "new" name)
	("type" identifier "is-type" "new" name "with_private")
	("type" identifier "is-type" "new" name "with_null_record")
	("type" identifier "is-type" "new" name "with" "record" declarations "end_record")
	("type" identifier "is-type" "new" interface_list "with" "record" declarations "end_record")

	;; enumeration_type_definition
	("type" identifier "is-enumeration_type");; FIXME: why 'enumeration' in 'is-type'?
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
	("type" identifier "is-type" "new" name "with_private")
	("type" identifier "is-type" "new" interface_list "with_private")
	;; leaving 'with' and 'private' as separate tokens causes conflicts

	;; private_type_declaration
	("type" identifier "is-type" "private-type")

	;; protected_type_declaration, single_protected_declaration
	;;
	;; We don't need "protected" in the grammar anywhere, so leave
	;; it as an identifier; this simplifies access-to-subprogram
	;; types, since we can just ignore "protected" there.  Note
	;; that in a single_protected_declaration, we are refining
	;; "protected" to "type".
	("type" identifier "is-type" declarations "private" declarations "end")
	("type" identifier "is-type" "new" interface_list "with" declarations
	 "private" declarations "end")

	;; record_type_definition
	("type" identifier "is-type" "null_record")
	("type" identifier "is-type" "record" declarations "end_record")
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

(defconst ada-indent-block-start-keywords
  '(
    ;; FIXME: begin?
    "is-entry_body"
    "is-package_body"
    "is-package"
    "is-protected-body"
    "is-subprogram_body"
    "private"
    "record")
  ;; This is not a subset of the open portion of the grammar
  ;; open/close list; that is restricted to keywords that don't bind
  ;; on the left.
  ;;
  ;; We don't need "type" here because the syntax is 'type name
  ;; (discriminants) is', so the indentation is handled by the
  ;; argument list logic.
  "Keywords that start indented blocks.")

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

(defconst ada-indent-block-end-keywords
  ;; We don't compute this list using ada-indent-matching-end, because
  ;; that would add all the bogus keywords, which would confuse the
  ;; indentation logic, since that assumes legal Ada. We want only the
  ;; keywords that actually end indented blocks. Fortunately, it's a
  ;; short list.
  '("end"
    "begin"
    "end_record"
    "private")
  "List of keywords that end indented blocks.")

;; FIXME: compile-time check that
;; block-start-keywords/block-end-keywords are inverse of each other.

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
;; refine-* must not move point, unless it is returning a
;; combined token. FIXME: deleting all such tokens.
;;
;; So each refine defun must take a 'forward' arg, and in general
;; start with:
;;
;; (save-excursion
;;  (when forward (smie-default-backward-token))
;;
;; This makes it difficult to move point for a combined token; see
;; ada-indent-refine-and and ada-indent-refine-private for an examples
;; of how to do this.
;;
;; If at all possible, these functions should not use
;; smie-backward-sexp, especially if the starting token is a guess,
;; because it is often too greedy; it leads to horrible recursive
;; parsing with wrong guesses, and ends up reporting no match. It also
;; relies on refined tokens, which can lead to recursion.
;;
;; We also try to avoid parsing forward, since that won't work while
;; the user is parsing code. But sometimes it can't be helped.

(defun ada-indent-refine-error (msg)
  ;; When running from the Makefile, we'd like to report the line
  ;; number here, but there is no 'current-line' function. So we show
  ;; the text of the current line.
  (error
   (concat msg ": "
	   (buffer-substring-no-properties
	    (progn (beginning-of-line) (point))
	    (progn (end-of-line) (point))))))

(defun ada-indent-refine-: (forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    ;; ':' occurs in object declarations and extended return statements:
    ;;
    ;; defining_identifier_list : [aliased] [constant] subtype_indication [:= expression] [aspect_specification];
    ;;
    ;; defining_identifier : [aliased][constant] return_subtype_indication [:= expression]
    ;;    [do handled_sequence_of_statements end return];
    ;;
    ;; This is too complex to sort out syntacticly. But 'do' and ';'
    ;; are unique, so we can use search-forward-regexp. We might find
    ;; neither, if the user is typing new code at the end of the
    ;; buffer.
    ;;
    ;; We have to allow for newline, which search-forward-regexp does
    ;; not. So first we search for just ';', since there must be
    ;; one. Then we use that as the bound to search for 'do'.
    (let ((bound (save-excursion (search-forward ";" nil t))))
      (if (and bound
	       (search-forward "do" bound t))
	  ":-do"
	":"))))

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
  (let((skip nil)
       ;; skip = number of tokens after 'is' to skip with smie-default-*-token before returning
       res)
    (setq
     res
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
			 (smie-default-backward-token)))) ; "protected" or "body"
	    (cond
	      ((equal token "protected") "is-type")
	      ((and
		(equal token "body")
		(setq token (smie-default-backward-token))
		(equal token "protected") "is-protected-body")))
	    ))

	(let ((token (save-excursion (ada-indent-backward-name))))
	  (cond
	   ((equal token "package") "is-package")
	   ;; "package" name ^ "is"

	   ((equal token "package") "is-package_body")
	   ;; "package" "body" name ^ "is"; "body" is an identifier

	   ((equal token "procedure")
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
		"is-enumeration_type");; type identifier is (...)

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
	)))
    (if skip
	(if forward
	    (dotimes (count skip)
	      (smie-default-forward-token))
	  ;; moving backwards we are in the right place
	  nil
	  ))
    res))

(defun ada-indent-maybe-refine-is (start-token forward)
  "point is in front of START-TOKEN; we are refining that. If
START-TOKEN is part of an is-* keyword, return that keyword,
moving point to its start."
  (cond
   ((equal start-token "is")
    (ada-indent-refine-is forward))
   ;; FIXME: inline this
   ))

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
  (let ((skip nil)
	res)
    (setq
     res
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
       ;;    token: with_private
       ;;
       ;; 7) protected_definition
       ;; 8) task_definition
       ;;
       (cond
	((equal "with" (save-excursion (smie-default-backward-token)))
	 (setq skip t)
	 "with_private"); 5

	((member (save-excursion
		   (smie-default-forward-token); private
		   (smie-default-forward-token))
		 '("with" ";"))
	 "private-type"); 1

	(t "private"))); all others
     )
    (if skip
	;; with_private
	(if forward
	    ;; moving forwards we are in the right place
	    nil
	  (smie-default-backward-token)
	  ))
    res))

(defun ada-indent-refine-return (forward)
  (let ((skip nil)
	res)
    (setq
     res
     (save-excursion
       (when forward (smie-default-backward-token))

       ;; 'return' occurs in:
       ;;
       ;; 1) a function declaration:
       ;;
       ;;    subprogram_declaration ::= [overriding_indicator] subprogram_specification [aspect_specification];
       ;;    function_specification ::= function defining_designator parameter_and_result_profile
       ;;    parameter_and_result_profile ::=
       ;;        [formal_part] return [null_exclusion] subtype_mark
       ;;      | [formal_part] return access_definition
       ;;
       ;;    access_definition ::=
       ;;        [null_exclusion] access [constant] subtype_mark
       ;;      | [null_exclusion] access [protected] procedure parameter_profile
       ;;      | [null_exclusion] access [protected] function parameter_and_result_profile
       ;;
       ;;    preceding token: "function"
       ;;    token: 1a) "return-spec" or 1b) "return+access"
       ;;
       ;;    FIXME: trailing constant, protected not implemented
       ;;
       ;; 2) a function specification in function body :
       ;;
       ;;    function identifier (...) return [access] name is
       ;;
       ;;    preceding token: "function"
       ;;    token: "return-spec" or "return+access"
       ;;
       ;; 3) a return statement:
       ;;
       ;;    return [exp];
       ;;
       ;;    token: "return" or "return-exp"
       ;;
       ;; 4) an extended return statement:
       ;;
       ;;    return identifier : name;
       ;;
       ;;    token: "return" (4a)
       ;;
       ;;    return identifier : name do statements end return;
       ;;
       ;;    token: "return-do" (4b), "end_return" (4c)
       ;;
       ;; 5) an access function type declaration:
       ;;
       ;;    type name is access [protected] function identifier (...) return [access] name;
       ;;
       ;;    preceding token: "is-type-access"
       ;;    token: 5a) "return-access" or 5b) "return+access"
       ;;
       ;; So we have to look both forward and backward to resolve this.
       (or
	(save-excursion (if (equal "end" (smie-default-backward-token)) "end_return")); 4c

	;; Do this now, otherwise we can't distinguish between:
	;;
	;; function F1 return Integer;
	;; return 0;
	;;
	(let ((token (save-excursion (ada-indent-backward-name))))
	  (cond
	   ((equal token "is-type-access")
	    (if (equal "access"
		       (save-excursion
			 (smie-default-forward-token); return
			 (smie-default-forward-token)))
		(progn
		  (setq skip t)
		  "return+access"); 5b
	      "return-access")); 5a

	   ((equal token "function")
	    (if (equal "access"
		       (save-excursion
			 (smie-default-forward-token); return
			 (smie-default-forward-token)))
		(progn
		  (setq skip t)
		  "return+access"); 1b or 2b
	      "return-spec")); 1a or 2a
	   ))

	(save-excursion
	  ;; FIXME: test this at end of buffer (not very
	  ;; likely to happen, but possible while entering code)
	  (if (equal ";"
		     (progn
		       (smie-default-forward-token); return
		       (smie-default-forward-token)))
	      "return"; 3a
	    (let (token (smie-default-forward-token))
	      (cond
	       ((equal token ";") "return-exp") ; special case of 3b with expression = identifier or literal
	       ((equal token ":") "return"); 4a
	       ((equal token ":-do") "return-do"); 4b
	       (t "return-exp"); 3b
	       ))))
	)))
    (if skip
	(if forward
	    (smie-default-forward-token)
	  ;; moving backwards we are in the right place
	  ))
    res))

(defun ada-indent-refine-with (forward)
  (let ((skip nil)
	res)
    (setq
     res
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
       ;;    keyword: "with_private"
       ;;
       ;;    also handled by "private" in ada-indent-backward-token.
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
       ;;
       ;; 5) requeue_statement ::= requeue procedure_or_entry_name [with abort];
       ;;
       ;;    not implemented yet; will be parsed as "with_abort"
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
       ;;    not implemented yet
       ;;    followed by "function", "procedure"
       ;;
       ;; 10) formal_abstract_subprogram_declaration ::=
       ;;       with subprogram_specification is abstract [subprogram_default]
       ;;       [aspect_specification];
       ;;
       ;;    not implemented yet, same as formal_concrete
       ;;
       ;; 11) formal_package_declaration ::=
       ;;       with package defining_identifier is new generic_package_name  formal_package_actual_part
       ;;       [aspect_specification];
       ;;
       ;;    not implemented yet
       ;;    followed by "package"
       ;;
       ;; 12) aspect_specification ::=
       ;;       with aspect_mark [=> aspect_definition] {, aspect_mark [=> aspect_definition] }
       ;;
       ;;    not implemented yet
       ;;    followed by "=>", ",", ";"
       ;;
       (or
	(let ((token (save-excursion (smie-default-backward-token))))
	  (if (or
	       (equal token ""); bob
	       (member token '("limited" "private" ";")))
	      "with-context"))

	(let ((token (save-excursion
		       (smie-default-forward-token); with
		       (smie-default-forward-token))))
	  (cond
	   ((equal token "null" )
	    (setq skip 2)
	    "with_null_record")

	   ((equal token "private" ) (setq skip 1) "with_private")

	   (t "with"))))))
    (if skip
	(if forward
	    (smie-default-forward-token)
	  ;; moving backwards we are in the right place
	  ))
    res))

;;; forward/backward token

(defun ada-indent-forward-token ()
  ;; This must be a complete inverse of ada-indent-backward-token;
  ;; smie-indent-keyword parses forward one token, potentially at
  ;; every keyword
  (let ((token (smie-default-forward-token)))
    (cond
     ;; Alphabetical order, except ada-indent-maybe-refine-is is last
     ;; because it's relatively slow (and it really confuses debugging
     ;; the other rules!), and some checks must be done after that.
     ((equal token ":") (ada-indent-refine-: t))

     ((equal token "and") (ada-indent-refine-and t))

     ((equal token "end")
      (let ((token (save-excursion (smie-default-forward-token))))
	(cond
	 ((equal "record" token)
	  (smie-default-forward-token)
	  "end_record")

	 ((equal "return" token)
	  (smie-default-forward-token)
	  "end_return")

	 (t "end")); all others
	))

     ; "is" handled by 'maybe-refine-is' below

     ((equal token "mod") (ada-indent-refine-mod t))

     ((equal token "null")
      (let ((token (save-excursion
		     (smie-default-backward-token); null
		      (smie-default-backward-token))))
	(cond
	 ((equal token "with")
	  (smie-default-forward-token)
	  (smie-default-forward-token)
	  "with_null_record")

	 ((equal "record" (save-excursion (smie-default-forward-token)))
	  (smie-default-forward-token)
	  "null_record")

	 (t "null")
	 )))

     ((equal token "package")
      (if (save-excursion (equal "access" (smie-default-backward-token)))
	  "package-access"
	(if (equal "body" (save-excursion (smie-default-forward-token)))
	      "package_body"
	  ;; FIXME: package-generic?
	  "package")))

     ((equal token "private") (ada-indent-refine-private t))

     ((equal token "protected")
      (if (equal "body" (save-excursion (smie-default-forward-token)))
	    "protected-body"
	;; "body" is an identifier. We don't check forward for "type",
	;; because we are leaving "protected" as an identifier.
	"protected"))

     ((equal token "return") (ada-indent-refine-return t))

     ((equal token "with") (ada-indent-refine-with t))

     ((ada-indent-maybe-refine-is token t))

     (t token))))

(defun ada-indent-backward-token ()
  (let ((token (smie-default-backward-token))
	pos)
    (cond
     ;; Alphabetical order, except ada-indent-maybe-refine-is is near
     ;; last because it's relatively slow, and it relies on some
     ;; tokens that can follow is being already checked. (and it
     ;; really confuses debugging the other rules!). Some other checks
     ;; must be done after maybe-refine-is.

     ((equal token ":") (ada-indent-refine-: nil))

     ((equal token "and") (ada-indent-refine-and nil))

     ;; "function", "is" handled by maybe-refine-is below

     ((equal token "mod") (ada-indent-refine-mod nil))

     ((equal token "not")
      (if (equal "null" (save-excursion (smie-default-forward-token)))
	(progn
	  (smie-default-forward-token)
	  "not_null")
	"not"))

     ((equal token "null")
      (let ((token (save-excursion (smie-default-backward-token))))
	(cond
	 ((equal token "not")
	  (smie-default-backward-token); not
	  "not_null")

	 ((equal token "with")
	  (smie-default-backward-token); with
	  "with_null_record")

	  (t "null"))))

     ((equal token "package")
      ;; FIXME: this is ok for a library level [generic] package alone
      ;; in a file. But it could be a problem for a nested [generic]
      ;; package. Idea: "end" can't occur in generic formal
      ;; parameters; search for "end|generic".
      (if (equal "generic" (save-excursion (smie-backward-sexp "package-generic")))
	  "package-generic"
	"package"))

     ((equal token "private") (ada-indent-refine-private nil))

     ((and
       (equal token "protected")
       (equal "body" (save-excursion
		       (smie-default-forward-token); "protected"
		       (smie-default-forward-token))))
      "protected-body")

     ;; "procedure" handled by maybe-refine-is below

     ((equal token "record")
      (let ((token (save-excursion (smie-default-backward-token))))
	(cond
	 ((equal token "end") (smie-default-backward-token) "end_record")
	 ((equal token "null")
	  (smie-default-backward-token)
	  (if (equal "with" (save-excursion (smie-default-backward-token)))
	      (progn
		(smie-default-backward-token); with
		"with_null_record")
	    "null_record"))
	 (t "record")
	 )))

     ((equal token "return") (ada-indent-refine-return nil))

     ((equal token "with") (ada-indent-refine-with nil))

     ((ada-indent-maybe-refine-is token nil))

     ((equal token "access")
      (let ((token (save-excursion (smie-default-backward-token))))
	(cond
	 ((equal token "return")
	  (smie-default-backward-token)
	  (ada-indent-refine-return nil))

	 (t "access"))))

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
	;; FIXME: need to detect a single_protected_declaration, without being infinite recursive with refine-is.
	))

     (t token))))

;;; indent rules

(defun ada-indent-rule-current (offset)
  "Indent relative to the current line"
  (cons 'column (+ (save-excursion (back-to-indentation) (current-column)) offset)))

(defun ada-indent-keywordp (token)
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
Point is left on the start of the result token."
  (let ((count up)
	(token
	 (cond
	  ((ada-indent-keywordp child)
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
	    ;; When smie-backward-sexp stops because the current token
	    ;; is an opener, it leaves point on the opener. We can't
	    ;; call smie-backward-sexp again with that token, so we
	    ;; have to move to the next keyword first.
	    ;;
	    ;; FIXME: parens?
	    ;; FIXME: at bob, smie-backward-sexp returns (t 1)
	    (if (listp (nth 0 token))
		;; token is an opener; move to the next keyword, which
		;; might be another opener, so we loop again before
		;; calling smie-backward-sexp
		(progn
		  (setq token (ada-indent-backward-name))
		  (setq token
			(list (nth 1 (assoc token ada-indent-grammar)) (point) token)))
	      (setq token (smie-backward-sexp (nth 2 token)))
	      (setq count (- count 1))))
	))
    ;; If we stopped because we found an opener, point is on token,
    ;; which is where we want it. If we stopped because the next token
    ;; has a higher precedence level, point is on the lower precedence
    ;; token; (nth 1 token) gives the position at the start of the
    ;; higher precendence token. This happens in the following code:
    ;;
    ;; function ...
    ;; is begin
    ;;    return exp;
    ;; end;
    ;;
    ;; 'return' is not an opener. We want point on "begin", which is
    ;; the parent token.
    ;;
    ;; The solution is to always go to the start of the terminating token.

    (goto-char (nth 1 token))
    token)) ;; return value is useful for a debug display

(defun ada-indent-rule-parent (offset &optional child start-token)
  "Find a relevant parent using `ada-indent-goto-parent', return
an indent by OFFSET relevant to that parent. Preserves point.  If
CHILD is non-nil, assume point is at the start of CHILD, which
must be a keyword, and START-TOKEN is the token from which we
searched for CHILD (defaults to CHILD)."
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
  ;; stop there.
  ;;
  ;; When indenting "(Param_1", we want to find the first "function",
  ;; which is the second parent ("(" is the first). Similarly, when
  ;; indenting the second "return", we want to find the second
  ;; "function", which again is the second parent.
  ;;
  ;; When indenting the following Ada statement "type Another_Type",
  ;; we need find "type Function_Access_Type" starting from ";", so we
  ;; have to loop:
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
  ;; is not at the beginning of the line:
  ;;
  ;;    overriding procedure Name;
  ;;
  ;;    protected type Name is ...
  ;;
  ;; The same is true for any other leading Ada keyword that we leave
  ;; as an identifier. To handle this, we do `back-to-indentation'
  ;; once we find the parent.
  ;;
  ;; We do _not_ need a similar loop for assignment statements:
  ;;
  ;;    A := B + C * D;
  ;;
  ;; because we set all operators to the same precedence
  ;; level. However, to find the start of A, we must use
  ;; ada-indent-backward-name.
  ;;
  ;; To find "(" from anywhere within an aggregate, we can use
  ;; `scan-lists'. FIXME: not clear what we want to do from inside an
  ;; aggregate.
  ;;
  ;;
  ;; The following psuedo-Ada leaves point at the proper indentation:
  ;;
  ;;    if start-token /= ";" then
  ;;       -- we are inside an Ada statement/declaration; indent
  ;;       -- relative to the first or second parent
  ;;       if child = "(" then
  ;;          ada-indent-goto-parent child 2
  ;;       else
  ;;          ada-indent-goto parent child 1
  ;;       end if
  ;;    else
  ;;       -- indent relative to the statement/declaration start
  ;;       loop
  ;;          ada-indent-goto-parent 2
  ;;          if not ((parent in ("procedure" "function")) and
  ;;                  (backward-token in ("access" "protected"))
  ;;          then
  ;;             exit loop
  ;;          end if
  ;;       end loop
  ;;    end if
  ;;
  ;;    if parent = ":=" then
  ;;       -- FIXME: this fails for initialized object.
  ;;       ada-indent-backward-name
  ;;    end if
  ;;
  ;;    back-to-indentation

  (save-excursion
    (let ((parent (nth 2 (ada-indent-goto-parent child (or (and (equal child "(") 2) 1)))))

      (if (equal (or start-token child) ";")
	(while (and (member parent `("procedure" "function"))
		    (member (save-excursion (smie-default-backward-token)) '("access" "protected")))
	  (setq parent (nth 2 (ada-indent-goto-parent parent 2)))))
      ;; FIXME: it would probably be best to merge this loop into ada-indent-goto-parent.

      (if (equal parent ":=")
	  (ada-indent-backward-name))

      (back-to-indentation)

      (cons 'column (+ (current-column) offset)))
    ))

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
     ;; arg is a smie keyword, at the beginning of a line.
     (cond
      ((equal arg "(")
       ;; parenthesis occur in expressions, and after names, as array
       ;; indices, subprogram parameters, type constraints.
       ;;
       ;; "(" is in the smie grammar, but smie-forward-token
       ;; handles it specially, returning ("" nil 0). Then
       ;; ada-indent-before-keyword passes "(" here.
       ;;
       ;; Since we have a known token, indenting relative to the
       ;; parent is a good idea (it works for subprogram declarations
       ;; and calls, for example). FIXME: after we get
       ;; ada-indent-exps working, we may want to let that handle
       ;; this.
       (ada-indent-rule-parent ada-indent arg))

      ((equal arg "with-context")
       (cons 'column 0))

      ((member arg ada-indent-block-start-keywords)
       ;; Example:
       ;;
       ;;    procedure
       ;;       (Param_1 : Integer)
       ;;    is
       ;;
       ;; We are indenting 'is'. Indent at the same level as the
       ;; parent. FIXME: this probably won't work for 'declare', 'begin'
       (ada-indent-rule-parent 0 arg))

      ((member arg ada-indent-block-end-keywords)
       ;; Indent relative to the start of the declaration or body,
       ;; which is the parent of this token.
       (ada-indent-rule-parent 0 arg))

      ((not (ada-indent-openerp arg))
       ;; Hanging; we are not at the start of a statement/declaration.
       ;; We have a known keyword; indent relative to its parent.
       (ada-indent-rule-parent ada-indent arg))

      ))

;;; :after
    (:after
     ;; `arg' is a keyword at the end of a line, point is at start of keyword
     ;; :before is checked first, so we don't need to consider those cases here
     (cond
      ((equal arg "(")
       ;; See comments in :before on "(" FIXME: nuh-uh!
       ;;
       ;; We have this case:
       ;;
       ;;    ... name (
       ;;              args
       ;;
       ;; This is from an comp.lang.ada discussion; some people like it
       ;; because it makes it easier to rearrange the list of args.
       (cons 'column (+ (current-column) 1)))

      ((member arg ada-indent-block-start-keywords)
       (if (member (save-excursion
		     (ada-indent-forward-token); "is"
		     (ada-indent-forward-token))
		   (ada-indent-matching-end arg))
	   ;; The token we are indenting is the corresponding block
	   ;; end; we are in an empty block.
	   (ada-indent-rule-parent 0 arg)

	 ;; Indent relative to the start of the declaration or body,
	 ;; which is the parent of this token.
	 (ada-indent-rule-parent ada-indent arg)))

      ((equal arg ";")
       ;; Two cases:
       ;;
       ;; The parent of the sexp preceding ";" is the start of the
       ;; corresponding Ada statement/declaration. Indent at the same
       ;; level as that.
       ;;
       ;; 1) procedure call or pragma:
       ;;
       ;;    package Ada_Mode.Nominal is
       ;;
       ;;       pragma Elaborate_Body (Ada_Mode.Nominal);
       ;;
       ;;       Procedure_1a (A_String, 'a');
       ;;
       ;;    There are no keywords in these sexp, so
       ;;    ada-indent-rule-parent will find the wrong parent. To
       ;;    find the parent of these sexp, we use
       ;;    ada-indent-backward-name.
       ;;
       ;; 2) context clause:
       ;;
       ;;    with name;
       ;;    package name is
       ;;
       ;;    When indenting 'package', ada-backward-name leaves us at
       ;;    'with', which is where we want to stop. There is a
       ;;    similar issue for all single-keyword
       ;;    statement/declarations. We'll keep a list here.
       ;;
       ;; 3) Non-initialized object declaration:
       ;;
       ;;    Object_1 : Integer;
       ;;
       ;;    Integer_G, Integer_H,
       ;;       Integer_I : Integer;
       ;;
       ;;    Here ":" is the only keyword in the sexp. We want a
       ;;    variant on ada-indent-backward-name; that goes one token
       ;;    too far. So call it, then come back one.
       ;;
       ;;    FIXME: That fails in this case:
       ;;
       ;;       Integer_G, Integer_H,
       ;;          Integer_I : Integer;
       ;;
       ;;       Integer_J,
       ;;          Integer_K, Integer_L : Integer;
       ;;
       ;;    When indenting Integer_K, there no keywords in sight, so
       ;;    we don't get here, and ada-indent-default does the wrong
       ;;    thing.  We could try to implement
       ;;    ada-indent-before-name. But "," will probably be a
       ;;    keyword when we get to aggregates, so we'll leave this
       ;;    for now.
       ;;
       ;; 4) use ada-indent-rule-parent 0
       ;;
       ;; To distinguish among the cases, we call
       ;; ada-indent-backward-name and check the found keyword.

       (save-excursion
	 (let ((token (ada-indent-backward-name)))
	   (cond
	    ((or
	      (equal token ";")
	      (member token ada-indent-block-start-keywords))
	     ;; this is a procedure call or pragma
	     (progn
	       ;; Move back to the actual parent
	       (ada-indent-forward-token); past the found token
	       (forward-comment (point-max)); also skips newlines and whitespace
	       (cons 'column (+ (current-column)))))

	    ((equal token ":")
	     (ada-indent-backward-name); now on the preceding ";" or block-start
	     (smie-default-forward-token); after the ";"
	     (smie-default-forward-token); after the first object identifier
	     (smie-default-backward-token)
	     (cons 'column (+ (current-column))))

	    ((equal token "with-context")
	     (back-to-indentation)
	     (cons 'column (+ (current-column))))

	    (t (ada-indent-rule-parent 0 token ";"))
	    ))))

      ;; We are left with these cases:
      ;;
      ;; 1) multi-token hanging:
      ;;
      ;;    type name is abstract
      ;;       tagged limited null record;
      ;;
      ;;    indent relative to parent
      ;;
      ;; 2) single-token hanging:
      ;;
      ;;    begin
      ;;       return
      ;;          foo;
      ;;
      ;;    We are indenting 'foo', and point is at the start of
      ;;    'return', because we are in :after.
      ;;
      ;;    We want to indent relative to 'return', which is on the
      ;;    current line.
      ;;
      ;; smie-indent--hanging-p returns nil if the keyword is alone
      ;; on the line, which is the distinguishing criterion we need.
      ;;
      ((smie-indent--hanging-p)
       (ada-indent-rule-parent ada-indent arg))

      (t (ada-indent-rule-current ada-indent))
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
	 ;; would align the comment with 'private', which is
	 ;; wrong. Other indentation functions can't work here. So we
	 ;; call a subset of the functions.
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
	  (ada-indent-keywordp token)
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
	 (ada-indent-keywordp token)
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
line. Intended to be the last item in `smie-indent-functions',
used when no indentation decision was made."
  (cdr (ada-indent-rule-parent ada-indent)))

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
    (goto-char (nth 1 toklevels))
    (message "%s" (assoc (save-excursion (ada-indent-forward-token)) ada-indent-grammar))))

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
