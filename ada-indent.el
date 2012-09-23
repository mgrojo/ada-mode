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
       ;; That means we only need enough of the grammar to specify the
       ;; precedence relationships among keywords (operators are
       ;; handled below), so lots of things are left out.
       ;;
       ;; However, we need to be able to compute proper indentation
       ;; with a line break between any two tokens, so all Ada
       ;; keywords are present in this grammar.
       ;;
       ;; We also need to be able to parse across large sections of
       ;; code, to match 'package' with 'private'; we need all the
       ;; keywords for that.
       ;;
       ;; Rather that start with all of appendix P and then fix
       ;; problems, we start with an extremely minimal grammar, and
       ;; add rules as we uncover a need for them; see test/Makefile.
       ;;
       ;; SMIE automatically allows balanced parens anywhere, so we
       ;; don't need to declare argument lists or discriminant lists
       ;; in the grammar.
       ;;
       ;; 'is' is used at several levels, so the lexer returns several
       ;; different tokens for it, to resolve grammar conflicts, and
       ;; for use with smie-backward-sexp to get to the correct
       ;; parent.
       ;;
       ;; ';' has a similar problem; it is used in several different
       ;; constructs, where we need to correctly identify the
       ;; parent. We solve that by declaring it as the separator for
       ;; those constructs.
       ;;
       ;; Other keywords are similarly refined to avoid grammar conflicts.
       ;;
       ;; We don't include any tokens after "end" in the grammar, so
       ;; it is always a closer.
       ;;
       ;; For all ';' separated non-terminals, the first and last
       ;; tokens in the syntax should be a keyword, so the
       ;; non-terminal is a single sexp when parsing forwards or
       ;; backwards. An exception is when there is a single keyword in
       ;; the syntax; then a trailing name is ok, and can reduce the
       ;; number of refined keywords we need. There are a couple of
       ;; other exceptions, noted below.

       ;; alphabetical order, since there isn't any more reasonable order
       ;; we use the same names as [1] Annex P as much as possible

       (context_clause
	(context_item)
	(context_item ";" context_item))

       (context_item
	("with-context")
	("use"))

       (declaration
	;; FIXME: (package_specification), (package_body); not tested yet.
	(pragma)
	(protected_body)
	(type_declaration)
	(subprogram_declaration)
	(subprogram_body)
	(identifier ":") ; object_declaration
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
	;; Ada legality rules, just computing indentation.
	(name "-operator-" name)
	("(" expression ")"))

       (generic_package_declaration
	;; no need to distinguish between 'declarations' and
	;; 'generic_formal_parameter_declaration' for our purposes.
	("generic" declarations
	 "package-generic" name "is-package" declarations "private" declarations "end")
	("generic" declarations
	 "package-generic" name "is-package" declarations "end"))

       (interface_list
	;; The Ada grammar sometimes has "name and interface_list".
	;; We can't (and don't need to) distinguish that from "interface_list"
	(name)
	(interface_list "and-interface_list" name))

       (name
	(identifier)
	(name "." identifier) ; selected_component
	)

       (package_specification
	("package" name "is-package" declarations "private" declarations "end")
	("package" name "is-package" declarations "end"))

       (package_body
	;; Leaving 'package body' as separate tokens causes problems in refine-is
	("package_body" name "is-package_body" declarations "begin" statements "end"))

       (pragma
	("pragma"))

       (protected_body ("protected_body" identifier "is-protected_body" declarations "end"))

       (statement
	(expression); matches procedure calls, assignment

	;; We leave "null" as an identifier, because it appears in so
	;; many places, and acts like a parameterless procedure call
	;; in most.

	("return")
	("return-exp")
	("return-do" identifier ":")
	("return-do" identifier ":-do" name "do" statements "end_return")
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
	("procedure" name "is_abstract")
	("procedure" name "is_null")
	)

       (type_declaration
	;; access_type_definition
	("type" identifier "is-type_access")
	("type" identifier "is-type_access_all")
	("type" identifier "is-type_access_constant")
	("type" identifier "is-type_not_null_access")
	("type" identifier "is-type_not_null_access_all")
	("type" identifier "is-type_not_null_access_constant")

	("type" identifier "is-type_access_procedure")
	("type" identifier "is-type_access_protected_procedure")
	("type" identifier "is-type_access_function" "return-access")
	("type" identifier "is-type_access_function" "return+access" name); same as 'function name return access'
	("type" identifier "is-type_access_protected_function" "return-access")
	("type" identifier "is-type_access_protected_function" "return+access" name)
	;; Note the difference between return-access and return+access;
	;; the latter is returning an anonymous access type. Perhaps
	;; not the best convention ...

       ;; array_type_definition
       ("type" identifier "is-type_array" expression "of")

       ;; derived_type_declaration
       ("type" identifier "is-type" "new" name); same as below
       ("type" identifier "is-type" "-type-modifiers-" "new" name)
       ("type" identifier "is-type" "new" name "with_null_record")
       ("type" identifier "is-type" "-type-modifiers-" "new" name "with_null_record")
       ("type" identifier "is-type" "new" name "with" "record" declarations "end_record")
       ("type" identifier "is-type" "-type-modifiers-" "new" name "with" "record" declarations "end_record")
       ("type" identifier "is-type" "new" interface_list "with" "record" declarations "end_record")
       ("type" identifier "is-type" "-type-modifiers-" "new" interface_list "with" "record" declarations "end_record")

       ;; enumeration_type_definition
       ("type" identifier "is-enumeration_type")
       ;; enumeration literals are an aggregate, which is an expression.

       ;; {ordinary_ | decimal_} fixed_point_definition
       ("type" identifier "is-type_delta" expression); match next
       ("type" identifier "is-type_delta_digits")

       ;; floating_point_definition
       ("type" identifier "is-type_digits" expression)
       ("type" identifier "is-type_digits" expression "range")

       ;; integer_type_definition
       ("type" identifier "is-type_range"); ".." is an operator

       ;; interface_type_definition, formal_interface_type_definition
       ("type" identifier "is-type" "interface")
       ("type" identifier "is-type" "interface_and" interface_list)
       ;; FIXME: [limited | task | protected | synchronized]

       ;; modular_type_definition
       ("type" identifier "is-type" "mod-type")

       ;; private_extension_declaration
       ("type" identifier "is-type" "new" name "with_private")
       ("type" identifier "is-type" "-type-modifiers-" "new" name "with_private")
       ("type" identifier "is-type" "new" interface_list "with_private")
       ("type" identifier "is-type" "-type-modifiers-" "new" interface_list "with_private")
       ;; leaving 'with' and 'private' as separate tokens causes conflicts

       ;; private_type_declaration
       ("type" identifier "is-type" "private-type")
       ("type" identifier "is-type" "-type-modifiers-" "private-type")

       ;; protected_type_declaration, single_protected_declaration
       ;;
       ;; prefixing "protected" gives a precedence conflict: 'token
       ;; type is both neither and opener', so "protected type" is
       ;; combined into one token in
       ;; ada-indent-forward/backward-token.
       ("protected_type" identifier "is-type" declarations "private" declarations "end")
       ("protected_type" identifier "is-type" "new" interface_list "with" declarations
	"private" declarations "end")

       ;; record_type_definition
       ("type" identifier "is-type" "null_record")
       ("type" identifier "is-type" "record" declarations "end_record")
       ("type" identifier "is-type" "-type-modifiers-" "null_record")
       ("type" identifier "is-type" "-type-modifiers-" "record" declarations "end_record")
       ;; no need to distinguish between 'declarations' and 'component_list'

       ); type_declaration
       )); smie-bnf->prec2

    (smie-precs->prec2
     '((nonassoc "-type-modifiers-")
       (nonassoc "abstract" "tagged" "limited")))

    ;; operators and similar things
    (smie-precs->prec2
     '((nonassoc "-operator-")
       ;; The structure of this table is stolen from the modula2 smie grammar.
       ;;
       ;; We can merge the relational, math, and other operators in
       ;; these levels, because we don't care about legality.  Which
       ;; means we really don't need any precedence hierarchy for
       ;; these at all, but it also doesn't hurt.
       ;;
       (nonassoc "=" "/=" "<" "<=" ">" ">=" "in") ; relational_operator, membership
       (assoc "or" "or_else" "xor" "+" "-" "&")
       (assoc "and" "and_then" "mod" "rem" "*" "/")
       (right "abs" "not")
       (left "'" "." "**" "..") ; Qualifier, selector, exponent, range
       (nonassoc ":=") ; assignment statement (always done last)
       ))
    )))

;;; utils for refine-*, forward/backward token

(defconst ada-indent-block-start-keywords
  '(
    ;; FIXME: begin?
    "is-entry_body"
    "is-package_body"
    "is-package"
    "is-protected_body"
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
don't actually appear in the grammar (if there still are any)
look like identifiers, so this also skips them.
Return the token that isn't part of the name (which may be found
before any name is seen).
Return empty string if encounter beginning of buffer."
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
;; double token (end_return, package_body etc).
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
  ;; All other occurances are logical expressions, returning "and".
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
  "Skip forward tokens that are in the \"-type-modifiers-\" non-terminal, return the following token."
  (let (result)
    (while (member (setq result (smie-default-forward-token)) ada-indent-type-modifiers))
    result))

(defconst ada-indent-type-keywords
  '("delta" "digits"
    "range"
    "array"
    "not" "null" "access" "all" "constant" "protected" "procedure" "function")
  "Kewords that can be combined with \"is\" in an access type, in the order they can appear.")

(defun ada-indent-skip-type-keywords-backward ()
  "Skip tokens, using smie-default-backward-token, if they are in `ada-indent-type-keywords.
Return next token."
  (let (token)
    (while (member (setq token (smie-default-backward-token)) ada-indent-type-keywords))
    token))

(defun ada-indent-consume-type-keywords (keyword)
  "Skip tokens forward if they are in `ada-indent-type-keywords.
Return (token count), where `token' is the concatentation of
\"is-type_KEYWORD\" and the keywords read, and `count' is the count of
keywords read."
  (let ((result (concat "is-type_" keyword))
	(count 1)
	token)
    (while (member (setq token (smie-default-forward-token)) ada-indent-type-keywords)
      (setq count (1+ count)
	    result (concat result "_" token)))
    (list result count)))

(defun ada-indent-refine-is (forward)
  (let((skip nil)
       ;; skip = number of tokens after 'is' to skip with smie-default-*-token before returning
       res)
    (setq
     res
     (save-excursion
       (when forward (smie-default-backward-token))

       ;; too many occurances to document them all.

       (or
	;; First try simple, common constructs.

	(let ((token (save-excursion (ada-indent-backward-name))))
	  (cond
	   ((equal token "package") "is-package")
	   ;; "package" name ^ "is"

	   ((equal token "package_body") "is-package_body")
	   ;; "package" "body" name ^ "is"

	   ;; FIXME: we don't handle private here yet, because that is recursive

	   ((equal token "procedure")
	    ;; "procedure" name ^ "is"
	    ;;  procedure name is abstract;
	    (if (equal "abstract" (save-excursion (smie-default-forward-token)))
		"is-abstract-null"
	      "is-subprogram_body"))

	   ((equal token "protected_type") "is-type")
	   ;; "protected" identifier ^ "is"

	   ((equal token "protected_body") "is-protected_body")
	   ;; "protected" "body" identifier ^ "is"

	   ((equal token "return-spec")
	    ;; "function" identifier "return" name ^ "is" declarations "begin"
	    ;; FIXME: abstract
	    "is-subprogram_body")

	   ((equal token "type")
	    (let ((token
		   (progn
		     (smie-default-forward-token); is
		     (ada-indent-skip-type-modifiers))))
	      (cond
	       ((and (equal token "")
		     (looking-at "("))
		"is-enumeration_type");; type identifier is (...)

	       ((member token '("delta" "digits" "range" "array" "not" "access"))
		(let ((temp (ada-indent-consume-type-keywords token)))
		  (setq skip (cadr temp))
		  (car temp)))

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
  "Skip tokens, moving backward, that are in
  `ada-indent-type-keywords'. If the next
  token (possibly START-TOKEN) is \"is\",
  call (ada-indent-refine-is FORWARD-KEYWORD)."
  (cond
   ((equal start-token "is")
    (ada-indent-refine-is forward))

   ((member token ada-indent-type-keywords)
    (let ((pos (point))
	  (token (ada-indent-skip-type-keywords-backward)))

      ;; We can't just look for 'is'; consider this case:
      ;;
      ;;    protected type Protected_1 is
      ;;       function F1 return Integer;
      ;;       function F2 return Integer;
      ;;    private
      ;;
      ;; vs
      ;;    type Function_Access_Type is access function (...) return float;
      ;;
      ;; If we are refining 'return' in function F1, we call
      ;; ada-indent-backward-name to look for 'function'. But that
      ;; calls ada-indent-backward-token, which calls
      ;; ada-indent-maybe-refine-is, and we skip the 'function' in F1
      ;; thinking it might be the 'function' in Function_Access_Type.
      ;;
      (if (and (equal "is" token)
	       (not (member (save-excursion (ada-indent-backward-name)) '("protected_type" "task_type"))))
	  (progn
	    (when forward
	      ;; skip-type-keywords-backward found "is" while moving
	      ;; backward, but ultimately called from
	      ;; ada-indent-forward-keyword; perhaps because we
	      ;; started between "is" and "not".
	      ;;
	      ;; Set point to match what refine-is expects for forward-keyword.
	      (smie-default-forward-token))
	    (ada-indent-refine-is forward))
	;; not refining
	(goto-char pos)
	nil)))
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
       ;;    function identifier (...) return [access] name;
       ;;
       ;;    preceding token: "function"
       ;;    token: 1a) "return-spec" or 1b) "return+access"
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
       ;;    preceding token: "is-type_access_function", "is-type_access_protected_function"
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
	   ((member token '("is-type_access_function" "is-type_access_protected_function"))
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

     ((equal token "not")
       (when (equal "null" (save-excursion (smie-default-forward-token)))
	 (smie-default-forward-token)
	 "not_null"))

     ((equal token "null")
      (or
       (when (equal "not" (save-excursion (smie-default-backward-token)))
	 (smie-default-forward-token)
	 "not_null")

       (when (equal "record" (save-excursion (smie-default-forward-token)))
	 (smie-default-forward-token)
	 "null_record")

       ;; We don't return "null" here; we are leaving it as an
       ;; identifier. (see comment in `statements' in
       ;; ada-indent-grammar)
       ))

     ((equal token "package")
      (if (save-excursion (equal "access" (smie-default-backward-token)))
	  "package-access"
	(if (equal "body" (save-excursion (smie-default-forward-token)))
	    (progn
	      (smie-default-forward-token)
	      "package_body")
	  "package_type")))

     ((equal token "private") (ada-indent-refine-private t))

     ((equal token "return") (ada-indent-refine-return t))

     ((equal token "with")
      (cond
       ((equal "with" (save-excursion (smie-default-forward-token)))
	(smie-default-forward-token)
	"with_private")

       ;;(FIXME: "with-context")

       (t "with")))

     ((ada-indent-maybe-refine-is token t))

     ((equal token "protected")
      ;; access_definition, access_to_subprogram_definition handled by maybe-refine-is
      (if (equal "body" (save-excursion (smie-default-forward-token)))
	  (progn
	    (smie-default-forward-token)
	    "protected_body")
	;; We don't check forward for "type" because that would be recursive
	;; FIXME: expand syntax for protected, verify this is correct.
	"protected_type"))

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

     ((equal token "body")
      (let ((token (save-excursion (smie-default-backward-token))))
	(cond
	 ((equal token "package")
	  (progn
	    (smie-default-backward-token)
	    "package_body"))

	 ((equal token "protected")
	  (progn
	    (smie-default-backward-token)
	    "protected_body"))

	 (t (ada-indent-refine-error "unrecognized 'body'")))))

     ;; "function", "is" handled by maybe-refine-is below

     ((equal token "mod") (ada-indent-refine-mod nil))

     ((equal token "not")
      (if (equal "null" (save-excursion (smie-default-forward-token)))
	(progn
	  (smie-default-forward-token)
	  "not_null")
	"not"))

     ((equal token "null")
      (if (equal "not" (save-excursion (smie-default-backward-token)))
	  (progn
	    (smie-default-backward-token)
	    "not_null")
	"null"))

     ((equal token "package")
      ;; FIXME: this is ok for a library level [generic] package alone
      ;; in a file. But it could be a problem for a nested [generic]
      ;; package. Idea: "end" can't occur in generic formal
      ;; parameters; search for "end|generic".
      (if (equal "generic" (save-excursion (smie-backward-sexp "package-generic")))
	  "package-generic"
	"package"))

     ((equal token "private") (ada-indent-refine-private nil))

     ;; "procedure" handled by maybe-refine-is below

     ((equal token "record")
      (let ((token (save-excursion (smie-default-backward-token))))
	(cond
	 ((equal token "end") (smie-default-backward-token) "end_record")
	 ((equal token "null") (smie-default-backward-token) "null_record")
	 (t "record")
	 )))

     ((equal token "return") (ada-indent-refine-return nil))

     ((equal token "type")
       (if (equal "protected" (save-excursion (smie-default-backward-token)))
	   (progn
	     (smie-default-backward-token)
	     "protected_type")
	 "type"))

     ((equal token "with")
      ;; 'with' occurs in:
      ;;
      ;; 1) record_extension_part in a [formal_]derived_type_definition in a type_declaration:
      ;;    type ... is ... new parent_subtype_indication [[and interface_list] with record_definition]
      ;;
      ;;    preceding keyword: "new", "and-interface_list"
      ;;    skip: name
      ;;    keyword: "with"
      ;;
      ;; 2) extension_aggregate ::=
      ;;    (ancestor_part with record_component_association_list)
      ;;
      ;;    not implemented yet
      ;;
      ;; 3) private_extension_declaration (see "private" above)
      ;;
      ;;    parsed as "with_private" by "private" above.
      ;;
      ;; 4) task_type_declaration, single_task_declaration (see "and" above)
      ;;
      ;;    parsed as "with_private" by "private" above.
      ;;
      ;; 5) protected_type_declaration, single_protected_declaration ::=
      ;;    protected [type] defining_identifier [known_discriminant_part] [aspect_specification] is
      ;;       [new interface_list with] protected_definition;
      ;;
      ;;    preceding keyword: "new", "and-interface_list"
      ;;    skip: name
      ;;    keyword: "with"
      ;;
      ;; 6) requeue_statement ::= requeue procedure_or_entry_name [with abort];
      ;;
      ;;    not implemented yet; will be parsed as "with_abort"
      ;;
      ;; 7) with_clause ::= [limited] [private] with library_unit_name {, library_unit_name};
      ;;
      ;;    preceding keyword: "private", ";", beginning of buffer
      ;;    keyword: "with-context"
      ;;
      ;; 8) raise_statement ::= raise;
      ;;       | raise exception_name [with string_expression];
      ;;
      ;;    not implemented yet
      ;;
      ;; 9) formal_concrete_subprogram_declaration, formal_abstract_subprogram_declaration ::=
      ;;    with subprogram_specification [is [abstract] subprogram_default] [aspect_specification];
      ;;
      ;;    not implemented yet
      ;;    followed by "function", "procedure"
      ;;
      ;; 10) formal_package_declaration ::=
      ;;        with package defining_identifier is new generic_package_name  formal_package_actual_part
      ;;        [aspect_specification];
      ;;
      ;;    not implemented yet
      ;;    followed by "package"
      ;;
      ;; 11) aspect_specification ::= with aspect_mark [=> aspect_definition] {,aspect_mark [=> aspect_definition] }
      ;;
      ;;    not implemented yet
      ;;    followed by "=>", ",", ";"
      ;;
      (let ((token (save-excursion (ada-indent-backward-name))))
	(cond
	 ((or (equal token "new")
	      (equal token "and-interface_list"))
	  "with"); 1, 5

	 ((or (equal token "private")
	      (equal token ";")
	      (equal token "")); beginning of buffer

	  "with-context"); 7
	 )))

     ((ada-indent-maybe-refine-is token nil))

     ((equal token "access")
      (let ((token (save-excursion (smie-default-backward-token))))
	(cond
	 ((equal token "return")
	  (smie-default-backward-token)
	  (ada-indent-refine-return nil))

	 ;; other cases handled by maybe-refine-is

	 (t (ada-indent-refine-error "unrecognized 'access'")))))

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
      (if (equal "body" (save-excursion (smie-default-forward-token)))
	  "protected_body"
	"protected_type"))

    (t token))))

;;; indent rules

(defun ada-indent-rule-current (offset)
  "Indent relative to the current line"
  (cons 'column (+ (save-excursion (back-to-indentation) (current-column)) offset)))

(defun ada-indent-keywordp (token)
  (assoc token ada-indent-grammar))

(defun ada-indent-openerp (token)
  (listp (nth 1 (assoc token ada-indent-grammar))))

(defconst ada-indent-single-token-sexps
  '("pragma")
  "Keywords that stand alone in an Ada statement or declaration.")

(defun ada-indent-rule-parent (offset &optional child)
  ;; If child is non-nil, find the relative parent, indent relative to
  ;; that.  If child is nil, find the previous smie token, start
  ;; there; that token may be the relevant parent.
  (save-excursion
    (let ((token (or child
		     (ada-indent-backward-name))))
      (if (or
	   (ada-indent-openerp token)
	   (member token ada-indent-single-keyword-sexps))
	  ;; token is the parent; we don't need to skip back more
	  nil
	(smie-backward-sexp token))
      ;; We don't consider any special cases here; the caller must
      ;; know what they are doing!
      (cons 'column (+ (current-column) offset)))))

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
       ;; "(" is _not_ in the smie grammar, but smie-forward-token
       ;; handles it specially, returning ("(" nil 0). Then
       ;; smie-indent-keyword treats it as a keyword, passing it here.
       ;; If we return nil, and the paren is at the beginning of a
       ;; line, smie-indent-keyword gives up, returning nil. So then
       ;; smie-indent-after-keyword has a go, looking at the previous
       ;; token. Since that is usually a name, not a keyword, that
       ;; does nothing as well, and we get indented to zero.
       ;;
       ;; In addition, we get here in several cases:
       ;;
       ;;    1)    ... name
       ;;             (args)
       ;;
       ;;    2)    ... name (args
       ;;                    )
       ;;
       ;; In case 1, we want to skip back to a keyword, find the
       ;; parent, indent from there.
       ;;
       ;; In case 2, we want to indent 1 from where we are.
       (if (smie-indent--bolp)
	   ;; case 1
	   (if (ada-indent-keywordp (save-excursion (ada-indent-backward-token)))
	       ;; let :after handle it
	       nil
	     (ada-indent-rule-parent ada-indent))
	 ;; case 2
	 (cons 'column (+ (current-column) 1))
	 ))

      ((member arg '("generic"))
       ;; FIXME: why do we need this?
       (ada-indent-rule-parent 0 arg))

      ((equal arg "with")
       ;; context clause; FIXME: also used in derived record declaration
       (cons 'column 0))

      ((member arg ada-indent-block-start-keywords)
       ;; Example:
       ;;
       ;;    procedure
       ;;       (Param_1 : Integer)
       ;;    is
       ;;
       ;; We are indenting 'is'. Indent at the same level as the
       ;; parent.
       (ada-indent-rule-parent 0))

      ((member arg ada-indent-block-end-keywords)
       ;; Indent relative to the start of the declaration or body,
       ;; which is the parent of this token.
       (ada-indent-rule-parent 0 arg))

      (t
       ;; If the previous token is not a smie keyword,
       ;; smie-indent-after-keyword won't work. So handle that case
       ;; here.
       (if (not (ada-indent-keywordp (save-excursion (ada-indent-backward-token))))
	   ;; Indent relative to parent of current token
	   (ada-indent-rule-parent ada-indent arg)))

      ;; FIXME: might as well call ada-indent-rules :after here!
      ))
;;;
    ;; FIXME: split this out
    (:after
     ;; `arg' is a keyword at the end of a line
     ;; :before is checked first, so we don't need to consider those cases here
     (cond
      ((equal arg "(")
       ;; See comments in :before on "("
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
       ;; The parent of the sexp preceding ";" is the start of the
       ;; corresponding Ada statement/declaration. Indent at the same
       ;; level as that.
       (ada-indent-rule-parent 0))

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
      ;;    'return', because we are in smie-indent-after-keyword.
      ;;
      ;;    Indent relative to 'return'. indent-rule-parent does not
      ;;    work here; 'return' and 'begin' are not parents of
      ;;    anything.
      ;;
      ;; smie-indent--hanging-p returns nil if the keyword is alone
      ;; on the line, which is the distinguishing criterion we need.
      ((smie-indent--hanging-p)
       (ada-indent-rule-current ada-indent))

      (t (ada-indent-rule-parent ada-indent))
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
	      (ada-indent-wrapper 'smie-indent-after-keyword)
	      (ada-indent-wrapper 'ada-indent-non-keyword)
	      )
	   (or
	    (smie-indent-bob)
	    (smie-indent-after-keyword)
	    (ada-indent-non-keyword)
	    )))
       ))

(defun ada-indent-before-keyword()
  "Replacement for `smie-indent-keyword', tailored to Ada.
It requires `ada-indent-rule' to return nil or ('column column),
never just an offset (since we would not know what the offset was
relative to."
  (let ((token (save-excursion (ada-indent-forward-token)))
	indent)

    (when (ada-indent-keywordp token)
      (setq indent (ada-indent-rules :before token))

      ;; Here we replace smie-indent--rules, so ada-indent-rules
      ;; cannot use smie-rule-parent; we use ada-indent-rule-parent
      ;; We do _not_ call smie-indent-virtual.
      (cond
       ((null indent) nil)
       ((eq (car-safe indent) 'column) (cdr indent))
       (t (error "Invalid `ada-indent-rules' result %s" indent))))
    ))

(defun ada-indent-error ()
  "Throw an error. Intended to be the last item in
`smie-indent-functions', to warn when no indentation decision was
made."
  (error "no indent computed"))

;;; debug
(defun ada-indent-show-keyword-forward ()
  "Show the grammar info for word following point, and move across it."
  (interactive)
  (message "%s" (assoc (ada-indent-forward-token) smie-grammar)))

(defun ada-indent-show-keyword-backward ()
  "Show the grammar info for word preceding point, and move across it."
  (interactive)
  (message "%s" (assoc (ada-indent-backward-token) smie-grammar)))

(defun ada-indent-show-parent()
  "Show the parent of the keyword following point, and move to it."
  (interactive)
  (let ((token (save-excursion (ada-indent-forward-token))))
    (when (not (ada-indent-keywordp token))
      (setq token (ada-indent-backward-name)))

    ;; Duplicate code in ada-indent-rule-parent
    (when
	(not
	 (or (ada-indent-openerp token)
	     (member token ada-indent-single-keyword-sexps)))
      (setq token (nth 2 (smie-backward-sexp token))))

    (message "%s" token)
    ))

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
  ;; FIXME: this still doesn't work; need some other form of 'lambda'
  (let (res func)
    (while (setq func (pop smie-indent-functions))
      (let ((newfunc (macroexpand `(ada-indent-wrap ,func))))
	(setq res (cons newfunc res))))
    (setq res (cons `ada-indent-error res))
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
  ;; ada-indent-before-keyword before smie-indent-after-keyword.
  ;;
  ;; We started out trying to use smie-indent-keyword. However, there
  ;; are too many cases when it does the wrong thing for Ada. We keep
  ;; the same overall structure of the code; ada-indent-before-keyword
  ;; calls ada-indent-rules in the same way, except it assumes
  ;; ada-indent-rules returns a column, not an offset.
  ;;
  ;; If ada-indent-rules returns an offset, instead of a column,
  ;; smie-indent-after-keyword will do something mysterious. So we
  ;; require ada-indent-rules to always return a column for :before
  ;; and :after.

  (make-local-variable 'smie-indent-functions)

  (if debug-on-error
      (setq smie-indent-functions
	    (list
	     (lambda () (ada-indent-wrapper 'smie-indent-bob))
	     (lambda () (ada-indent-wrapper 'smie-indent-close))
	     (lambda () (ada-indent-wrapper 'ada-indent-comment))
	     (lambda () (ada-indent-wrapper 'ada-indent-before-keyword))
	     (lambda () (ada-indent-wrapper 'smie-indent-after-keyword))
	     (lambda () (ada-indent-wrapper 'ada-indent-non-keyword))
	     (lambda () (ada-indent-wrapper 'smie-indent-exps))
	     'ada-indent-error)
	    )
    (setq smie-indent-functions
	  '(smie-indent-bob; handle first non-comment line in buffer

	    smie-indent-close
	    ;; align close paren with opening paren.  FIXME: uses
	    ;; smie-indent-virtual, which may explain why it doesn't
	    ;; work with our code.

	    ada-indent-comment
	    ada-indent-before-keyword

	    smie-indent-after-keyword
	    ;; FIXME: calls smie-indent-virtual if ada-indent-rules fails

	    ada-indent-non-keyword
	    smie-indent-exps);; FIXME: haven't tested expressions yet
	  )
    )

  (smie-setup ada-indent-grammar #'ada-indent-rules
	      :forward-token #'ada-indent-forward-token
	      :backward-token #'ada-indent-backward-token))

(add-hook 'ada-mode-hook 'ada-indent-setup)

(define-key ada-mode-map "\t" 'indent-for-tab-command)
;; TAB will now use smie indentation in Ada mode buffers

(provide 'ada-indent)

;;; end of file
