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
       ;; We only need enough of the grammar to allow indentation to work; see
       ;; (info "(elisp)SMIE Grammar")
       ;;
       ;; That means we only need enough of the grammar to specify the precedence
       ;; relationships among keywords (operators are handled below),
       ;; so all BNF productions that have only one keyword are left
       ;; out.
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
       ;; different tokens for it, so that smie-indent--parent will
       ;; identify the correct parent.
       ;;
       ;; ';' has a similar problem; it is used in several different
       ;; constructs, where we need to correctly identify the
       ;; parent. We solve that by declaring it as the separator for
       ;; those constructs.
       ;;
       ;; We don't include any tokens after "end" in the grammar, so
       ;; it is always a closer. SMIE allows extra tokens!
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

       (access_type_definition
	("type" identifier "is-type" "access" name)
	;; trailing 'name' to match 'access protected procedure'.
	;; don't need "not null all"; they are just ignored (treated as identifiers)

	("type" identifier "is-type" "access" "protected-access" "procedure-access")
	("type" identifier "is-type" "access" "protected-access" "function-access" "return-access"))

       (array_type_definition ("type" identifier "is-type" "array" expression "of"))

       (context_clause
	(context_item)
	(context_item ";" context_item))

       (context_item
	("with-context")
	("use"))

       (declaration
	(access_type_definition)
	(array_type_definition)
	(derived_type_declaration)
	(interface_type_definition)
	(private_extension_declaration)
	(private_type_declaration)
	(protected_body)
	(protected_type_declaration)
	(record_type_definition)
	(subprogram_declaration)
	(subprogram_body)
	(identifier ":") ; object_declaration
	)

       (declarations
	(declaration)
	(declaration ";" declaration))

       (derived_type_declaration
	("type" identifier "is-type" "new" name); same as the following
	("type" identifier "is-type" "new" name "with" "null_record")
	("type" identifier "is-type" "new" name "with" "record" declarations "end_record")
	("type" identifier "is-type" "new" interface_list "with" "record" declarations "end_record"))

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
	 "package-generic" name "is-package_declaration" declarations "begin" statements "end"))

       (interface_list
	;; The Ada grammar sometimes has "name and interface_list".
	;; We can't (and don't need to) distinguish that from "interface_list"
	(name)
	(interface_list "and-interface_list" name))

       (interface_type_definition
	("type" name "is-type" "interface")
	("type" name "is-type" "interface_and" interface_list))
       ;; [limited | task | protected | synchronized] ignored
       ;; also covers formal_interface_type_definition

       (name
	(identifier)
	(name "." identifier) ; selected_component
	)

       (package_declaration
	("package" name "is-package_declaration" declarations "begin" statements "end"))

       (package_body
	;; Leaving 'package body' as separate tokens causes problems in refine-is
	("package_body" name "is-package_body" declarations "begin" statements "end"))

       (private_extension_declaration
	("type" identifier "is-type" "new" name "with_private")
	("type" identifier "is-type" "new" interface_list "with_private"))
       ;; leaving 'with' and 'private' as separate tokens causes conflicts

       (private_type_declaration ("type" identifier "is-type" "private-type"))

       (protected_body ("protected_body" identifier "is-protected_body" declarations "end"))

       (protected_type_declaration
	;; prefixing "protected" gives a precedence conflict: 'token
	;; type is both neither and opener', so "protected type" is
	;; combined into one token in
	;; ada-indent-forward/backward-token.
	("protected_type" identifier "is-type" declarations "private" declarations "end")
	("protected_type" identifier "is-type" "new" interface_list "with" declarations
	 "private" declarations "end"))
       ;; also covers single_protected_declaration

       (record_type_definition ("type" identifier "is-type" "record" declarations "end_record"))
       ;; no need to distinguish between 'declarations' and 'component_list'

       (statement
	(expression); matches procedure calls, assignment
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
	("procedure" name "is-subprogram_body" declarations "begin" statements "end"))

       (subprogram_declaration
	;; factoring out subprogram_specification here breaks something.

	("function" name "return-spec" name)
	;; trailing name makes this return-spec the same as same as
	;; 'function name return-spec name is-subprogram-body'; that
	;; avoids recursion between refine-is and refine-return

	("procedure" name)); same as 'procedure name is-subprogram_body'
       ))

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
       (left "'" "." "**") ; Qualifier, selector, exponent
       (nonassoc ":=") ; assignment statement (always done last)
       ))
    )))

;;; utils for refine-*, forward/backward token

(defvar ada-indent-recursing nil
  "Bound to non-nil on some occasions when
ada-indent-forward/backward-token calls
ada-indent-forward/bacward-token, to avoid infinite recursion.")
;; FIXME: not used yet; too many changes at once

(defun ada-indent-skip-param_list (next-token direction)
  ;; While refining tokens, we don't want to call smie-next-sexp,
  ;; because it relies on refined tokens. So we just call the C
  ;; scanner (see the lisp source for forward-sexp).
  ;;
  ;; We could call (smie-backward-sexp), which would be safe, and
  ;; would call the C scanner on the second iteration (it also binds
  ;; forward-sexp-function nil when it hits a paren), but this is
  ;; clearer.
  (let ((forward-sexp-function nil))
    (if (eq direction 'ada-indent-backward-name)
	(backward-sexp)
    (forward-sexp))))

(defun ada-indent-next-name (next-token)
  "Skip over a name using NEXT-TOKEN. A 'name' consists of
identifiers, dots, and anything that looks like a parameter
list (could be an array index). Note that Ada 2012 keywords that
don't actually appear in the grammar look like identifiers, so
this also skips them. Return the token that isn't part of the
name (may be before any name is seen)."
  (let (token)
    (while
	(progn
	  (setq token (funcall next-token))
	  (if (equal "" token)
	      ;; we hit a parameter list or something similar; use the lower level scanner
	      (progn
		(ada-indent-skip-param_list next-token 'ada-indent-backward-name)
		(setq token (funcall next-token))))
	  (setq token (nth 0 (assoc token smie-grammar)))
	  (or (not token); not a keyword, so it must be an identifier
	      (equal token "."))))
    token))

(defun ada-indent-backward-name ()
  (ada-indent-next-name 'ada-indent-backward-token))

(defun ada-indent-forward-name ()
  (ada-indent-next-name 'ada-indent-forward-token))

;;; refine-*

;; ada-indent-forward-token calls refine-* with point after token;
;; ada-indent-backward with point before token.
;;
;; In addition, refine-* must not move point, unless it is returning a
;; double token (end_return, package_body etc).
;;
;; So each refine defun must take a 'forward' arg, and start with:
;; (save-excursion
;;  (when forward (smie-default-backward-token))
;;
;; If it is returning a double token, the save-excursion must be
;; embedded carefully. It is usually simpler to handle the double
;; tokens in forward/backare-token before calling refine-*

(defun ada-indent-refine-: (forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    ;; ':' occurs in object declarations and extended return statements:
    ;; defining_identifier_list : [aliased] [constant] subtype_indication [:= expression] [aspect_specification];
    ;; defining_identifier : [aliased][constant] return_subtype_indication [:= expression]
    ;;    [do handled_sequence_of_statements end return];
    ;;
    ;; To complex to sort out syntacticly. But 'do' and ';' are
    ;; unique, so we can use search-forward-regexp. We might find
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

(defun ada-indent-refine-is (forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    ;; too many occurances to document them all.

    (or
     ;; First try simple, common constructs.  We don't use
     ;; smie-backward-sexp for these, because it is often too greedy;
     ;; it leads to horrible recursive parsing with wrong guesses,
     ;; and ends up reporting no match.
     (save-excursion
       (let ((token (ada-indent-backward-name)))
	 (cond
	  ((equal token "package") "is-package_declaration")
	   ;; "package" name ^ "is"

	  ((equal token "package_body") "is-package_body")
	  ;; "package" "body" name ^ "is"

	  ;; FIXME: we don't handle private here yet, because that is recursive

	  ((equal token "procedure") "is-subprogram_body")
	  ;; "procedure" name ^ "is"

	  ((equal token "protected_type") "is-type")
	  ;; "protected" identifier ^ "is"

	  ((equal token "protected_body") "is-protected_body")
	  ;; "protected" "body" identifier ^ "is"

	  ((equal token "return-spec") "is-subprogram_body")
	   ;; "function" identifier "return" name ^ "is"

	  ((equal token "type") "is-type")
	  ;; "type" identifier ^ "is"
	  ;; covers "protected type"; that's lexed as the token "type"
	  )))

     ;; now more complicated things
     (save-excursion
       ;; entry body with params: "entry" identifier "("...")" "when" exp "is"
       ;;
       ;; FIXME: if we can be guessing wrong here, we can't use
       ;; smie-backward-sexp (because it will just get confused). So
       ;; far, this is the only possibility at this point, so we don't
       ;; really need to check, but we want to identify missing cases.
       (if (equal "entry" (nth 2 (smie-backward-sexp "is-entry_body"))) "is-entry_body"))

     (error "unrecognized 'is'"))))

(defun ada-indent-refine-return (forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    ;; return occurs in several places;
    ;; 1) a function declaration:
    ;;
    ;;      function identifier (...) return name;
    ;;
    ;;    token: "return-spec"
    ;;
    ;; 2) a function body:
    ;;
    ;;      function identifier (...) return name is
    ;;
    ;;    token: "return-spec"
    ;;
    ;; 3) a return statement:
    ;;
    ;;      return;
    ;;
    ;;    token: "return"
    ;;
    ;;      return exp;
    ;;
    ;;    token: "return-exp"
    ;;
    ;; 4) an extended return statement:
    ;;
    ;;       return identifier : name;
    ;;
    ;;    token: "return" (4a)
    ;;
    ;;       return identifier : name do statements end return;
    ;;
    ;;    token: "return-do" (4b) or "end_return" (4c)
    ;;
    ;; 5) an access function type declaration:
    ;;
    ;;      type name is access [protected] function identifier (...) return name;
    ;;
    ;;    token: "return-access"
    ;;
    ;; So we have to look both forward and backward to resolve this.
    (or
     (save-excursion (if (equal "end" (smie-default-backward-token)) "end_return")); 4c

     ;; do this before now, otherwise can't distinguish between:
     ;; function F1 return Integer;
     ;; return 0;
     ;;
     (save-excursion
	(if (equal "function" (ada-indent-backward-name))
	    (if (or (equal "access" (smie-default-backward-token))
		    (equal "access" (smie-default-backward-token)))
		"return-access"; 5
	      "return-spec"))); 1 or 2

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
	    ((equal token "is") "return-spec"); special case of 2, with name = identifier
	    (t_ "return-exp"); 3b
	    )))))
     ))

(defun ada-indent-refine-subprogram (subprogram forward)
  ;; "procedure" or "function"
  (save-excursion
    (if forward (smie-default-backward-token)); procedure/function
    (if (or (equal "access" (smie-default-backward-token))
	    (equal "access" (smie-default-backward-token)))
	(concat subprogram "-access")
      subprogram)))

;;; forward/backward token

(defun ada-indent-forward-token ()
  ;; This must be a complete inverse of ada-indent-backward-token;
  ;; smie-indent-keyword parses forward, potentially at every keyword
  (let ((token (smie-default-forward-token)))
    (cond
     ((equal token ":") (ada-indent-refine-: t))

     ;; FIXME: not doing "protected-access" yet, because it's recursive

     ((equal token "and") (ada-indent-refine-and t))

     ((equal token "end")
      (let ((token (save-excursion (smie-default-forward-token))))
	(cond
	 ((equal "record" token)
	  (smie-default-forward-token)
	  "end_record")

	 ((equal "return" token)
	  ;; see comment on "end_return" in refine-return above
	  (smie-default-forward-token)
	  "end_return"); 4c

	 (t "end")); all others

     ((equal token "function")
      ;; type identifier is access [protected] function
      (ada-indent-refine-subprogram "function" t))

     ((equal token "is")
      ;; See comment in backward-token for "private-type".
      ;;
      ;; This is recursive; to refine 'private', we call
      ;; backward-token, which puts us right back here.  FIXME: No
      ;; solution for that yet.
      ;;
      ;; (or
      ;;  (when (equal "private" (save-excursion (ada-indent-forward-name t)))
      ;; 	 "private-type"); 1
      (ada-indent-refine-is t))

     ((equal token "null")
      (when (equal token "record") (smie-default-forward-token) "null_record"))))

     ((equal token "procedure")
      ;; type identifier is access [protected] procedure
      (ada-indent-refine-subprogram "procedure" t))

     ((equal token "package")
      (if (save-excursion (equal "access" (smie-default-backward-token)))
	  "package-access"
	(if (equal "body" (save-excursion (smie-default-forward-token)))
	    (progn
	      (smie-default-forward-token)
	      "package_body")
	  "package_type")))

     ((equal token "protected")
      (if (save-excursion (equal "access" (smie-default-backward-token)))
	  "protected-access"
	(if (equal "body" (save-excursion (smie-default-forward-token)))
	    (progn
	      (smie-default-forward-token)
	      "protected_body")
	  ;; We don't check forward for "type" because that would be recursive
	  ;; FIXME: expand syntax for protected, verify this is correct.
	  "protected_type")))

     ((equal token "return") (ada-indent-refine-return t))

     ((equal token "with")
      ;; see comment in backward-token for "with_private"
      (cond
       ((equal "with" (save-excursion (smie-default-forward-token)))
	(smie-default-forward-token)
	"with_private"); 5

       ;;(FIXME: "with-context")

       (t "with")))

     (t token))))

(defun ada-indent-backward-token ()
  (let ((token (smie-default-backward-token)))
    (cond
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

	 (t (error "unrecognized 'body': %s" token)))))

     ((equal token "function")
      ;; type identifier is access [protected] function
      (ada-indent-refine-subprogram "function" nil))

     ((equal token "is") (ada-indent-refine-is nil))

     ((equal token "package")
      ;; FIXME: this is ok for a library level [generic] package alone in
      ;; a file. But it could be a problem for a nested [generic]
      ;; package.
      (if (equal "generic" (smie-backward-sexp "package-generic"))
	  "package-generic"
	"package"))

     ((equal token "private")
      ;; 'private' occurs in:
      ;;
      ;; 1) [formal_]private_type_declaration
      ;;
      ;;   type defining_identifier [discriminant_part] is [[abstract] tagged] [limited] private
      ;;      [aspect_specification];
      ;;
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
	"with_private"); 5

       ((equal "is-type" (save-excursion (ada-indent-backward-name)))
	"private-type"); 1

       (t "private"))); all others

     ((equal token "procedure")
      ;; type identifier is access [protected] procedure
      (ada-indent-refine-subprogram "procedure" nil))

     ((equal token "protected")
      ;; 'protected' occurs in:
      ;; access_definition
      ;; access_to_subprogram_definition
      ;; interface_type_definition
      ;; protected_body
      ;; protected_body_stub
      ;; protected_type_declaration
      ;; single_protected_declaration
      ;;
      ;; We don't have to check for 'body' here; we would have already
      ;; hit that (we never stop on the 'body').
      ;;
      ;; We can get here after stopping on 'procedure'
      (if (save-excursion
	    (equal "access" (smie-default-backward-token)))
	  "protected-access"
	"protected_type"))

     ((equal token "record")
      (let ((token (save-excursion (smie-default-backward-token))))
	(cond
	 ((equal token "end") (smie-default-backward-token) "end_record")
	 ((equal token "null") (smie-default-backward-token) "null_record"))))

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
      (let ((token (ada-indent-backward-name)))
	(cond
	 ((or (equal token "new")
	      (equal token "and-interface_list"))
	  "with"); 1, 5

	 ((or (equal token "private")
	      (equal token ";"))
	  "with-context"); 7 FIXME: test beginning of buffer
	 )))

    (t token))))

;;; indent rules

(defun ada-indent-rule-current (offset)
  "Indent relative to the current line"
  (cons 'column (+ (save-excursion (back-to-indentation) (current-column)) offset)))

(defun ada-indent-rules (method arg)
  (case method
    (:elem
     (case arg
       (basic ada-indent)
       (args 0))
     )
    (:before
     (cond
       ((or
	 (equal arg "access")
	 (equal arg "array"))
	(smie-rule-parent ada-indent))

       ((or
	 (equal arg "end")
	 (equal arg "generic")
	 (equal arg "with")) ; context clause; FIXME: also used in derived record declaration
	(smie-rule-parent 0))

       ((equal arg "function-access")
	;; We are in an access_to_subprogram type_definition; we
	;; want to indent 'function' relative to 'type'
	(smie-rule-parent ada-indent))
       ))
    (:after
     (or
      (cond
	((equal arg ";") 0)

	((equal arg "do") ada-indent)

	((equal arg "procedure-access")
	 ;; always indent the parameter list relative to the line 'procedure' is on, not to a parent token
	 (ada-indent-rule-current ada-indent))

	((or
	  (equal arg "is-package_body")
	  (equal arg "is-package_declaration")
	  (equal arg "is-protected_body")
	  (equal arg "is-subprogram_body"))
	 ;; indent relative to the start of the declaration or body,
	 ;; which is the parent of 'is'.
	 (smie-rule-parent ada-indent))
	)

      (if (smie-indent--hanging-p) ada-indent)
      ))))

;;; setup

(defun ada-indent-setup ()
   ;; smie-indent-comment doesn't do what we want, but
   ;; smie-indent-after-keyword does what we want for comments; it
   ;; indents the comment as a simple single-line statement.
  (set (make-local-variable 'smie-indent-functions)
       (remove 'smie-indent-comment smie-indent-functions))

  (smie-setup ada-indent-grammar #'ada-indent-rules
	      :forward-token #'ada-indent-forward-token
	      :backward-token #'ada-indent-backward-token))

(add-hook 'ada-mode-hook 'ada-indent-setup)

(define-key ada-mode-map "\t" 'indent-for-tab-command)
;; TAB will now use smie indentation in Ada mode buffers

(provide 'ada-indent)

;;; end of file
