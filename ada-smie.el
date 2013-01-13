;;; Ada mode indentation engine, based on SMIE
;;
;; [1] ISO/IEC 8652:201z (draft 18); Ada 2012 reference manual
;;
;; Copyright (C) 2012, 2013  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
;; Contributors: Simon Wright <simon@pushface.org>
;;
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
;; not using lexical-binding because we support Emacs 23
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

;;;;; code

(require 'ada-indent-user-options)
(eval-when-compile (require 'cl)); 'case'
(require 'smie)

;; all indentation-related user variables are in ada-indent-user-options.el

;;;; SMIE grammar

(defconst ada-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '(;; non-terminal syntax terms not otherwise expanded
      (identifier)

      ;; BNF from [1] appendix P, vastly simplified
      ;; (info "(aarm2012)Annex P" "*info Annex P*")
      ;;
      ;; We only need enough of the grammar to allow indentation to
      ;; work; see (info "(elisp)SMIE Grammar")
      ;;
      ;; The important operation in indentation is finding the
      ;; beginning of the current Ada statement or declaration.
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
      ;; first keyword.
      ;;
      ;; The primary work in building the grammar is finding ways to
      ;; avoid conflicts without breaking the ability to find the
      ;; first keyword. One approach is to leave out as many keywords
      ;; as possible, another is to refine Ada keywords into several
      ;; different smie keywords.
      ;;
      ;; SMIE automatically allows balanced parens anywhere, so we
      ;; don't need to declare argument lists or discriminant lists
      ;; in the grammar.
      ;;
      ;; Ada keywords that are not needed to find first keywords, or
      ;; otherwise help in indentation, do not need to be present in
      ;; the grammar; they will be treated as identifiers. In
      ;; particular, no operators are in the grammar.
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
      ;; it as the separator for each of those constructs.
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
      ;; SMIE allows any non-terminal to be empty, or to repeat. So
      ;; 'identifier' can cover several non-keywords.

      ;; We list the non-terminals in alphabetical order, since there
      ;; isn't any more reasonable order. We use the same names as [1]
      ;; Annex P as much as possible.
      ;;
      ;; Refined token naming convention:
      ;;
      ;; If an Ada keyword is refined, all occurances of the keyword
      ;; in the smie grammar must be refined. Use "-other" if no
      ;; better name is available. That way it is clear when a keyword
      ;; is being left as an indentifier.

      ;; abstract_subprogram_declaration
      ;; "is" "abstract" are identifiers

      (accept_statement
       ("accept-open" identifier "do" statements "end-block"))
       ;; "accept identifier;" is left as an identifier; otherwise it
       ;; is an opener without a matching closer

      ;; (allocator
      ;;  ("new" expression))
      ;; "new" is left as identifier, so it doesn't need to be in the grammar

      (aspect_list
       (name)
       (aspect_list "," name))

      (association_list
       (expression)
       (association_list "," expression))
      ;; We need aspect_list, association_list in the grammar, because
      ;; "," affects indentation. We don't need 'aspect_item' or
      ;; 'association'; => does not affect indentation in those.
      ;; 'association_list' does not appear in any other non-terminal;
      ;; we only see it inside parens, which is handled separately
      ;; from the rest of the grammar. Similarly, we don't need "in",
      ;; "out", "access" for parameter lists; "in", "access" are
      ;; keywords for other statements, but that doesn't interfere
      ;; with indenting parameter lists.

      (asynchronous_select
       ("select-open" statements "then-select" "abort-select" statements "end-select" "select-end"))

      (attribute_definition_clause
       ("for-attribute" name "use-attribute"); ie Foo'Address
       ("for-attribute" name "use-record" "record-type" declarations "end-record" "record-end"))
      ;; we need "for-attribute" to be distinct from "for-loop", so we
      ;; can distinguish between declaration and statement for
      ;; refine-begin.

      (block_statement
       ;; see loop_statement for discussion of block label
       ("declare" declarations "begin-body" statements "exception-block"
	"when-case" object_declaration "=>-when" statements "end-block")
       ("declare" declarations "begin-body" statements "exception-block"
	"when-case" identifier "=>-when" statements "end-block"); "|" is an identifier
       ("declare" declarations "begin-body" statements "end-block")
       ("begin-open" statements "end-block"))
      ;; we don't need to repeat the optional exception handler in the
      ;; other cases, nor "|"; once is enough to establish the
      ;; precedence of "exception-block".  no need to distinguish
      ;; between "when-case" and "when-exception"

      (case_statement
       ("case" name "is-case" "when-case" identifier "|" identifier "=>-when" statements "end-case" "case-end")
       ("case" name "is-case" "when-case" identifier "=>-when" statements "end-case" "case-end")
       ("case" name "is-case" "end-case" "case-end"))
      ;; "when =>" is optional and therefore repeatable.
      ;; "|" is optional and therefore repeatable.
      ;;
      ;; "=>" is refined, because it appears elsewere in the syntax
      ;; (associations, aspects), although not in the grammar, and the
      ;; indentation is different here.

      (context_clause
       (context_item)
       (context_item ";" context_item))

      (context_item
       ("limited-context" "private-context-2" "with-context-2")
       ("limited-context" "with-context-2")
       ("private-context-1" "with-context-2")
       ("with-context-1")
       ("use-decl" name))
      ;; we cannot easily distinguish between a use_clause in a
      ;; context item, and a use_clause in a body.

      (declaration
       (attribute_definition_clause)
       (entry_body)
       (exception_declaration)
       (formal_object_declaration)
       (formal_package_declaration)
       (formal_subprogram_declaration)
       (generic_instantiation)
       (generic_package_declaration)
       (generic_subprogram_declaration)
       (object_declaration)
       (package_body)
       (package_renaming_declaration)
       (package_specification)
       (protected_body)
       (subprogram_declaration)
       (subprogram_body)
       (subtype_declaration)
       (task_body)
       (type_declaration)
       (use_clause)
       (use_type_clause)
       )

      (declarations
       (declaration)
       (declaration ";" declaration))

      (entry_body
       ("entry" identifier "when-entry" expression "is-entry_body" declarations "begin-body" statements "end-block"))

      (exception_declaration
       (identifer ":-object" "exception-declare"))
      ;; covers exception renaming; "renames" is an identifier

      (exit_statement
       ("exit-other"); leaving identifier out
       ("exit-when" "when-exit" expression))

      (expression
       ;; We don't need operators at all in the grammar; they do not
       ;; affect indentation or navigation.
       (name)
       (aggregate)
       (name "with-agg" name))
      ;; The actual syntax for extension_aggregate is more complex,
      ;; but all we really need is for "with-agg" to be in the
      ;; grammar.

      ;; Formal generic parameters. Most formal_* are covered in this
      ;; grammar by the equivalent non-formal syntax.

      (formal_object_declaration
       (identifier ":-object" "in" name); leaving "out not null" as identifiers
       (identifier ":-object" name)
       (identifier ":-object" name ":=" expression))

      (formal_package_declaration
       ;; with package defining_identifier is new generic_package_name
       ;;    formal_package_actual_part [aspect_specification];
       ("with-formal" "package-formal" identifier "new-formal" name))
      ;; leave "is" an identifier. name after "new-formal" so it is
      ;; not a closer. formal_package_actual_part is an association
      ;; list
      ;; FIXME (later): aspects not implemented yet

      (formal_subprogram_declaration
       ;; leaving "with" "function" "procedure" unrefined gives
       ;; conflicts with the non-formal use.

       ("with-formal" "function-formal" name "return-formal" name); trailing name same as non-formal
       ("with-formal" "procedure-formal" name); trailing name same as non-formal
       ;; We leave out [is name]; "is" is an identifier here.
       )

      (generic_instantation
       ("package-inst" name "new-inst" name)
       ("procedure-inst" name "new-inst" name)
       ("function-inst" name "new-inst" name))
      ;; Leaving out generic_formal_part. Leaving "is" as an
      ;; identifier.

      (generic_package_declaration
       ;; No need to distinguish between 'declarations' and
       ;; 'generic_formal_parameter_declaration' in the grammar.
       ("generic" declarations
	"package-generic" identifier "is-package" declarations "private-spec" declarations "end-block")
       ("generic" declarations
	"package-generic" identifier "is-package" declarations "end-block"))

      (generic_subprogram_declaration
       ("generic" declarations "function-generic" name "return-spec" name)
       ("generic" declarations "procedure-generic" name))

      (interface_list
       ;; The Ada grammar sometimes has "name and interface_list".
       ;; We can't (and don't need to) distinguish that from "interface_list"
       (name)
       (interface_list "and-interface_list" name))

      (loop_statement
       ;; we treat 'identifier :' as a separate statement, here and
       ;; for blocks, because it is indented differently; we don't
       ;; want it to be the statement-start. It is simplest if we
       ;; leave ":-label" out of the grammar, but still refine it, so
       ;; ada-smie-label can handle it.

       ("for-loop" identifier "in" name "loop-body" statements "end-loop" "loop-end")
       ("for-loop" identifier "of-loop" name "loop-body" statements "end-loop" "loop-end")
       ("for-loop" identifier ":-loop" name "of-loop" name "loop-body" statements "end-loop" "loop-end")
       ("while" expression "loop-body" statements "end-loop" "loop-end")
       ("loop-open" statements "end-loop" "loop-end")
       ;; "reverse" is an identifer
       ;; Splitting out iteration_scheme makes it look optional, which it's not.
       )

      (name
       (identifier)
       ;; selected_component is covered by the SMIE rule that any
       ;; non-terminal can repeat. '.' is an identifier.
       ;;
       ;; Paired parenthesis are simply skipped by SMIE. So we
       ;; don't need to represent subprogram parameter lists, or array
       ;; indices here (and also no aggregates in 'expression').
       ;;
       ;; We could just use 'identifier' everywhere instead of 'name',
       ;; but this is less confusing.
       )

      (object_declaration
       (identifier ":-object" name); same as ":-object" in extended return.
       ;; Covers object_renaming_declaration; "renames" is an identifier
       (identifier ":-object" name ":=" expression); same as ":-object" in extended return
       (identifier ":-object" name "of-object" name); anonymous array
       (identifier ":-object" name "of-object" name ":=" expression)
       )

      (package_body
       ;; we leave "body" as an identifier. "private" before "package"
       ;; is treated as a separate statement, to avoid further
       ;; refining "package".
       ("private-library")
       ("package-plain" name "is-package" declarations "begin-body" statements "end-block")
       ("package-plain" name "is-package" "separate-stub")
       ("separate-unit" "package-separate" name "is-package" declarations "begin-body" statements "end-block"))

      (package_renaming_declaration
       ("package-renames"))
      ;; covers generic_renaming_declaration

      (package_specification
       ("package-plain" name "is-package" declarations "private-spec" declarations "end-block")
       ("package-plain" name "is-package" declarations "end-block"))

      (protected_body
       ("protected-body" identifier "is-protected_body" declarations "end-block")
       ("protected-body" name "is-protected-body" "separate-stub")
       ("separate-unit" "protected-separate" identifier "is-protected_body" declarations "end-block"))

      (raise_statement
       ;; "raise;" is left as an identifier
       ("raise-stmt" name "with-raise"))

      (select_statement
       ;; accept_statement, delay_statement are covered here by
       ;; "statements". "terminate" looks like a procedure call, so
       ;; we leave it as an identifier.
       ("select-open" "when-select" expression "=>-when" statements
	"or-select" "when-select" expression "=>-when" statements
	"else-other" statements "end-select" "select-end")

       ("select-open" statements
	"or-select" statements
	"else-other" statements "end-select" "select-end"))

      (statement
       (expression); covers procedure calls

       ;; assignment_statement
       (name ":=" expression)

       ;; "abort" is an indentifier, except in asynchronous-select

       (accept_statement)
       (asynchronous_select)
       (block_statement)
       (case_statement)

       ;; delay_statement
       ("delay" expression)

       (exit_statement)

       ;; if_statement
       ("if-open" expression "then-if" statements "end-if" "if-end")
       ("if-open" expression "then-if" statements "else-other" statements "end-if" "if-end")
       ("if-open" expression "then-if" statements
	"elsif" expression "then-if" statements
	"else-other" statements "end-if" "if-end")
       ;; "then-if .. elsif" is optional here, so SMIE allows it to
       ;; repeat. It also allows "else-other" to repeat, but we don't
       ;; care.
       ;;
       ;; "then" also occurs in "and then" logical operator. We leave
       ;; that as an identifier, so this is "then-if", even though
       ;; there is no other "then" in the grammar.  "else" also occurs
       ;; in "or else" and select statements; "or else" is an
       ;; identifier.

       ;; label_statement
       ("<<" identifier ">>")
       ;; In Ada, this is _not_ followed by ";"; it labels the
       ;; following statement. This grammar doesn't care;
       ;; `ada-smie-goto-statement-start' from the terminal ";" of a
       ;; labeled statement will stop at the start of the label.

       (loop_statement)

       (raise_statement)

       ;; return exp; is left as an identifier

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
       ("function-spec" name "return-spec" name "is-subprogram_body" declarations "begin-body" statements "end-block")
       ("overriding" "function-overriding" name "return-spec" name "is-subprogram_body" declarations "begin-body"
	statements "end-block")
       ("function-spec" name "return-spec" "is-subprogram_body" "separate-stub")
       ("procedure-spec" name "is-subprogram_body" declarations "begin-body" statements "end-block")
       ("overriding" "procedure-overriding" name "is-subprogram_body" declarations "begin-body" statements "end-block")
       ("procedure-spec" name "is-subprogram_body" "separate-stub")
       ("separate-unit" "function-separate" name "return-spec" name "is-subprogram_body" declarations "begin-body"
	statements "end-block")
       ("separate-unit" "procedure-separate" name "is-subprogram_body" declarations "begin-body"
	statements "end-block"))

      (subprogram_declaration
       ("function-spec" name "return-spec" name)
       ;; Trailing name makes "return-spec" have the same binding as
       ;; in subprogram_body; that avoids recursion between refine-is
       ;; and refine-return.

       ("procedure-spec" name); same as 'procedure name is-subprogram_body'
       ;; We leave out ("procedure" name "is" "null") here; we
       ;; are treating a couple of occurences of "is", and most
       ;; occurences of "null", as identifiers.

       ("overriding" "function-overriding" name "return-spec" name)
       ("overriding" "procedure-overriding" name)
       ;; We need "overriding" as a keyword, because the indentation
       ;; policy for it is an exception to the hanging policy:
       ;;
       ;;    overriding
       ;;    procedure (...);

       ;; subprogram_renaming_declaration
       ("function-spec" name "return-spec" name "renames-subprogram" name)
       ("procedure-spec" name "renames-subprogram" name)
       ;; We need "renames" in the grammar because there is an
       ;; indentation option for it.  No need to repeat the optional
       ;; renames for "overriding" cases; one occurance establishes
       ;; the precendence in the grammar.
       )

      (subtype_declaration
       ("subtype" identifier "is-subtype" name))

      (task_body
       ("task-body" identifier "is-task_body" declarations "begin-body" statements "end-block")
       ("task-body" identifier "is-task_body" "separate-stub")
       ("separate-unit" "task-separate" identifier "is-task_body" declarations "begin-body"
	statements "end-block"))

      (type_declaration
       ;; access_type_definition
       ("type-other" identifier "is-type-access")
       ;; Any occurance of "is-type" as the last keyword in a
       ;; declaration must be further refined; otherwise it is
       ;; ambiguous with several other declarations.

       ;; We don't include access-to-subprogram in the grammar,
       ;; because we want to indent relative to 'function' and
       ;; 'procedure' in these types. So we allow them to be first
       ;; keywords. This also greatly simplifies refine-is.

       ;; array_type_definition; we leave "array" as an identifier
       ("type-other" identifier "is-type" name "of-type" name); same as anonymous array

       ;; derived_type_declaration
       ("type-other" identifier "is-type" "new-type" name); same as below
       ("type-other" identifier "is-type" "new-type" name "with-new" "private-with")
       ("type-other" identifier "is-type" "new-type" name "with-new" "record-null"); "null" is an identifier
       ("type-other" identifier "is-type" "new-type" name "with-new" "record-type" declarations "end-record"
	"record-end")

       ;; enumeration_type_definition
       ("type-other" identifier "is-type-enumeration")
       ;; enumeration literals are an aggregate, which is ignored.

       ;; {ordinary_ | decimal_} fixed_point_definition
       ;; "delta" and "digits" are left as identifiers
       ;; floating_point_definition, integer_type_definition; "range" is an identifier
       ("type-other" identifier "is-type-numeric")

       ;; incomplete_type_declaration ::= type defining_identifier [discriminant_part] [is tagged];
       ;;
       ;; We don't need the variant without "is tagged", since it has
       ;; only one keyword.  We don't need "tagged" as a keyword.
       ("type-other" identifier "is-type-tagged")

       ;; interface_type_definition, formal_interface_type_definition
       ("type-other" identifier "is-type" "interface-plain")
       ("type-other" identifier "is-type" "interface-and" "and-interface" interface_list)

       ;; modular_type_definition
       ("type-other" identifier "is-type" "mod-type")

       ;; private_extension_declaration
       ("type-other" identifier "is-type" "new-type" name "with-new" "private-with")
       ("type-other" identifier "is-type" "new-type" interface_list "with-new" "private-with")
       ;; leaving 'with' and 'private' as separate tokens causes conflicts

       ;; private_type_declaration
       ("type-other" identifier "is-type" "private-type-spec")

       ;; protected_type_declaration, single_protected_declaration
       ;;
       ;; single_protected_declaration just leaves out
       ;; "type-protected"; we don't need that in the grammar, things
       ;; work fine without it.
       ("protected-type" "type-protected" identifier "is-type-block" declarations
	"private-spec" declarations "end-block")
       ("protected-type" "type-protected" identifier "is-type-block" declarations "end-block")
       ("protected-type" "type-protected" identifier "is-type" "new-type" interface_list "with-new" declarations
	"private-spec" declarations "end-block")

       ;; record_type_definition
       ("type-other" identifier "is-type" "record-null")
       ("type-other" identifier "is-type" "record-type" declarations "end-record" "record-end")
       ("type-other" identifier "is-type" "record-type" "record-type" declarations "end-record"
	"record-end-aspect" "with-aspect" aspect_list)
       ;; No need to distinguish between 'declarations' and
       ;; 'component_list'.

       ;; task_type_declaration, single_task_declaration: we need
       ;; "task" in the grammar to classify "task name;" as a
       ;; declaration, not a procedure call statement. Task entries
       ;; can have entry families, but that's a parenthesized
       ;; expression, so we don't need it in the grammar.
       ("task-single" name)
       ("task-type" "type-task" identifier "is-type-block" declarations "end-block")

       ); type_declaration

      (use_clause
       ("use-decl" name))

      (use_type_clause
       ("use-decl" "type-use" name))
      ;; "all" is left as an identifier

      )); smie-bnf->prec2
    ))

;;;; utils for refine-*, forward/backward token

(defconst ada-smie-block-keywords
  '("=>-when"
    "abort-select"
    "begin-body"
    "begin-open"
    "declare"
    "do"
    "else-other"
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
    "or-select"
    "package-plain"
    "private-spec"
    "record-type"
    "select-open"
    "then-select"
    "then-if")
  ;; We don't split this into start and end lists, because most are
  ;; both. The keywords that are an end but never a start are in
  ;; ada-smie-block-end-keywords. Being a start but never end is not
  ;; a problem.
  ;;
  ;; This is not a subset of the open portion of the grammar
  ;; open/close list; that is restricted to keywords that don't bind
  ;; on the left.
  "Keywords that start or end indented blocks, excluding keywords that always end blocks.")

(defconst ada-smie-block-end-keywords
  '("elsif"
    "end-case"
    "end-block"
    "end-if"
    "end-loop"
    "end-record"
    "end-return"
    "end-select"
    "function-generic"
    "or-select"
    "package-generic"
    "procedure-generic"
    "when-case"
    "when-select"
    )
  "Keywords that always end indented blocks.")

(defconst ada-smie-pre-begin-tokens
  '("declare"
    "is-subprogram_body"
    "is-package"
    "is-task_body"
    "is-entry_body")
  ;; found by searching [1] Annex P for "begin", then checking for
  ;; refinements. Thus in Annex P order.
  "All refined tokens that are followed by \"begin\" in an Ada declaration.")

(defconst ada-smie-labeled-unrefined-keywords
  '("begin"
    "declare"
    "for"
    "loop"
    "while")
  "Unrefined keywords that can be preceded by a label.")

(defun ada-smie-matching-end (keyword)
  "Return a list of keywords that could end the block started by KEYWORD.
This is found by searching the grammar; it will produce results
that are not allowed by Ada, but we don't care."
  ;; IMPROVEME: change to take the block start keyword and the current
  ;; keyword, and return t/nil
  (let ((prec (nth 2 (assoc keyword ada-smie-grammar)))
	(list (cdr ada-smie-grammar))
	(found nil)
	toklevels)
    (while (setq toklevels (pop list))
      (if (and
	   (integerp (nth 1 toklevels))
	   (= prec (nth 1 toklevels)))
	  (setq found (cons (nth 0 toklevels) found))
	))
    found))

(defun ada-smie-error (message)
  (ada-smie-invalidate-cache)
  (error
   "%s:%d: %s: %s"
   (buffer-file-name)
   (line-number-at-pos)
   message
   (buffer-substring-no-properties (point-at-bol) (point-at-eol))))

(defun ada-smie-generic-p ()
  "Assuming point is at the start of a package or subprogram
spec, return t if is a generic, nil otherwise."
  (save-excursion
    ;; Scan back over things that might be generic formal
    ;; parameters. If we find a definite formal param (has -formal in
    ;; the refined keyword), we're done. If we find "generic", we're
    ;; done. If we find something else that can't be a formal
    ;; parameter (ie package start), we're done.
    (let ((token (ada-smie-backward-token))
	  stmt-or-decl
	  (result 'not-found)
	  (first t))
      (while (eq result 'not-found)
	(cond
	 ((equal token ""); bob
	  (setq result nil))

	 ((member token '("not" ";")); "not overriding ..."
	  (if first
	      (progn
		(setq first nil)
		;; try again
		(ada-smie-goto-statement-start token)
		(setq token (ada-smie-backward-token)))

	    ;; we've skipped a statement or declaration; see if we can tell which
	    (save-excursion
	      (smie-default-forward-token)
	      (setq stmt-or-decl (ada-smie-statement-or-decl)))

	    (ecase (car stmt-or-decl)
	      (statement
	       (ada-smie-error "found statement preceding package or subprogram spec"))

	      (formal
	       (setq result t))

	      ((declaration context)
	       ;; There are only two non-formal declarations that can
	       ;; occur in a generic formal parameter list;
	       ;; formal_object_declaration, formal_type_declaration.
	       (if (not (member (cadr stmt-or-decl) '(":-object" "type-other")))
		   (setq result nil)
		 ;; try again
		(ada-smie-goto-statement-start token)
		(setq token (ada-smie-backward-token))))

	      (unknown
	       ;; try again
	       (ada-smie-goto-statement-start token)
	       (setq token (ada-smie-backward-token)))
	      )))

	 ((equal token "private-library")
	  (setq result nil))

	 ((equal token "generic")
	  (setq result t))

	 ((member token ada-smie-block-keywords)
	  (setq result nil))

	 (t
	  (if ada-smie-debug
	      (ada-smie-error "ada-smie-generic-p: unexpected statement or prev keyword")
	    ;; user is probably editing code
	    (setq result nil)))

	 ));; while
      result
      )))

(defun ada-smie-library-p ()
  "Assuming point is before \"private\" at the start of a package
or subprogram spec, return t if is a library_item, nil
otherwise."
  (save-excursion
    ;; Scan back over things that might precede a library item;
    ;; "private", context_clause, pragmas.
    (let ((token (ada-smie-backward-keyword))
	  stmt-or-decl
	  (result 'not-found)
	  (first t))
      (while (eq result 'not-found)
	(cond
	 ((equal token nil); bob
	  (setq result t))

	 ((equal token ";")
	  (if first
	      (progn
		(setq first nil)
		;; try again
		(ada-smie-goto-statement-start token)
		(setq token (ada-smie-backward-keyword)))

	    ;; we've skipped a statement or declaration; see if we can tell which
	    (save-excursion
	      (smie-default-forward-token)
	      (setq stmt-or-decl (ada-smie-statement-or-decl)))

	    (ecase (car stmt-or-decl)
	      (statement
	       (ada-smie-error "found statement preceding package or subprogram spec"))

	      (declaration (setq result nil))

	      (context (setq result t))

	      ((unknown formal)
	       ;; try again
	       (ada-smie-goto-statement-start token)
	       (setq token (ada-smie-backward-keyword)))
	      )))

	 ((member token ada-smie-block-keywords)
	  (setq result nil))

	 (t
	  (if ada-smie-debug
	      (ada-smie-error "ada-smie-library-p: unexpected statement or prev keyword")
	    ;; user is probably editing code
	    (setq result nil)))

	 ));; while
      result
      )))

(defun ada-smie-statement-or-decl ()
  "Assuming point is at the start of a statement, a normal
declaration, or a generic formal declaration, examine a few
refined tokens following point to see if we can determine which.
Return (class token), where `class' is 'statement, 'declaration,
'formal, 'context, or 'unknown; `token' is the determining token
or nil.  Preserves point."
  (save-excursion
    (catch 'quit
      (let ((token (ada-smie-forward-token)))
	;; check for special cases
	(when (not (ada-smie-keyword-p token))
	  (cond
	   ((equal token "pragma")
	    ;; not worth making this a keyword. can occur anywhere, so
	    ;; it doesn't tell us anything here.
	    nil)
	   (t (setq token (ada-smie-forward-keyword)))))

	;; lists compiled by going thru (statement ..) and (declaration
	;; ...) in the ada-smie-grammar declaration above.

	;; token = ";" indicates a procedure call; there are no
	;; keyword-less declarations except "pragma", which we handle
	;; above.
	(cond
	 ((member
	   token
	   '(";" ":=" "accept-open" ":-label" "declare" "begin-open" "case" "delay" "exit-other"
	     "exit-when" "if-open" "<<" "for-loop" "while" "loop-open" "return-ext" "select-open"))
	  (list 'statement token))

	 ((member
	   token
	   '("for-attribute" "entry" ":-object" "generic" ":-object" "package-plain" "package-renames"
	     "protected-body" "function-spec" "overriding" "procedure-spec" "protected-type" "subtype" "subtype"
	     "task-type" "type-other" "use-attribute" "use-decl"))
	      ;; use-decl is both a context clause and a declaration
	  (list 'declaration token))

	 ((member
	   token
	   '("limited-context" "private-context" "with-context-1" "with-context-2"))
	  (list 'context token))

	 ((equal token "with-formal")
	  (list 'formal token))

	 (t (list 'unknown nil)))))))

;;;; multi-token movement

(defun ada-smie-skip-lowlevel-sexp (forward)
  ;; While refining tokens, we don't want to call smie-next-sexp,
  ;; because it relies on refined tokens. So we call the C scanner
  ;; directly when we need to skip a parenthesis or string (see the
  ;; lisp source for forward-sexp).
  (let ((forward-sexp-function nil))
    (condition-case err
	(if forward
	    (forward-sexp)
	  (backward-sexp))
      (scan-error
	 ;; unbalanced parens can happen when user is typing code;
	 ;; we'll get here from newline-and-indent calling validate-cache.
       (if forward
	   ;; Point is at open paren; caller wants us to move to close
	   ;; paren. Best we can do is get past this paren, to let
	   ;; validate-cache process the following tokens.
	   (forward-char 1)
	 ;; not clear when the reverse happens, but a similar argument applies
	 (forward-char -1)))
      )))

(defun ada-smie-forward-name ()
  "Move forward across whitespace, then tokens that can be part of a selected Ada name.
Return the name as a string."
  (skip-syntax-forward " ")
  (let ((begin (point))
	token
	(done nil))
    (while (not done)
      (setq token (ada-smie-forward-token))
      (cond
       ((string= "" token)
	;; hit a paren or eob
	(skip-syntax-backward " >"); whitespace and comment end, which is newline
	(setq done t))

       ((equal token ".")
	nil)

       ((ada-smie-keyword-p token)
	(ada-smie-backward-token)
	(setq done t))

       (t nil)
       ))
    (skip-syntax-backward " ")
    (buffer-substring-no-properties begin (point))))

(defun ada-smie-skip-identifier-list (forward)
  "Skip forward/backward over {identifier , |}.
Return (preceding-pos preceding-string) for backward, succeeding
for forward."
  (let (parent parent-pos)
    (while
	(progn
	  (setq parent (if forward (ada-smie-forward-keyword) (ada-smie-backward-keyword)))
	  (setq parent-pos (point))
	  (cond
	   ((equal "," parent) t)
	   ((equal "|" parent) t)
	   ((equal "(" parent) (forward-char 1) nil)
	   ((equal ")" parent) (backward-char 1) nil)
	   (t
	    (if forward (smie-default-backward-token) (smie-default-forward-token))
	    nil))))
    (forward-comment (if forward (- (point)) (point-max)))
    (list parent-pos parent)))

(defun ada-smie-token-special (forward)
  "Assuming smie-default-{forward|backward}-token has returned \"\",
return one of \"(\", \")\", \"\"\", or nil for bob, eob."
  (if forward
      (cond
       ((eq (char-after) ?\)) ")")
       ((eq (char-after) ?\() "(")
       ((eq (char-after) ?\") "\""))
    ;; backward
    (cond
     ((eq (char-before) ?\)) ")")
     ((eq (char-before) ?\() "(")
     ((eq (char-before) ?\") "\""))))

(defun ada-smie-next-keyword (next-token forward)
  "Skip tokens using function NEXT-TOKEN, until a keyword (a
token defined in the grammar) or unpaired parenthesis is found.
Skips string literals, character literals, paired parentheses.
Stops at left paren going backwards, right paren going forwards.
Return the grammar entry for the keyword (which may be the first
token found), or (paren nil nil); point is beyond the
keyword (after for `forward' t, before otherwise).  Return nil if
encounter beginning or end of buffer."
  (let (token
	tok-levels)
    (catch 'quit
      (while
	  (progn
	    (setq token (funcall next-token))
	    (if (equal "" token)
		;; We hit a paren, string, character literal, bob, eob
		(progn
		  (cond
		   (forward
		    (when (eobp) (throw 'quit nil))
		    (when (eq (char-after) ?\))
		      (forward-char 1)
		      (throw 'quit '(")" nil nil)))
		    )

		   (t ;; backward
		    (when (bobp) (throw 'quit nil))
		    (when (eq (char-before) ?\()
		      (backward-char 1)
		      (throw 'quit '("(" nil nil))))
		   )

		  (ada-smie-skip-lowlevel-sexp forward)
		  ;; the next token might be another paren, so we loop
		  t)

	      ;; a token
	      (setq tok-levels (assoc token ada-smie-grammar))
	      (not tok-levels); not a keyword
	      )))
      tok-levels)
    ))

(defun ada-smie-backward-keyword ()
  (nth 0 (ada-smie-next-keyword 'ada-smie-backward-token nil)))

(defun ada-smie-forward-keyword ()
   (nth 0 (ada-smie-next-keyword 'ada-smie-forward-token t)))

(defun ada-smie-next-token-unrefined (next-token forward)
  "Move to the next token using function NEXT-TOKEN. Skips parentheses, strings.
Return the token (case preserved), or wrong paren, or empty string if encounter
beginning or end of buffer."
  (let ((token nil))
    (while (not token)
      (setq token (funcall next-token))
      (if (equal "" token)
	  ;; We hit a parenthesis, bob, eob, string, char literal
	  (cond
	   ((bobp) (setq token ""))
	   ((eobp) (setq token ""))
	   ((and forward (eq (char-after) ?\))) (setq token ")"))
	   ((and (not forward) (eq (char-before) ?\()) (setq token "("))
	   (t
	    (setq token nil)
	    (ada-smie-skip-lowlevel-sexp forward)); also skips strings, char literals
	   )))
    token))

(defun ada-smie-backward-token-unrefined ()
  (ada-smie-next-token-unrefined 'smie-default-backward-token nil))

(defun ada-smie-forward-token-unrefined ()
  (ada-smie-next-token-unrefined 'smie-default-forward-token nil))

(defun ada-smie-backward-tokens-unrefined (&rest targets)
  "Move backward over unrefined tokens, strings and parens. Stop
when found token is an element of TARGETS, return that
token. Empty string (for bob) is implicitly added to TARGETS, to
avoid infinite loop on illegal code."
  (let (result)
    (while (not (member (setq result (ada-smie-backward-token-unrefined))
			(cons "" targets))))
    result))

(defun ada-smie-forward-tokens-unrefined (&rest targets)
  "Move forward over unrefined tokens, strings and parens. Stop
when found token is an element of TARGETS, return that
token. Also stops before unmatched right paren, empty string (for
eob). "
  (let (result
	(targets (cons ")" (cons "" targets))))
    (while (not (member (setq result (ada-smie-next-token-unrefined 'smie-default-forward-token t))
			targets)))
    result))

(defconst ada-smie-type-modifiers '("abstract" "tagged" "limited" "synchronized"))

(defun ada-smie-forward-type-modifiers ()
  "Skip forward tokens that are in `ada-smie-type-modifiers', return the following token."
  (let (result)
    (while (member (setq result (smie-default-forward-token)) ada-smie-type-modifiers))
    result))

(defun ada-smie-backward-type-modifiers ()
  "Skip backward unrefined tokens that are in `ada-smie-type-modifiers', return the preceding token."
  (let (result)
    (while (member (setq result (smie-default-backward-token)) ada-smie-type-modifiers))
    result))

(defun ada-smie-refine-error (token)
  (if ada-smie-debug
      (ada-smie-error (concat "unrecognized '" token "'"))
    ;; else return the unrefined keyword. Indentation will be wrong,
    ;; but this is more friendly to the user.
    token))

;;;; refine-*

;; ada-smie-forward-token calls refine-* with point
;; after token; ada-smie-backward with point before token.
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

(defun ada-smie-refine-: (token forward)
  (let ((token (save-excursion
		 (when (not forward) (smie-default-forward-token))
		 (ada-smie-unrefined-token t))))
    (if (member token ada-smie-labeled-unrefined-keywords)
	":-label"
      ;; check for for loop
      (cond
       ((save-excursion
	  (smie-default-backward-token); ":"
	  (ada-smie-backward-token); identifier
	  (equal "for-loop" (ada-smie-backward-token)))
	":-loop")
      (t ":-object")
      ))))

(defun ada-smie-refine-=> (token forward)
  (let ((token (save-excursion
		 (when forward (smie-default-backward-token))
		 (ada-smie-backward-keyword))))

    (cond
     ((member token '("(" "|" ","))
      ;; case or aggregate discrete choice
      (if (ada-in-paren-p)
	  "=>"; identifier in aggregates
	"=>-when"))

     ((member token '(":-object" ; in exception handler
		      "when-case"
		      "when-select"))
      "=>-when")

     (t	"=>-other"))))

(defun ada-smie-refine-abort (token forward)
  (let ((token (save-excursion
		 (when forward (smie-default-backward-token))
		 (ada-smie-backward-token))))

    (if (equal token "then-select")
	"abort-select"
      "abort")))

(defun ada-smie-refine-accept (token forward)
  (let ((token (save-excursion
		 (when (not forward) (smie-default-forward-token))
		 (ada-smie-forward-keyword);; identifier, entry family, parameters; returns ";", "do"
		 )))
    (cond
     ((equal token "do") "accept-open")
     ((equal token ";")  "accept"); identifier
     (t (ada-smie-refine-error "accept")))))

(defun ada-smie-refine-and (token forward)
  ;; 'and' occurs in interface types and logical expressions
  ;; (search for interface_list in [1] annex P):
  ;;
  ;; 1) [formal_]derived_type_definition ::=
  ;;    type defining_identifier [discriminant_part] is
  ;;       [abstract] [limited] new parent_subtype_indication
  ;;       [[and interface_list] record_extension_part]
  ;;       [aspect_specification];
  ;;
  ;;    preceding refined keyword: "new-type"
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
  ;;    preceding refined keyword: "new-type"
  ;;    skip: name
  ;;    keyword: "and-interface_list"
  ;;
  ;; 5) task_type_declaration, single_task_declaration ::=
  ;;       task [type] defining_identifier [known_discriminant_part] [aspect_specification]
  ;;       [is [new interface_list with] task_definition];
  ;;
  ;;    preceding refined keyword: "new-type"
  ;;    skip: name
  ;;    keyword: "and-interface_list"
  ;;
  ;; 6) protected_type_declaration ::=
  ;;    protected type defining_identifier [known_discriminant_part] [aspect_specification] is
  ;;       [new interface_list with] protected_definition;
  ;;
  ;;    preceding refined keyword: "new-type"
  ;;    skip: name
  ;;    keyword: "and-interface_list"
  ;;
  ;; 7) single_protected_declaration ::=
  ;;    protected defining_identifier [aspect_specification] is
  ;;    [new interface_list with] protected_definition;
  ;;
  ;;    preceding refined keyword: "new-type"
  ;;    skip: name
  ;;    keyword: "and-interface_list"
  ;;
  ;; All other occurences are logical expressions, returning "and-op".
  (save-excursion
    (when forward (smie-default-backward-token))

    (or
     (when (equal "interface" (save-excursion (ada-smie-unrefined-token nil)))
       "and-interface"); 2

     (let ((token (ada-smie-backward-keyword)))
       (cond
	((or (equal token "and-interface_list"); 3
	     (equal token "new-type")); 1, 4, 5, 6, 7
	   "and-interface_list")
	(t "and"))); operator identifier
     )))

(defun ada-smie-refine-begin (token forward)
  ;; If "begin" follows a declaration, or immediately follows a block
  ;; start (see ada-smie-pre-begin-tokens), it is begin-body. If it
  ;; follows a statement, it is begin-open. We can determine that by
  ;; parsing backward thru one statement. If that statement is a
  ;; subprogram body, we will have to recurse on refining its "begin",
  ;; but only once.
  ;;
  ;; Consider this code:
  ;;
  ;;   package body Ada_Mode.Nominal is
  ;;
  ;;	  function Function_1b return Float
  ;;	  is
  ;;	     Local_1 : constant := 3.0;
  ;;	  begin
  ;;	     declare
  ;; 	     begin
  ;;		return Local_1;
  ;;	     end;
  ;; 	  end Function_1b;
  ;;   begin
  ;;	  null;
  ;;   end Ada_Mode.Nominal;
  ;;
  ;; To refine the final "begin", we need to look back to
  ;; "function ... is".
  ;;
  (save-excursion
    (when forward (smie-default-backward-token))
    (let ((token (ada-smie-backward-token))
	  (result nil)
	  (first t))
      (while (not result)
	(cond
	 ((equal token ";")
	  (if first
	      (progn
		(setq first nil)
		;; try again
		(ada-smie-goto-statement-start token)
		(setq token (ada-smie-backward-token)))

	    ;; we've skipped a statement or declaration; see if we can tell which
	    (ecase (car (save-excursion
			  (smie-default-forward-token)
			  (ada-smie-statement-or-decl)))
	      (statement
	       (setq result "begin-open"))
	      (declaration
	       (setq result "begin-body"))
	      (formal
	       (ada-smie-error "found generic formal parameter preceding begin spec"))
	      (unknown
	       ;; try again
	       (ada-smie-goto-statement-start token)
	       (setq token (ada-smie-backward-token)))
	      )))

	 ((member token ada-smie-pre-begin-tokens)
	  (setq result "begin-body"))
	 (t
	  (setq result "begin-open")))
	)
      result
      )))

(defun ada-smie-refine-case (token forward)
  (let ((token (save-excursion
		 (when forward (smie-default-backward-token))
		 (ada-smie-unrefined-token nil))))

    (if (equal token "end")
	"case-end"
      "case")))

(defun ada-smie-refine-else (token forward)
  (let ((token (save-excursion
		 (when forward (smie-default-backward-token))
		 (ada-smie-unrefined-token nil))))

    (if (equal token "or")
	"else"; identifier
      "else-other")))

(defun ada-smie-refine-end (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    (let ((token (save-excursion (smie-default-forward-token) (ada-smie-unrefined-token t))))
      (cond
       ((equal "case" token) "end-case")
       ((equal "if" token) "end-if")
       ((equal "loop" token) "end-loop")
       ((equal "record" token) "end-record")
       ((equal "return" token) "end-return")
       ((equal "select" token) "end-select")
       (t "end-block"))
      )))

(defun ada-smie-refine-exception (token forward)
  ;; identifier : exception;
  ;;
  ;; begin
  ;;    statements;
  ;; exception
  ;; when =>
  (let ((token (save-excursion
		 (when forward (smie-default-backward-token))
		 (ada-smie-unrefined-token nil))))
    (cond
     ((equal token ":") "exception-declare")
     ((equal token ";") "exception-block")
     (t (ada-smie-refine-error "exception"))
     )))

(defun ada-smie-refine-exit (token forward)
  (save-excursion
    (let ((token
	   (progn
	     (when (not forward) (smie-default-backward-token))
	     (ada-smie-unrefined-token t))))
      (cond
       ((equal "when" token) "exit-when")
       ((equal "when" (ada-smie-unrefined-token t)); loop label
	"exit-when")
       (t "exit-other")
       ))
    ))

(defun ada-smie-refine-for (token forward)
  ;; 1) quantified_expression ::=
  ;;       for {all | some} loop_parameter_specification => predicate
  ;;     | for {all | some} iterator_specification => predicate
  ;;
  ;; same as 2)
  ;;
  ;; 2) iteration_scheme ::= ...
  ;;     | for defining_identifier in [reverse] discrete_subtype_definition
  ;;     | for defining_identifier in [reverse] iterator_name
  ;;     | for defining_identifier [: subtype_indication] of [reverse] iterable_name
  ;;
  ;;  succeeding unrefined token: "in" ":" "of"
  ;;  skip: identifier
  ;;  token: "for-loop"
  ;;
  ;; 3) attribute_definition_clause ::=
  ;;       for local_name'attribute_designator use expression;
  ;;     | for local_name'attribute_designator use name;
  ;;
  ;;    token: "for-attribute"
  ;;
  ;; 4) enumeration_representation_clause ::=
  ;;     for first_subtype_local_name use enumeration_aggregate;
  ;;
  ;;    token: "for-attribute"
  ;;
  ;; 5) record_representation_clause ::=
  ;;     for first_subtype_local_name use ...
  ;;
  ;;    token: "for-attribute"
  ;;
  ;; 6) at_clause ::= for direct_name use at expression;
  ;;
  ;;    token: "for-attribute"

  (let ((token (save-excursion
		 (when (not forward) (smie-default-forward-token)); for
		 (smie-default-forward-token); identifier
		 (ada-smie-unrefined-token t))))
    (cond
     ((member token '("in" ":" "of")) "for-loop"); 1, 2
     (t "for-attribute")
     )))

;; ada-smie-refine-function see ada-smie-refine-subprogram

(defun ada-smie-refine-if (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    (let ((token (save-excursion (ada-smie-unrefined-token nil))))
      (cond
       ((equal "end" token) "if-end")
       (t "if-open"))
      )))

(defun ada-smie-refine-interface (token forward)
  ;; see `ada-smie-refine-and' for Ada syntax cases
  (let ((token (save-excursion
		 (when (not forward) (smie-default-forward-token))
		 (ada-smie-unrefined-token t))))

    (if (equal token "and")
	"interface-and"
      "interface-plain")))

(defun ada-smie-refine-is (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    ;; too many occurences to document them all at once.

    (or
     ;; First try simple, common constructs.

     (save-excursion
       ;; This is a special case because "protected" and "task" are
       ;; not keywords, so ada-smie-backward-keyword doesn't find
       ;; them. Fortunately, single tasks and protected objects cannot
       ;; have discriminants.
       (let ((token (progn
		      (smie-default-backward-token); identifier
		      (ada-smie-unrefined-token nil))))
	 ;; "protected", "task", "body", "type", "subtype": handled here
	 ;;
	 ;; "": discriminant or parameter list; below
	 ;;
	 ;; "package": below
	 ;;
	 ;; "function" "procedure": generic instantiation,
	 ;;   parameter-less subprogram body, parameter-less
	 ;;   generic_formal_parameter; below
	 (cond
	  ((member token '("protected" "task")) "is-type-block")
	  ((equal token "body")
	    (setq token (ada-smie-unrefined-token nil))
	    (cond
	     ((equal token "protected") "is-protected_body")
	     ((equal token "task") "is-task_body")
	     ))
	  ((equal token "subtype") "is-subtype")
	  ((equal token "type")
	    (setq token (ada-smie-unrefined-token nil))
	    (when (member token '("protected" "task")) "is-type-block")
	    ;; "is-type-*" handled below
	    )
	 )))

     (let* (pos
	    (token (save-excursion (prog1 (ada-smie-backward-keyword) (setq pos (point))))))
       (cond
	((equal token "case") "is-case")

	((member token '("function-inst"))
	 ;; function name is new ...;  generic_instantiation declaration
	 ;; function .. return ...;  see below at "return-*"
	 "is"); identifier

	((member token '("package-formal" "package-inst")) "is"); identifier

	((member token '("package-generic" "package-plain" "package-separate")) "is-package")

	((equal token "procedure-formal") "is"); identifier

	((equal token "procedure-inst")
	 ;;  procedure name is new ...;  generic_instantiation declaration
	 "is"); identifier

	((member token '("procedure-spec" "procedure-overriding" "procedure-separate"))
	 ;;  procedure name is abstract; declaration
	 ;;  procedure name is null;     declaration
	 ;;  procedure name is declarations begin statements end;  body
	 ;;  separate procedure name is declarations begin statements end; separate body
	 (let ((token (save-excursion
			(smie-default-forward-token); is
			(ada-smie-unrefined-token t))))
	   (cond
	    ((member token '("abstract" "null")) "is"); identifier
	    (t "is-subprogram_body"))))

	((equal token "return-formal")
	 ;; with function identifier return name is name
	 "is")

	((equal token "return-spec")
	 ;; function identifier return name is declarations begin
	 ;; function identifier return name is abstract;
	 (if (equal "abstract"
		    (progn
		      (smie-default-forward-token); is
		      (ada-smie-unrefined-token t)))
	     "is"
	 "is-subprogram_body"))

	((member token '("type-protected" "type-task")) "is-type-block")

	((equal token "type-other")
	 (let* (pos
		(token
		 (save-excursion
		   (smie-default-forward-token); is
		   (prog1 (ada-smie-unrefined-token t)
		     (setq pos (point))))))
	   (cond
	    ((equal token ""); paren, string, or end of buffer; assume paren
	     "is-type-enumeration");; type identifier is (...)

	    ((member token '("not" "access")) "is-type-access")

	    ((equal token "record") "is-type")

	    ((and
	      (equal token "tagged")
	      (let ((token (save-excursion (goto-char pos) (ada-smie-unrefined-token t))))
		(cond
		 ((equal token ";")
		  ;; type Incomplete_Type_1 (Discriminant_1 : Integer) is tagged; -- in spec
		  ;; type Incomplete_Type_1 (Discriminant_1 : Integer) is tagged null record; -- in body
		  "is-type-tagged")
		 ((equal token "record") "is-type")
		 (t nil))
		)))

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
	   ))))

     ;; now more complicated things
     (save-excursion
       ;; entry body with params: "entry" identifier "("...")" "when" exp "is"
       ;;
       ;; If we can be guessing wrong here, we can't use
       ;; smie-backward-sexp (because it will just get confused). So
       ;; far, this is the only possibility at this point, so we don't
       ;; really need to check, but we want to identify missing
       ;; cases.
       (if (equal "entry" (nth 2 (smie-backward-sexp "is-entry_body"))) "is-entry_body"))

     (ada-smie-refine-error "is")
     )))

(defun ada-smie-refine-limited (token forward)
  (let ((token (save-excursion
		 (when (not forward) (smie-default-forward-token)); limited
		 (ada-smie-unrefined-token t))))

    (cond
     ((member token '("private" "with")) "limited-context")
     (t "limited"); identifier
     )))

(defun ada-smie-refine-loop (token forward)
  (let ((token (save-excursion
		 (when forward (smie-default-backward-token))
		 (ada-smie-unrefined-token nil))))
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
    ;; If there is no iteration scheme, the first "loop" is
    ;; "loop-open"; otherwise it is "loop-body". Labels are treated
    ;; separately.
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
	      (ada-smie-backward-keyword)))

      (if (member token '("in" "while" "of-loop"))
	  "loop-body"
	"loop-open"))))

(defun ada-smie-refine-mod (token forward)
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

(defun ada-smie-refine-new (token forward)
  ;; 1) [formal_]derived_type_definition ::=
  ;;       type name is [abstract] [limited] new parent_subtype_indication [[and interface_list] record_extension_part]
  ;;
  ;;    preceding unrefined token: is
  ;;    skip: ada-smie-type-modifiers
  ;;    2nd preceding refined token: type-other
  ;;    token: new-type
  ;;
  ;; 2) allocator ::=
  ;;       new [subpool_specification] subtype_indication
  ;;     | new [subpool_specification] qualified_expression
  ;;
  ;;    preceding, succeeding; no help
  ;;    token: "new"; identifier
  ;;
  ;; 3) private_extension_declaration ::=
  ;;       type defining_identifier [discriminant_part] is
  ;;         [abstract] [limited | synchronized] new ancestor_subtype_indication
  ;;         [and interface_list] with private
  ;;           [aspect_specification];
  ;;
  ;;    preceding unrefined token: is
  ;;    skip: ada-smie-type-modifiers
  ;;    2nd preceding refined token: nothing helpful
  ;;    token: new-type
  ;;
  ;; 4) single_protected_declaration, protected_type_declaration, task_type_declaration, single_task_declaration ::=
  ;;       {protected | task} [type] defining_identifier [known_discriminant_part]
  ;;            [aspect_specification] [is [new interface_list with] task_definition];
  ;;
  ;;    preceding unrefined token: is
  ;;    skip: nothing
  ;;    2nd preceding refined token: nothing helpful
  ;;    token: new-type
  ;;
  ;; 5) generic_instantiation ::=
  ;;       {function | procedure | package} defining_program_unit_name is
  ;;          new generic_package_name [generic_actual_part] [aspect_specification];
  ;;
  ;;    preceding unrefined tokens: is
  ;;    skip: nothing
  ;;    2nd preceding refined token: {function-inst | procedure-inst | package-inst}
  ;;    skip: non-keywords
  ;;    token: new-inst
  ;;
  ;; 6) formal_package_declaration ::=
  ;;       with package defining_identifier is new generic_package_name formal_package_actual_part
  ;;            [aspect_specification];
  ;;
  ;;    preceding unrefined tokens: is
  ;;    skip: nothing
  ;;    2nd preceding refined token: {function-formal | procedure-formal | package-formal}
  ;;    skip: non-keywords
  ;;    token: new-formal

  (save-excursion
    (when forward (smie-default-backward-token))
    (let ((token (ada-smie-backward-type-modifiers)))
      (cond
       ((equal token "is")
	(let ((token (ada-smie-backward-keyword)))
	  (cond
	   ((member token '("package-formal" "procedure-formal" "function-formal")) "new-formal")
	   ((member token '("package-inst" "procedure-inst" "function-inst")) "new-inst")
	   (t "new-type"))))

       (t "new"); allocator
       ))))

(defun ada-smie-refine-of (token forward)
  ;; 1)  unconstrained_array_definition ::=
  ;;       array(index_subtype_definition {, index_subtype_definition}) of component_definition
  ;;     constrained_array_definition ::=
  ;;       array (discrete_subtype_definition {, discrete_subtype_definition}) of component_definition
  ;;
  ;; "array" is an identifier
  ;;
  ;; 1a) type identifier is array_type_definition;
  ;;
  ;;  preceding refined token: "is-type"
  ;;  token: "of-type"
  ;;
  ;; 1b) identifier : array_type_definition
  ;;
  ;;  preceding refined token: ":-object"
  ;;  token: "of-object"
  ;;
  ;; 2)  for {all | some} defining_identifier [: subtype_indication] of [reverse] iterable_name
  ;;
  ;;  "all", "some" are left as identifiers
  ;;  preceding refined keyword: ":-loop", "for-loop"
  ;;  token: "of-loop"

  (save-excursion
    (when forward (smie-default-backward-token))

    (let ((token (save-excursion (ada-smie-backward-keyword))))
      (cond
       ((equal token "is-type") "of-type")
       ((member token '(":-loop" "for-loop")) "of-loop")
       (t "of-object")
       ))))

(defun ada-smie-refine-or (token forward)
  (let ((token (save-excursion
		 (when (not forward) (smie-default-forward-token))
		 (ada-smie-unrefined-token t))))
    (cond
     ((member token '("accept" "delay" "terminate" "when")) "or-select")
     (t "or")))); operator identifier

(defun ada-smie-refine-package (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))
    (let ((token (save-excursion (ada-smie-unrefined-token nil))))
      (or
       (cond
	((and
	  (equal token "")
	  (progn (forward-comment (- (point)))
		 (bobp))) "package-plain");; beginning of buffer
	((equal token "") "package-separate");; token is ")"; separate (name) package
	((equal token "access") "package-access")
	((equal token "with") "package-formal")
	)

       (save-excursion
	 (setq token (ada-smie-forward-tokens-unrefined "body" "is" "renames"))
	 (cond
	  ((equal token "renames") "package-renames")
	  ((and (equal token "is")
		(equal "new" (save-excursion (ada-smie-unrefined-token t))))
	   "package-inst")
	  ))

       (if (ada-smie-generic-p) "package-generic")

       "package-plain")
    )))

(defun ada-smie-refine-private (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    ;; 'private' occurs in:
    ;;
    ;; 1) [formal_]private_type_declaration
    ;;
    ;;    type defining_identifier [discriminant_part] is [[abstract] tagged] [limited] private
    ;;       [with aspect_mark];
    ;;
    ;;    succeeding unrefined tokens: "with aspect_clause;" or ";"
    ;;    preceeding unrefined token: is
    ;;    preceeding refined token: is-type
    ;;       need to check for is-type to distinguish from package with null public part
    ;;    skip: ada-smie-type-modifiers
    ;;    token: private-type-spec
    ;;
    ;; 2) [non]limited_with_clause ::=
    ;;       [limited] [private] with library_unit_name {, library_unit_name};
    ;;
    ;;    succeeding unrefined tokens: "with name[, name];"
    ;;    skip: nothing
    ;;    token: private-context-1, -2
    ;;
    ;; 3) formal_derived_type_definition
    ;;
    ;; 4) library_item ::= [private] library_unit_declaration
    ;;
    ;;    token: private-library
    ;;
    ;; 5) package_specification
    ;;
    ;;    package defining_program_unit_name [aspect_specification] is
    ;;       {basic_declarative_item}
    ;;    [private
    ;;       {basic_declarative_item}]
    ;;    end [[parent_unit_name.]identifier]
    ;;
    ;;    token: private-spec
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
    (cond
     ((equal "with" (save-excursion (ada-smie-unrefined-token nil)))
      "private-with"); 6

     ((equal "is-type" (save-excursion (ada-smie-backward-type-modifiers)
				       (unless (bobp)
					 (ada-smie-forward-token))))
      "private-type-spec"); 1

     ((let ((token (save-excursion
		     (smie-default-forward-token); private
		     (ada-smie-unrefined-token t))))
	(cond
	 ((member token '("with" ";"))
	  ;; 2
	  (if (equal "limited" (save-excursion (ada-smie-unrefined-token nil)))
	      "private-context-2"
	    "private-context-1"))

	 ((member token '("package" "procedure" "function" "generic")); 4 or 5
	  (if (ada-smie-library-p)
	      "private-library"; 4
	    "private-spec")); 5
	 )))

     (t "private-spec")); all others
  ))

;; ada-smie-refine-procedure see ada-smie-refine-subprogram

(defun ada-smie-refine-protected (token forward)
  ;; 'protected' occurs in:
  ;;
  ;; 1) interface_type_definition
  ;;
  ;;    not implemented
  ;;
  ;; 2) access_to_subprogram_definition ::=
  ;;       access [protected] procedure parameter_profile
  ;;     | access [protected] function  parameter_and_result_profile
  ;;
  ;;    identifier
  ;;
  ;; 3) access_definition  ::=
  ;;       [null_exclusion] access [constant] subtype_mark
  ;;     | [null_exclusion] access [protected] procedure parameter_profile
  ;;     | [null_exclusion] access [protected] function parameter_and_result_profile
  ;;
  ;;    identifier
  ;;
  ;; 4) protected_type_declaration
  ;;
  ;;    protected type defining_identifier [known_discriminant_part]
  ;;       [aspect_specification] is [new interface_list with] protected_definition;
  ;;
  ;;    token: protected_type
  ;;
  ;; 5) single_protected_declaration
  ;;
  ;;    same as protected_type_declaration, without "type-other"
  ;;
  ;; 6) protected_body, subunit :=
  ;;
  ;;    a) protected body defining_identifier ...
  ;;
  ;;       token: protected_body
  ;;
  ;;    b) separate (parent_unit_name) protected body defining_identifier ...
  ;;
  ;;       token: protected_separate
  ;;
  ;; 7) protected_body_stub  ::=
  ;;      protected body defining_identifier is separate [aspect_specification];
  ;;
  ;;       token: protected_body
  ;;
  (save-excursion
    (when forward (smie-default-backward-token))
    (cond
     ((equal "access" (save-excursion (ada-smie-unrefined-token nil)))
      "protected"); 2, 3

     ((equal "separate-unit" (save-excursion (ada-smie-backward-keyword)))
      "protected-separate"); 6b

     ((equal "body" (save-excursion
		      (smie-default-forward-token)
		      (ada-smie-unrefined-token t)))
      "protected-body"); 6a, 7

     (t "protected-type")); 5, 4
    ))

(defun ada-smie-refine-raise (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    (let ((token (save-excursion (ada-smie-forward-tokens-unrefined "with" ";"))))
      (cond
       ((equal token ";") "raise"); identifier
       ((equal token "with") "raise-stmt")
       ))
    ))

(defun ada-smie-refine-record (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    (let ((token (save-excursion (ada-smie-unrefined-token nil))))
      (cond
       ((equal token "end")
	(if (equal "with" (save-excursion
			    (smie-default-forward-token); record
			    (ada-smie-unrefined-token t)))
	    "record-end-aspect";
	  "record-end"))
       ((equal token "null") "record-null")
       (t "record-type")
       ))
    ))

(defun ada-smie-refine-renames (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    ;; 1) [overriding] subprogram renaming: "renames-subprogram"
    ;; 2) anywhere else: "renames-other"

    (let ((parent (progn
                    (ada-smie-goto-parent token 1)
                    (ada-smie-unrefined-token t))))
      (when (equal parent "overriding")
	(setq parent (ada-smie-unrefined-token t)))
      (cond
       ((member parent '("function" "procedure")) "renames-subprogram")    ; 1
       (t "renames-other")
       ))
    ))

(defun ada-smie-refine-return (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    ;; 'return' occurs in:
    ;;
    ;; 1) a [generic] function subprogram_declaration or formal_subprogram_declaration:
    ;;
    ;;    1a) function_specification ::= function defining_designator parameter_and_result_profile
    ;;
    ;;    preceding refined token: "function-spec", "function-overriding",
    ;;       "function-generic", "function-separate"
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
    ;;    preceding refined token: "function-spec", "function-overriding"
    ;;    token: "return-spec"
    ;;
    ;; 3) a return statement:
    ;;
    ;;    3a) return;
    ;;    3b) return exp;
    ;;
    ;;    identifier
    ;;
    ;; 4) an extended return statement:
    ;;
    ;;    return identifier : name;
    ;;
    ;;    identifier (4a)
    ;;
    ;;    return identifier : name do statements end return;
    ;;
    ;;    token: "return-ext" (4b), "return-end" (4c)
    ;;
    ;; 5) an access function type declaration:
    ;;
    ;;    type name is access [protected] function identifier (...) return [access] name;
    ;;
    ;;    preceding refined token: "function-spec"  _not_ overriding or generic
    ;;    token: "return-spec"
    ;;
    ;; 6) a subprogram_specification in a subprogram_renaming_declaration
    ;;    same as 1a.
    ;;
    ;; So we have to look both forward and backward to resolve this.
    (or
     (if (equal "end" (save-excursion (ada-smie-unrefined-token nil))) "return-end"); 4c

     ;; Do this now, otherwise we can't distinguish between:
     ;;
     ;; function F1 return Integer;
     ;; return 0;
     ;;
     (let ((token (save-excursion (ada-smie-backward-keyword))))
       (cond
	((member token '("function-spec" "function-overriding" "function-generic" "function-separate"))
	 "return-spec"); 1a, 2, 5

	((equal token "function-formal")
	 "return-formal"); 1b
	))

     (save-excursion
       (let ((token (progn
		      (smie-default-forward-token); return
		      (ada-smie-unrefined-token t))))
	 (cond
	  ((equal token ";") "return"); 3a
	  (t
	   (setq token (ada-smie-unrefined-token t)); identifier | :
	   (cond
	    ((equal token ":-do") "return-ext"); 4b
	    (t "return"); 3b, 4a
	    )))))
     )))

(defun ada-smie-refine-select (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    (let ((prev-token (ada-smie-unrefined-token nil)))
      (cond
       ((equal prev-token "end") "select-end")
       (t "select-open"))
  )))

(defun ada-smie-refine-separate (token forward)
  (let ((token (save-excursion
		 (when (not forward) (smie-default-forward-token))
		 (ada-smie-unrefined-token t))))

    (if (equal token ";")
	"separate-stub"
      "separate-unit")))

(defun ada-smie-refine-subprogram (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    (let ((prev-token (save-excursion (ada-smie-unrefined-token nil))))
      (cond
       ((equal prev-token "with")	(concat token "-formal"))
       ((equal prev-token "overriding") (concat token "-overriding"))
       ((and
	 (equal prev-token "");; '"', ")", bob; '"' not legal
	 (save-excursion (forward-comment (- (point-max))) (not (bobp))))
	(concat token "-separate")); separate (name) [function | procedure]

       ((member prev-token '("access" "protected"))
	(concat token "-spec")); access_to_subprogram_definition

       ((save-excursion
	  (and
	   (equal "is" (ada-smie-forward-tokens-unrefined "return" "is" ";")); end of subprogram-spec
	   (equal "new" (ada-smie-unrefined-token t))))
	;; generic_instantiation. We have to check for "new" without
	;; refining it.
	(concat token "-inst"))

       ((ada-smie-generic-p) (concat token "-generic"))

       (t (concat token "-spec"))
  ))))

(defun ada-smie-refine-task (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))
    (cond
     ((equal (save-excursion (ada-smie-backward-keyword)) "separate-unit")
      "task-separate"); separate (name) task body

     ((equal (save-excursion
               (smie-default-forward-token)
               (ada-smie-unrefined-token t))
	     "body")
      "task-body")

     ((equal (save-excursion
	       (smie-default-forward-token)
	       (ada-smie-forward-keyword)) ";")
      "task-single")

     (t "task-type"))
    ))

(defun ada-smie-refine-then (token forward)
  (let ((token (save-excursion
		 (when forward (smie-default-backward-token))
		 (ada-smie-unrefined-token nil))))

    (cond
     ((equal token "and") "then"); identifier
     ((member (nth 2 (save-excursion (ada-smie-goto-parent token 1)))
	      '("if-open" "elsif"))
      "then-if")
     (t "then-select")
     )))

(defun ada-smie-refine-type (token forward)
  (let ((token (save-excursion
		 (when forward (smie-default-backward-token))
		 (ada-smie-unrefined-token nil))))

    (cond
     ((equal token "protected") "type-protected")
     ((equal token "task") "type-task")
     ((equal token "use") "type-use")
     (t "type-other")
     )))

(defun ada-smie-refine-use (token forward)
  ;; 1) use_clause:
  ;;
  ;;    a) use_package_clause ::= use package_name {, package_name};
  ;;
  ;;    b) use_type_clause ::= use [all] type subtype_mark {, subtype_mark};
  ;;
  ;;    token: use-decl
  ;;
  ;; 2a) attribute_definition_clause ::=
  ;;      for local_name'attribute_designator use expression;
  ;;    | for local_name'attribute_designator use name;
  ;;
  ;;  b) enumeration_representation_clause ::=
  ;;        for first_subtype_local_name use enumeration_aggregate;
  ;;
  ;;    token: use-attribute
  ;;
  ;; 3) record_representation_clause ::=
  ;;       for first_subtype_local_name use
  ;;          record [mod_clause]
  ;;             {component_clause}
  ;;          end record;
  ;;
  ;;    use-record

  (let ((token (save-excursion
		 (when forward (smie-default-backward-token))
		 (ada-smie-backward-keyword))))

    (if (equal token "for-attribute")
	(if (equal "record" (save-excursion (ada-smie-unrefined-token t)))
	    "use-record"
	  "use-attribute")
      "use-decl")))

(defun ada-smie-refine-when (token forward)
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
     (if (member (save-excursion (ada-smie-unrefined-token nil)) '("select" "or"))
	 "when-select")

     (save-excursion
       (let ((token (ada-smie-unrefined-token nil)))
	 (cond
	  ((equal token "exit")
	   "when-exit")
	  ((and
	    (not (ada-smie-keyword-p token)) ;; "exit label when", not "exit; when foo => "
	    (equal (ada-smie-unrefined-token nil) "exit"))
	   "when-exit"))))

     (if (equal "entry" (ada-smie-backward-keyword))
	 "when-entry"; 5
       "when-case"); 1
     )))

(defun ada-smie-refine-with (token forward)
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
    ;;    keyword: "with-new"
    ;;
    ;; 2) extension_aggregate ::=
    ;;       (ancestor_part with record_component_association_list)
    ;;
    ;;    ancestor_part can be an aggregate for the parent type, or the parent type name.
    ;;    ada-smie-backward-keyword : "(" for either (aggregate paired parens are skipped)
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
    ;;    preceding refined token: "new-type", "and-interface_list"
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
    ;;    keyword: "with-context-1" -2
    ;;
    ;; 7) raise_statement ::= raise;
    ;;       | raise exception_name [with string_expression];
    ;;
    ;;    preceding refined token: "raise-stmt"
    ;;    keyword: "with-raise"
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
    ;;    preceding unrefined token: "record"
    ;;    skip: nothing
    ;;    token: with-aspect
    ;;
    (or
     (let ((token (save-excursion
		    (smie-default-forward-token); with
		    (ada-smie-unrefined-token t))))
       (cond
	((member token '("function" "package" "procedure")) "with-formal"); 8
	((equal token "record") "with-new"); 1
       ))

     ;; this checks for preceding ";", so it has to be after the
     ;; above; with-formal also has a preceding ";"
     (let ((token (save-excursion (ada-smie-unrefined-token nil))))
       (cond
	((equal token "record") "with-aspect"); 11
	((equal token ""); bob, ")"
	 (if (progn (forward-comment (- (point)))
		    (bobp))
	     "with-context-1"; 6
	   "with-agg");2
	 )
	((equal token ";")
	 "with-context-1"); 6
	((member token '("limited" "private"))
	 "with-context-2"); 6
	(t nil)))

     (let ((token (save-excursion (ada-smie-backward-keyword))))
       (cond
	((equal token "(") "with-agg"); 2
	((equal token "raise-stmt") "with-raise"); 7
	))

     "with-new")
    ))

;;;; forward/backward token

;; ada-smie-forward-token must produce the same results on a string
;; of tokens as ada-smie-backward-token. To ensure that, we use an
;; alist to specify the refining functions.

(defconst ada-smie-next-token-alist
  ;; Alphabetical order.
  '(("<>;" ";")
    ("''';" ";")
    ;; These chars all have punctuation syntax. We need that for
    ;; expressions, so we don't change it. "<>", "'''" are not grammar
    ;; keywords, so we can just refine this to ";"

    (":" 	 ada-smie-refine-:)
    ("=>" 	 ada-smie-refine-=>)
    ("abort" 	 ada-smie-refine-abort)
    ("accept" 	 ada-smie-refine-accept)
    ("and" 	 ada-smie-refine-and)
    ("begin" 	 ada-smie-refine-begin)
    ("case" 	 ada-smie-refine-case)
    ("end" 	 ada-smie-refine-end)
    ("else" 	 ada-smie-refine-else)
    ("exception" ada-smie-refine-exception)
    ("exit" 	 ada-smie-refine-exit)
    ("for" 	 ada-smie-refine-for)
    ("function"  ada-smie-refine-subprogram)
    ("interface" ada-smie-refine-interface)
    ("if" 	 ada-smie-refine-if)
    ("is" 	 ada-smie-refine-is)
    ("limited" 	 ada-smie-refine-limited)
    ("loop" 	 ada-smie-refine-loop)
    ("mod" 	 ada-smie-refine-mod)
    ("new" 	 ada-smie-refine-new)
    ("of" 	 ada-smie-refine-of)
    ("or" 	 ada-smie-refine-or)
    ("package" 	 ada-smie-refine-package)
    ("private" 	 ada-smie-refine-private)
    ("procedure" ada-smie-refine-subprogram)
    ("protected" ada-smie-refine-protected)
    ("raise" 	 ada-smie-refine-raise)
    ("record" 	 ada-smie-refine-record)
    ("renames" 	 ada-smie-refine-renames)
    ("return" 	 ada-smie-refine-return)
    ("select" 	 ada-smie-refine-select)
    ("separate"  ada-smie-refine-separate)
    ("task" 	 ada-smie-refine-task)
    ("then" 	 ada-smie-refine-then)
    ("type" 	 ada-smie-refine-type)
    ("use" 	 ada-smie-refine-use)
    ("when" 	 ada-smie-refine-when)
    ("with" 	 ada-smie-refine-with)
    )
  "Alist of (token keyword) or (token refine-defun), used by
`ada-smie-next-token' to refine tokens.
`token' is the string returned by smie-default-[forward|backward]-token.
If `keyword' is a string, it is returned.
If `keyword' is a defun, it is called with two args (token forward);
forward is t if moving forward (point at right end of token), nil if
moving backward (point at left end of token). It must return the
refined token or nil.
If a token is not in the alist, it is returned unrefined.")

;; cache operations

(defvar ada-smie-cache-max 0
  "Maximimum position in buffer where ada-smie token refinement cache is valid.")
(make-variable-buffer-local 'ada-smie-cache-max)

(defvar ada-smie-refining nil
  "t when parsing forward to validate cache.")

(defun ada-smie-invalidate-cache()
  "Invalidate the ada-smie token cache for the current buffer."
  (interactive)
  (setq ada-smie-cache-max 0)
  (with-silent-modifications
    (remove-text-properties (point-min) (point-max) '(ada-smie-cache))))

(defun ada-smie-validate-cache (pos)
  "Update cache from `ada-smie-cache-max' to at least POS."
  (save-excursion
    (let ((ada-smie-refining t))
      (goto-char ada-smie-cache-max)
      (when (bobp) (ada-smie-forward-keyword))
      (while (> pos (point))
	(ada-smie-forward-keyword))
      ;; If pos is inside a paren, but ada-smie-cache-max was
      ;; outside it, this just skipped us.
      (when (not (ada-smie-get-cache pos))
	;; make sure ada-smie-cache-max is after parens
	(setq ada-smie-cache-max (point))
	(goto-char pos)
	(ada-smie-validate-cache-parens))
    )))

(defun ada-smie-validate-cache-parens ()
  "Assuming point is inside parens, validate cache within the parens."
  (save-excursion
    (let ((prev-cache-max ada-smie-cache-max)
	  (done nil)
	  (ada-smie-refining t)
	  token)

      (condition-case err
	  (progn
	    ;; move to just after outermost openning paren
	    (while (ada-in-paren-p)
	      (ada-goto-open-paren 0))
	    (forward-char 1)

	    ;; force calling refine-*. Set ada-smie-cache-max before (
	    ;; so first keyword doesn't think it's already refined.
	    (setq ada-smie-cache-max (- 1 (point)))
	    (while (not done)
	      (setq token (ada-smie-forward-token))
	      (cond
	       ((equal token "")
		(cond
		 ((eobp)
		  (setq done t))

		 ((eq (char-after) ?\))
		  (forward-char 1)
		  (setq done (not (ada-in-paren-p))))

		 ((eq (char-after) ?\()
		  (forward-char 1))

		 ((member (char-after) '(?\" ?\'))
		  (ada-smie-skip-lowlevel-sexp t))

		 (t
		  (ada-smie-error "unexpected empty token in ada-smie-validate-cache-parens"))
		 ))

	       (t nil)
	       ))
	    (setq ada-smie-cache-max prev-cache-max)
	    )
	(scan-error
	 ;; from scan-lists; can happen when user is typing code
	 (setq ada-smie-cache-max prev-cache-max)))
  )))

(defun ada-smie-get-cache (pos)
  "Return refined token string from the `ada-smie-cache' text property at POS."
  (get-text-property pos 'ada-smie-cache))

(defun ada-smie-put-cache (pos token)
  "Set TOKEN as the refined token string in the `ada-smie-cache' text property at POS.
Return TOKEN."
  (let ((inhibit-modification-hooks t))
    ;; We are not changing any text, so neither font-lock nor
    ;; ada-smie-after-change needs to be called.
    (with-silent-modifications
      (put-text-property pos (+ 1 pos) 'ada-smie-cache token))
    (setq ada-smie-cache-max (max ada-smie-cache-max pos))
    token))

(defun ada-smie-after-change (begin end length)
  "For `after-change-functions'."
  ;; We only need to move ada-smie-cache-max if this change affects
  ;; code, ie non-whitespace non-comment. Otherwise, we could just
  ;; adjust it by the change amount. But we're keeping it simple, so
  ;; we always move cache-max to `begin' or earlier; optimize later if
  ;; needed.
  ;;
  ;; ada-smie-cache-max must always be on a keyword.
  (when (>= ada-smie-cache-max begin)
    (save-excursion
      ;; (info "(elisp)Parser State")
      (let ((state (syntax-ppss begin)))
	(cond
	 ((or
	   (nth 3 state); in string
	   (nth 4 state)); in comment
	  (goto-char (nth 8 state)));; start of string or comment

	 (t ;; syntax-ppss has moved point to "begin".
	  )
	 )
	 (ada-smie-backward-keyword)
	 (setq ada-smie-cache-max (point))
	))))

(defun ada-smie-unrefined-token (forward)
  "Move to the next token; forward if FORWARD non-nil, backward otherwise.
Return the unrefined lowercased token text."
  (if forward
      (downcase (smie-default-forward-token))
    (downcase (smie-default-backward-token))))

(defun ada-smie-next-token (forward)
  "Move to the next token; forward if FORWARD non-nil, backward otherwise.
Return the token text or a refinement of it. Manage the refinement cache."
  (let* (cache-pos
	 (token (if forward
		    (progn
		      (forward-comment (point-max))
		      (setq cache-pos (point))
		      (smie-default-forward-token))
		  (prog1
		      (smie-default-backward-token)
		    (setq cache-pos (point)))))
	 (refine (cadr (assoc (downcase token) ada-smie-next-token-alist))))
    (cond
     ((stringp refine) refine)

     ((functionp refine)
      (if (<= cache-pos ada-smie-cache-max)
	  (or (ada-smie-get-cache cache-pos)
	      (if (ada-in-paren-p)
		  ;; parens are skipped on the first pass, but we are
		  ;; now parsing inside one.
		  (progn
		    (ada-smie-validate-cache-parens)
		    (ada-smie-get-cache cache-pos))
		;; else something has screwed up the cache, so start over
		(ada-smie-invalidate-cache)
		(ada-smie-validate-cache cache-pos)
		(ada-smie-get-cache cache-pos)))
	(if ada-smie-refining
	    (ada-smie-put-cache cache-pos (funcall refine (downcase token) forward))
	  (ada-smie-validate-cache cache-pos)
	  (ada-smie-get-cache cache-pos)
	  )))

     (t token))
    ))

(defun ada-smie-forward-token () (ada-smie-next-token t))
(defun ada-smie-backward-token () (ada-smie-next-token nil))

;;;; indent rules

(defun ada-smie-when (base)
  "Return indentation offset to use after \"when\", \"=>-when\".
BASE should be `ada-indent' or `ada-indent-broken'."
  ;; Use ada-indent-when if "when" is at start of line. It may not be
  ;; in a select statement.
  (save-excursion
    (unless (looking-at "when")
      (if (not (equal "when" (ada-smie-backward-tokens-unrefined "when" "(" ";")))
	  (ada-smie-error "'when' not found")))
    (if (smie-indent--bolp)
	(+ base ada-indent-when)
      base)))

(defun ada-smie-rule-current (offset)
  "Indent relative to the current line"
  (cons 'column (+ (save-excursion (back-to-indentation) (current-column)) offset)))

(defun ada-smie-keyword-p (token)
  (assoc token ada-smie-grammar))

(defun ada-smie-opener-p (token)
  (let ((association (assoc token ada-smie-grammar)))
    (when association (listp (nth 1 association)))))

(defun ada-smie-closer-p (token)
  (let ((association (assoc token ada-smie-grammar)))
    (when association (listp (nth 2 association)))))

(defun ada-smie-next-statement-keyword (keyword forward next-token lvl-forw lvl-back)
  "Assuming point is at beginning of KEYWORD that is in a
multi-keyword statement, move to the next keyword in the same
statement, skipping contained statements.  Return found
keyword (a string).  If FORWARD is non-nil, move forward,
otherwise move backward.  If already at farthest keyword, do not
move, and return KEYWORD."
  (let ((tok-levels (assoc keyword ada-smie-grammar))
	stack
	(done nil))

    (if (listp (funcall lvl-forw tok-levels))
	;; keyword is farthest in statement; don't move
	(setq done t)
      (when forward (smie-default-forward-token))
      (push tok-levels stack))

    (while (not done)
      (setq tok-levels (ada-smie-next-keyword next-token forward))

      (cond
       ((listp (funcall lvl-back tok-levels))
	;; start of new multi-keyword statement
	(push tok-levels stack))

       ((= (funcall lvl-back tok-levels) (funcall lvl-forw (car stack)))
	;; new keyword is in same statement as top of stack
	(pop stack)
	(setq done (null stack))
	(when (and (not done)
		   (numberp (funcall lvl-forw tok-levels)))
	  ;; new token is in current statement, but not the furthest
	  (push tok-levels stack)))

       ((> (funcall lvl-back tok-levels) (funcall lvl-forw (car stack)))
	;; new keyword is a single-keyword contained statement; ignore.
	nil)

       (t
	;; most likely the user was not on a multi-keyword statement
	;; in the first place, so this is a reasonable place to stop.
	(if (not ada-smie-debug)
	    (setq done t)
	  (ada-smie-error "ada-smie-next-statement-keyword: that does not compute")))
       ))
    (nth 0 tok-levels)
    ))

(defun ada-smie-backward-statement-keyword (keyword)
  (ada-smie-next-statement-keyword
   keyword
   nil; forward
   'ada-smie-backward-token
   (lambda (tok-levels) (nth 1 tok-levels)); lvl-forw
   (lambda (tok-levels) (nth 2 tok-levels)); lvl-back
   ))

(defun ada-smie-forward-statement-keyword (keyword)
  (ada-smie-next-statement-keyword
   keyword
   t; forward
   'ada-smie-forward-token
   (lambda (tok-levels) (nth 2 tok-levels)); lvl-forw
   (lambda (tok-levels) (nth 1 tok-levels)); lvl-back
   ))

(defun ada-smie-rule-keyword (offset keyword pos)
  "Find the previous keyword in the statement/declaration
containing KEYWORD (using `ada-smie-backward-statement-keyword'),
return an indent by OFFSET relevant to it. Preserves point.
POS must be at the start of KEYWORD."
  (save-excursion
    (goto-char pos)
    (ada-smie-backward-statement-keyword keyword)
    (back-to-indentation)
    (cons 'column (+ (current-column) offset))
    ))

(defun ada-smie-goto-parent (child up)
  "Goto a parent (defined by where smie-backward-sexp stops).
If CHILD is non-nil and a smie keyword, find its parent (which may be
itself, if it is a parent and UP is 1).
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
	  ((ada-smie-keyword-p child)
	   child)
	  (t (ada-smie-backward-keyword)))))

    (setq token
	  (list (nth 1 (assoc token ada-smie-grammar)) (point) token))
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
		(setq token (ada-smie-backward-keyword))
		(setq token
		      (list (nth 1 (assoc token ada-smie-grammar)) (point) token))))

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

(defun ada-smie-rule-parent (offset child)
  "Find the parent of CHILD (using `ada-smie-goto-parent'),
return an indent by OFFSET relevant to it. Does not stop on
CHILD. Preserves point.  CHILD must be non-nil and a keyword
or \"(\", and point must be at the start of CHILD."
  (save-excursion
    (let
	((parent
	  (ada-smie-goto-parent
	   child
	   (if (and child
		    (ada-smie-opener-p child))
	       2
	     1))))
      (cond
       ((equal (nth 2 parent) "(")
	;; indent to the (, not the line it is on
	(forward-char 1)
	(forward-comment (point-max)))

       (t (back-to-indentation)))
      (cons 'column (+ (current-column) offset))
    )))

(defun ada-smie-goto-statement-start (child)
  "Move point to the start of the statement/declaration
containing point.  If point is in a parenthesized list, move to
the start of the current list element.  Return (preceding-pos
preceding-token), where `preceding-token' is the text of the
token preceding the statement start, and `preceding-pos' is the
position of its first character.  If point is at statement or
list element start, does nothing.  If CHILD is non-nil, it must
be a keyword, and point must be at the start of CHILD."
  ;; See ada-smie-show-statement-start for interactive call of
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
  ;; "function" keyword is a smie parent; ada-smie-goto-parent will
  ;; stop there. So we have to loop:
  ;;
  ;;    loop
  ;;       ada-smie-goto-parent
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
  ;; because operators are just identifiers. However, to find the
  ;; start of A (which could be a complex name), we must use
  ;; ada-smie-backward-keyword.
  ;;
  ;; Object declarations can declare multiple objects:
  ;;
  ;;   A,
  ;;    B : type;
  ;;
  ;; The first keyword is ":"; we use ada-smie-backward-keyword.
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
  (let (parent-count
	(parent nil)
	(parent-pos nil)
	(orig-child child)
	pos)

    (catch 'done

      ;; We have to check the previous keyword for procedure calls,
      ;; "procedure" and "function", and a few other cases.  Many
      ;; statements have non-keyword tokens before the first token
      ;; (assignment), so we have to use use ada-smie-backward-keyword
      ;; to find the previous keyword.
      (when (not (or
		  (member child ada-smie-block-keywords)
		  (member child ada-smie-block-end-keywords)))
	(setq pos (point));; not using save-excursion so we can do throw 'done from here.
	(setq parent (ada-smie-backward-keyword))
	(setq parent-pos (point))
	(cond
	 ((or
	   (equal parent ";")
	   (member parent ada-smie-block-keywords))
	  (smie-default-forward-token); ";" or block start
	  (forward-comment (point-max)); also skips final whitespace
	  ;; Make sure we moved. In this case:
	  ;;
	  ;;    is begin
	  ;;       return Integer (Function_1a);
	  ;;
	  ;; intending "return", goto-statement-start is called from
	  ;; indent-after-keyword with point on
	  ;; "begin". backward-keyword finds "is", which is a block
	  ;; keyword. Then we move back to "begin". No other blocks
	  ;; may be empty.
	  (if (not (= pos (point)))
	      (throw 'done (list parent-pos parent))
	    ))

	 ((equal parent "("); this is a function call in an aggregate or param list
	  (progn
	    (forward-char 1)
	    (forward-comment (point-max)); also skips final whitespace
	    (throw 'done (list parent-pos parent))))

	 ((equal parent ":-object")
	  (throw 'done (ada-smie-skip-identifier-list nil)))
	 ))

      (when pos (goto-char pos))

      (if (equal child ";")
	  ;; we want to go to the start of the statement preceding
	  ;; ";". goto-parent with child = ";" will go to the next ";"
	  ;; or block open, which is not the same. In particular,
	  ;; block and statement labels get in the way.
	  (setq child (ada-smie-backward-keyword)))

      (setq parent-count
	    (if (member child '("<<"))
		2
	      1))
      ;; If child is "(" the first run thru smie-backward-sexp
      ;; produces no motion; that causes the second run to call the
      ;; lower level scanner to skip the parens, because
      ;; forward-sexp-function is let-bound to nil.
      ;;
      ;; If child is record, that is its own parent, and we want the
      ;; next one up. Same for "<<".

      (setq parent (ada-smie-goto-parent child parent-count))

      (when (or
	     (member (nth 2 parent) '("(" ";"))
	     (member (nth 2 parent) ada-smie-block-keywords)
	     )
	;; goto-parent left point on token following parent; point is at statement start
	(throw 'done (list (nth 1 parent) (nth 2 parent))))

      (when (equal (nth 2 parent) "in")
	;; we're in a parameter list or formal object declaration with
	;; a default expression; move to ",", "(", or ";"
	(smie-default-backward-token)
	(setq parent (ada-smie-goto-parent "in" 1))
	(when (member (nth 2 parent) '("(" ";"))
	  (throw 'done (list (nth 1 parent) (nth 2 parent)))))

      (when (equal (nth 2 parent) ",")
	(cond
	 ((equal orig-child ",")
	  (throw 'done (list (nth 1 parent) (nth 2 parent))))
	 ((equal orig-child ";")
	  ;; we're in a parameter list or object declaration, with
	  ;; multiple identifiers before the ":".
	  (while (equal (nth 2 parent) ",")
	    (setq parent (ada-smie-goto-parent "," 1)))
	  (throw 'done (list (nth 1 parent) (nth 2 parent))))
	 ))

      (if (and
	   (not (= (point) (nth 1 parent)))
	   (not (equal (nth 2 parent) "(")))
	  ;; goto-parent stopped because of a lower-precedence token
	  ;; or paren, not a closer; that might be because:
	  ;;
	  ;; 1) a parameter list object:
	  ;;
	  ;;    1a) (A : in Integer;
	  ;;         B : in Integer;
	  ;;
	  ;;        indenting B; stops on "in", moves to A, returns (
	  ;;
	  ;;    1b) (A : in Integer;
	  ;;         B : in Integer;
	  ;;         C : in Integer;
	  ;;
	  ;;        indenting C; stops on "in", moves to B, returns ;
	  ;;
	  ;; 2) probably other situations.
	  ;;
	  ;; This seems to work so far.
	  (goto-char (nth 1 parent)))

      ;; handle access-to-subprogram
      (while (and (member (nth 2 parent) `("procedure-spec" "function-spec")) ; _not -overriding -generic
		  (member (save-excursion (ada-smie-unrefined-token nil)) '("access" "protected")))
	(setq parent (ada-smie-goto-parent parent 2)))

      (cond
       ((member (nth 2 parent) '(":=" ":-object" "|"))
	;; Simple types in object declarations were handled above; for
	;; complex types and parameter lists, we get here.
	(ada-smie-skip-identifier-list nil))

       (t
	(if (= (point) (nth 1 parent))
	    ;; point is at statement start, but need to return info about previous token
	    (let ((stmt-pos (point))
		  (result-str (smie-default-backward-token))
		  result-pos)
	      (if (equal result-str "")
		  (progn
		    (setq result-str (ada-smie-token-special nil))
		    (setq result-pos (max (point-min) (- 1 (point)))))
		;; other token
		(setq result-pos (point))
		(goto-char stmt-pos))

	      (list result-pos result-str))

	  ;; point is at statement start, `parent' is previous token
	  (list (nth 1 parent) (nth 2 parent))))
       )
      )))

(defun ada-smie-rule-statement (offset child)
  "Find the start of the statement/declaration containing point (using
`ada-smie-goto-statement-start'), return an indent by OFFSET relevant
to it. Preserves point.  If CHILD is non-nil, point must be at
the start of CHILD, which must be a keyword."
  (save-excursion
    (let ((parent (ada-smie-goto-statement-start child)))
      (cond
       ((equal (nth 1 parent) "(")
	nil); indent to the (, not the line it is on
       (t (back-to-indentation)))
      (cons 'column (+ (current-column) offset))
      )))

;;;;
(defun ada-smie-rules (method arg)
  ;; This is called from ada-smie-{before|after}-keyword; it must
  ;; not move point, and must return nil or ('column n).
  (case method
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
      ((equal arg "(")
       ;; parenthesis occur in expressions, and after names, as array
       ;; indices, subprogram parameters, type constraints.
       ;;
       ;; "(" is not in the smie grammar, but
       ;; ada-smie-before-keyword handles is specially.
       ;;
       ;; 1) A subprogram declaration. Indent relative to "function"
       ;;    or "procedure", which are both parent and statement
       ;;    start.
       ;;
       ;; 2) The parameter list for an anonymous subprogram in an
       ;;    access type, we want to indent relative to "procedure" or
       ;;    "function", which is the parent, not "type-other", which is the
       ;;    statement start.
       ;;
       ;; 3) A procedure call:
       ;;
       ;;    function Function_1 return Integer
       ;;    is begin
       ;;       procedure_1
       ;;          (
       ;;
       ;;    we want to indent relative to the procedure name, which
       ;;    is the start of the current statement, while the parent
       ;;    is "function".
       ;;
       ;; 4) part of an expression. Indent relative to statement start.
       ;;
       ;; 5) an accept statement in a select statement
       ;;
       ;; 6) a function call in an aggregate or parameter list:
       ;;
       ;;       return Float
       ;;          (Integer'Value
       ;;             (Local_6));
       ;;
       ;; 7) An aggregate in an aggregate:
       ;;
       ;;    return Matrix'
       ;;       ((1, 2, 3),
       ;;        (4, 5, 6),
       ;;        (7, 8, 9));

       (save-excursion
	 (let ((pos (point))
	       (token (ada-smie-unrefined-token nil)))
	   (cond
	    ((member token '("procedure" "function")); 1, 2
	       (progn (goto-char pos) (ada-smie-rule-parent ada-indent-broken arg)))
	    ((equal token ","); 7
	     (progn (goto-char pos) (ada-smie-rule-statement 0 arg)))
	    (t ; 3, 4, 5
	     (progn (goto-char pos) (ada-smie-rule-statement ada-indent-broken arg))))
	 )))

      ((equal arg ")")
       ;; find the open paren
       (save-excursion
	 (forward-char 1)
	 (backward-sexp)
	 (cons 'column (current-column))))

      ((equal arg "=>-when")
       ;; exception to block statement rule.
       (ada-smie-rule-statement (ada-smie-when ada-indent-broken) arg))

      ((equal arg "abort-select")
       ;; exception to block-keyword indentation; preserve Ada mode 4.01 behavior
       (save-excursion
	 (ada-smie-goto-parent arg 1)
	 (back-to-indentation)
	 (cons 'column (+ (current-column) ada-indent-broken))))

      ((equal arg "end-record")
       ;; backward-statement-keyword leaves point on "record-type".
       (save-excursion
	 (ada-smie-backward-statement-keyword arg)
	 (back-to-indentation)
	 (cons 'column (current-column))))

      ((member arg '("record-type" "record-null"))
       ;; This indents the first line. The components are indented
       ;; relative to the line containing "record-type"; see "record-type" in
       ;; :after.
       (ada-smie-rule-statement ada-indent-record-rel-type arg))

      ((equal arg "renames-subprogram")
       (let* (keyword
	      ;; We need to skip "return" when looking for parameters
	      ;; if this subprogram is a function, so remember the
	      ;; keyword we found while calculating subprogram-col
	      ;; below.

	      (subprogram-col ;; the subprogram keyword's start column
	       (save-excursion
		 (setq keyword
		       (ada-smie-backward-tokens-unrefined
			"function" "procedure"))
		 (current-column)))
	      (parameter-col ;; the start of the parameters (nil if none)
	       (save-excursion
		 (if (equal keyword "function")
		     (ada-smie-backward-keyword));; "return"
		 (smie-backward-sexp)
		 (if (equal (char-after) ?\()
		     (current-column)
		   nil)))
	      )

	 (cond ((<= ada-indent-renames 0)
		;; If 'ada-indent-renames' is zero or negative, then
		(cond ((null parameter-col)
		       ;; No parameters; use 'ada-indent-broken' from
		       ;; subprogram keyword.
		       (cons 'column (+ subprogram-col ada-indent-broken)))
		      (t
		       ;; Parameters: indent relative to the "(".
		       (cons 'column (- parameter-col ada-indent-renames)))))
	       ;; If 'ada-indent-renames' is positive, indent from
	       ;; the subprogram keyword.
	       (t
		(cons 'column (+ subprogram-col ada-indent-renames))))
	 ))

      ((member arg '("return-spec" "return-formal"))
       ;; Function declaration, function body, or access-to-function
       ;; type declaration.

       (let ((parameter-start
	      ;; pos of start of the parameters (the paren; nil if none)
	      (save-excursion
		(smie-backward-sexp)
		(if (equal (char-after) ?\()
		    (current-column)
		  nil)))
             (function-col
	      ;; pos of the "function" keyword
	      (save-excursion
		(ada-smie-backward-keyword)
		(current-column)))
             )

         (cond
	  ((<= ada-indent-return 0)
	   (cond
	    ((null parameter-start)
	     ;; No parameters; use 'ada-indent-broken' from
	     ;; "function".
	     (cons 'column (+ function-col ada-indent-broken)))
	    (t
	     ;; Parameters: indent relative to the "(".
	     (cons 'column (- parameter-start ada-indent-return)))))

	  (t
	   ;; indent relative to "function"
	   (cons 'column (+ function-col ada-indent-return))))
         ))

      ((equal arg "overriding")
       (save-excursion
	 ;; this handles:
	 ;;
	 ;; not
	 ;; overriding
	 ;; procedure
	 ;;
	 (if (equal "not" (ada-smie-unrefined-token nil))
	     (cons 'column (current-column))
	   ;; else let :after handle it
	   nil)))

      ((member arg '("procedure-overriding" "function-overriding"))
       (save-excursion
         (let (col)
	   ;; this handles:
	   ;;
	   ;; not overriding
	   ;; procedure
	   (smie-default-backward-token)
	   (setq col (current-column))
	   (if (equal "not" (ada-smie-unrefined-token nil))
	       (setq col (current-column)))
           (cons 'column col))))

      ((member arg '("function-separate" "package-separate" "procedure-separate" "protected-separate"
		     "task-separate"))
       (ada-smie-rule-statement 0 arg))

      ((equal arg "when-case")
       (ada-smie-rule-statement ada-indent-when arg))

      ((equal arg "when-select")
       (save-excursion
	 (smie-default-backward-token)
	 ;; we are now before "or-select" or "select-open"
	 (cons 'column (+ (current-column) ada-indent-when))))

      ((equal arg "with-agg")
       (ada-smie-rule-statement 0 arg))

      ((member arg '("limited-context"
		     "private-context-1"
		     "with-context-1"))
       (cons 'column 0))

      ((or (member arg ada-smie-block-end-keywords)
	   (member arg ada-smie-block-keywords))
       ;; some example code:
       ;;
       ;;    function Function_1b return Float
       ;;    is
       ;;    begin
       ;;       declare
       ;;       begin
       ;;
       ;;    function Function_1b return Float
       ;;    is
       ;;    begin
       ;;       P1;
       ;;       declare
       ;;       begin
       ;;
       (save-excursion
	 (cond
	  ((ada-smie-opener-p arg)
	   ;; Indenting first keyword; declare, begin-open,
	   ;; etc. If there is a previous statement, indent 0 relative
	   ;; to it. Otherwise indent ada-indent relative to the block
	   ;; opening. That is handled by :after, so return nil here.
	   nil)

	  (t
	   ;; Indenting second or third keyword; is, begin-body,
	   ;; etc. Indent relative to block start. We use
	   ;; goto-statement, not goto-parent, to handle function
	   ;; returning anonymous access to function
	   ;; (test/ada_mode-nominal.adb Function_Access_11)
	   (ada-smie-rule-statement 0 arg))
	  )
	 ))

      ((save-excursion
	 (let* ((parent (ada-smie-goto-statement-start arg))
		(pos (point)))
	   (when (not (= pos (point)))
	     ;; Hanging; we are not at the start of a statement/declaration.
	     ;; Indent relative to the line the statement start is on.
	     (goto-char pos)
	     (cond
	      ((equal (nth 1 parent) "(")
	       nil); indent to the (, not the line it is on
	      (t (back-to-indentation)))

	     (cons
	      'column
	      (+ (current-column) ada-indent-broken))
	 ))))
      ))

;;;; :after
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
	 (ada-smie-rule-parent ada-indent-broken "(")))

      ((equal arg ",")
       ;; ada-mode 4.01 uses no indent after "," for multi-identifier in
       ;; aggregates, object declarations; broken-indent in
       ;; parameter lists. "with", "use" have their own
       ;; controlling options. We have to check for ":-object" first,
       ;; since otherwise a parameter list looks like an aggregate.
       ;; New in ada-mode 5.0: case expressions - see test/ada-mode-conditional_expressions.adb
       (let ((pos-tok-back (save-excursion (ada-smie-skip-identifier-list nil)))
	     pos-back token-back
	     (token-forw (nth 1 (save-excursion (ada-smie-skip-identifier-list t)))))

	 (setq token-back (nth 1 pos-tok-back))
	 (setq pos-back   (nth 0 pos-tok-back))
	 (cond
	  ((equal token-forw ":-object")
	   (cond
	    ((ada-in-paren-p); parameter list
	     (ada-smie-rule-statement ada-indent-broken arg))
	    (t ; object declaration
	     (ada-smie-rule-statement 0 arg))
	    ))

	  ((member token-back '("(" "with-agg"))
	   (ada-smie-rule-statement 0 arg))

	  ((equal token-back "use-decl"); also use-context
	   (ada-smie-rule-statement ada-indent-use arg))

	  ((member token-back '("with-context-1" "with-context-2"))
	   ;; match 4.01; indent by ada-indent-with relative to "with"
	   (cons 'column (+ (save-excursion (goto-char pos-back) (current-column)) ada-indent-with)))

	  ((equal token-forw "when-case")
	   (ada-smie-rule-keyword 0 token-back pos-back))

	  )))

      ((equal arg ";")
       (cond
	((save-excursion
	   (when (equal "record-type" (ada-smie-backward-keyword))
	     ;; we are indenting the line after:
	     ;;
	     ;;  for Record_Type_2 use record at mod 4;
	     ;;
	     ;; see "record-type" below
	     (back-to-indentation)
	     (cons 'column (+ (current-column) ada-indent)))))

	(t
	 (ada-smie-rule-statement 0 arg))))

      ((equal arg "=>-when")
       ;; exception to block statement rule
       (ada-smie-rule-statement (ada-smie-when ada-indent) arg))

      ((equal arg "|")
       ;; case or aggregate discrete choice
       (if (ada-in-paren-p)
	   (ada-smie-rule-statement ada-indent-broken arg)
	 (ada-smie-rule-statement (ada-smie-when ada-indent-broken) arg)))

      ((equal arg "private-library")
       (ada-smie-rule-statement 0 arg))

      ((equal arg "record-end")
       ;; We are indenting the aspect specification for the record.
       (back-to-indentation)
       (cons 'column (current-column)))

      ((equal arg "record-type")
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

      ((equal arg "when-case")
       ;; exception to block statement rule
       (ada-smie-rule-statement (ada-smie-when ada-indent-broken) arg))

      ((equal arg "is-case")
       ;; This rule must be after "when-case"
       ;;
       ;; Most likely this:
       ;;
       ;;   case Local_1 is
       ;;   -- comment after "is", before "when"
       ;;   when A =>
       (back-to-indentation)
       (cons 'column (+ (current-column) ada-indent-when)))


      ((equal arg "with-agg")
       (ada-smie-rule-statement 0 arg))

      ((member token '("with-context-1" "with-context-2"))
       (ada-smie-rule-statement ada-indent-with arg))

      ((member arg ada-smie-block-keywords)
       (ada-smie-rule-statement ada-indent arg))

      (t (ada-smie-rule-statement ada-indent-broken arg))
      ))
    ))

;;;;; smie-indent-functions
;;
;; each must not move point, and must return a column (as an integer) or nil.
;;
;; declared in order called

(defun ada-smie-comment-indent ()
  "For `comment-indent-function'."
  ;; This should only be called by comment-indent-new-line or
  ;; fill-comment-paragraph, so there will be a preceding comment line
  ;; that we can trust.
  (save-excursion
    (forward-comment -1)
    (if (looking-at comment-start)
	(current-column)
      (error "ada-smie-comment-indent called after non-comment"))))

(defun ada-smie-blank ()
  "Compute indentation of a blank line. For `smie-indent-functions', to handle adding new code."
  ;; Check to see if we are on a blank line
  (when
      (and (smie-indent--bolp)
	   (eolp))

    ;; yes, we are on a blank line; indent to previous code or comment.
    (let ((smie-indent-functions
	   (ada-smie-wrap-indent-functions
	    '(smie-indent-bob
	      ada-smie-after-keyword
	      ada-smie-default))))
      (smie-indent-calculate))
    ))

(defun ada-smie-comment ()
  "Compute indentation of a comment. For `smie-indent-functions'."
  ;; Check to see if we are at a comment
  (when
      (and (smie-indent--bolp)
	   (let ((pos (point)))
	     (save-excursion
	       (beginning-of-line)
	       (and (re-search-forward comment-start-skip (line-end-position) t)
		    (eq pos (or (match-end 1) (match-beginning 0)))))))

    ;; yes, we are at a comment; indent to previous code or comment.
    (save-excursion

     (cond
      ((and ada-indent-comment-col-0
	    (= 0 (current-column)))
       0)

      ((or
	(save-excursion (forward-line -1) (looking-at "\\s *$"))
	(save-excursion (forward-comment -1)(not (looking-at comment-start))))
       ;; comment is after a blank line or code; indent as if code
       ;;
       ;; indent-before-keyword will find the keyword _after_ the
       ;; comment, which could be 'private' for example, and that
       ;; would align the comment with 'private', which is wrong. So
       ;; we call a subset of the indentation functions.
       ;; ada-smie-default handles this case:
       ;;
       ;;     procedure Incorrect_Sub
       ;;       --  comment
       ;;       (

       (let ((smie-indent-functions
	      (ada-smie-wrap-indent-functions
	       '(smie-indent-bob
		 ada-smie-after-keyword
		 ada-smie-default))))
	 (smie-indent-calculate))
       )

      (t
       ;; comment is after a comment
       (forward-comment -1)
       (current-column))
      ))
    ))

(defun ada-smie-label ()
  "Indent a label. For `smie-indent-functions'."
  (cond
   ((save-excursion
      (or
       (equal "<<" (ada-smie-forward-token))
       (equal ":-label" (ada-smie-forward-token))))
    ;; before a label
    (let (offset
	  (token (save-excursion (ada-smie-backward-token))))
      (cond
       ((equal token "=>-when")
	(setq offset (ada-smie-when (+ ada-indent ada-indent-label))))

       ((member token ada-smie-block-keywords)
	(setq offset (+ ada-indent ada-indent-label)))

       (t (setq offset ada-indent-label)))

      (cdr (save-excursion
	     (ada-smie-backward-token)
	     (ada-smie-rule-statement offset token)))
      ))

   ((member (save-excursion (ada-smie-backward-token))
	    '(">>" ":-label"))
    ;; after a statement or block label
    (save-excursion
      (ada-smie-backward-token)
      (back-to-indentation)
      (- (current-column) ada-indent-label)))
   ))

(defun ada-smie-record()
  "Indent a line containing the \"record\" that starts a record component list.
 For `smie-indent-functions'."
  (catch 'quit
    (when (ada-in-paren-p)
      ;; might be inside discriminant parens; let other rules handle it
      (throw 'quit nil))

    (save-excursion
      (if (not (member (ada-smie-forward-token) '("type-other" "for-attribute")))
	  (let*
	      ((token (progn
			(goto-char (+ 1 (line-end-position)))
			;; forward-comment doesn't work from within a comment
			(forward-comment -1)
			(ada-smie-backward-token)))
	       (indent
		(if (equal token "record-type")
		    (ada-smie-rule-statement ada-indent-record-rel-type token))))

	    (cdr indent)
	    )))
    ))

(defun ada-smie-before-keyword()
  "Replacement for `smie-indent-keyword', tailored to Ada.
It requires `ada-smie-rule' to return nil or ('column column),
never just an offset (since we would not know what the offset was
relative to)."
  (save-excursion
    (forward-comment (point-max));; handle indenting blank line before code
    (let*
	((token (save-excursion (ada-smie-forward-token)))
	 char)

      (cdr
       (or
	(and
	 (equal token ""); ( ) "
	 ;; " is handled by ada-smie-default
	 (setq char
	       (case (char-after)
		 (?\( "(")
		 (?\) ")")
		 (?\" nil)))
	 (ada-smie-rules :before char))

	(and
	 (ada-smie-keyword-p token)
	 (ada-smie-rules :before token)))
       ))))

(defun ada-smie-after-keyword()
  "Replacement for `smie-indent-after-keyword', tailored to Ada.
It requires `ada-smie-rule' to return nil or ('column column),
never just an offset (since we would not know what the offset was
relative to)."
  (let*
      ((pos (point));; ada-smie-rules wants point at the start of its arg
       (token (ada-smie-backward-token))
       (indent
	(or
	 (and
	  (equal token ""); ( ) "
	  ;; ) " are handled by ada-smie-default; we only need to handle ( here.
	  (case (char-before)
	    (?\( t)
	    (?\) nil)
	    (?\" nil))
	  (ada-smie-rules :after "("))

	 (and
	  (ada-smie-keyword-p token)
	  (ada-smie-rules :after token)))))

      (goto-char pos)

      ;; Here we replace smie-indent--rules, so ada-smie-rules
      ;; cannot use smie-rule-parent; we use ada-smie-rule-parent
      ;; We do _not_ call smie-indent-virtual.
      (cdr indent)
      ))

(defun ada-smie-default ()
  "Unconditionally indent by `ada-indent-broken' from the current
statement start.  Intended to be the last item in
`smie-indent-functions', used when no indentation decision was
made."
  (cdr (ada-smie-rule-statement ada-indent-broken nil)))

;;;; other stuff

(defun ada-smie-goto-declaration-start (&optional declare)
  "Move point to start of declaration point is currently in or just after.
Return declaration name, set ff-function-name as needed by `ada-which-function'.
If DECLARE non-nil, stop at first containing declarative region (for 'declare' blocks)."
  (let (token)

    (when (ada-in-comment-p)
      ;; backward-token doesn't work from inside a comment.
      (beginning-of-line))

    (while
	(not
	 (member (setq token (ada-smie-backward-keyword))
		 (list
		  "function-overriding"
		  "function-spec"
		  "package-generic"
		  "package-plain"
		  "procedure-overriding"
		  "procedure-spec"
		  "protected-body"
		  "protected-type"
		  "task-body"
		  "task-single"
		  "task-type"
		  (when declare "declare")
		  nil))); bob, just in case we forgot something
      )

    (ada-smie-goto-statement-start token)

))

(defun ada-smie-goto-declarative-region-start ()
  "For `ada-goto-declarative-region-start', which see."
  (ada-smie-goto-declaration-start t)
  (let ((token (ada-smie-forward-token))
	(done nil))
    ;; FIXME: infinite loop on partial code?
    (cond
     ((equal token "declare")
      nil)
     (t
      (while (not (member token '("is-package" "is-protected_body" "is-subprogram_body" "is-task_body")))
	(smie-default-backward-token)
	(setq token (ada-smie-forward-statement-keyword token))))
     )))

(defun ada-smie-forward-statement-keyword-1 ()
  "For `ada-forward-statement-keyword'."
  (let ((start (point)))
    (when (ada-in-string-or-comment-p)
      (end-of-line))
    (ada-smie-forward-statement-keyword (save-excursion (ada-smie-forward-keyword)))
    (cond
     ((= start (point))
      ;; already on last keyword in current statement. Goto next
      ;; keyword; either in containing statement or next statement.
      (smie-default-forward-token)
      (ada-smie-forward-keyword)
      (smie-default-backward-token))

     (t
      ;; forward-statement-keyword leaves point following found keyword
      (ada-smie-backward-token))
     )))

(defun ada-smie-backward-statement-keyword-1 ()
  "For `ada-backward-statement-keyword'."
  ;; FIXME: match forward logic, test
  (ada-smie-backward-statement-keyword (save-excursion (ada-smie-forward-keyword))))

(defun ada-smie-which-function ()
  "For `ada-which-function', which see."
  ;; FIXME: If point is in a local subprogram, package, or protected
  ;; type, or after a spec for same, that name is returned. That may
  ;; be what the user wants for 'which-name-function' mode-line
  ;; display, but they may want the selected name, starting with the
  ;; file level compilation unit. Need an option?
  ;;
  ;; For ada-find-other-file, we want the declaration that is directly
  ;; contained within the file level compilation unit. Need a
  ;; parameter to distinguish the two?
  ;;
  ;; One solution for both cases is to parse up to the top level,
  ;; remembering the names along the way. Another is to mark each
  ;; declaration with a text property giving the selected name.
  (save-excursion
    (let (token
	  (done nil)
	  (result nil)
	  (symbol-end
	   ;; we can't just add \> here; that might match _ in a user modified ada-mode-syntax-table
	   "\\([ \t]+\\|$\\)")
	  )

      (ada-smie-goto-declaration-start)

      (while (not done)
	(setq token (ada-smie-forward-token))
	(cond
	 ((equal token "package-plain")
	  (setq token (ada-smie-forward-token))
	  (if (equal token "body")
	      (progn
		(setq result (ada-smie-forward-name))
		(when (not ff-function-name)
		  (setq ff-function-name
			(concat
			 "package\\s-+"
			 result
			 symbol-end))))
	    ;; not body
	    (ada-smie-backward-token)
	    (setq result (ada-smie-forward-name))
	    (when (not ff-function-name)
	      (setq ff-function-name
		    (concat
		     "package\\s-+body\\s-+"
		     result
		     symbol-end))))
	  (setq done t))

	 ((equal token "protected-body")
	  (setq token (ada-smie-forward-token)); body
	  (setq result (ada-smie-forward-name))
	  (when (not ff-function-name)
	    (setq ff-function-name
		  (concat
		   "protected\\s-+\\(type\\s-+\\)?"
		   result
		   symbol-end)))
	  (setq done t))

	 ((equal token "protected-type")
	  (when (equal "type-protected" (save-excursion (ada-smie-forward-token)))
	    (setq token (ada-smie-forward-token)))
	  (setq result (ada-smie-forward-name))
	  (when (not ff-function-name)
	    (setq ff-function-name
		  (concat
		   "protected\\s-+body\\s-+"
		   result
		   symbol-end)))
	  (setq done t))

	 (t
	  (when (not (ada-smie-keyword-p token))
	    ;; check for selected name
	    (while (eq (char-after) ?.)
	      (if result
		  (setq result (concat result "." token))
		(setq result token))
	      (forward-char 1)
	      (setq token (smie-default-forward-token)))
	    (if result
		(setq result (concat result "." token))
	      (setq result token))
	    (setq done t)))
	 ))

      (if (not ff-function-name)
	  (setq ff-function-name
		;; use a regexp that won't match similarly named items
		(concat
		 "^"
		 (buffer-substring-no-properties (point-at-bol) (point))
		 symbol-end)))
    result)))

(defun ada-smie-in-paramlist-p ()
  "For `ada-in-paramlist-p'."
  (when (ada-in-paren-p)
    ;; We could be in:
    ;; 1) aggregate
    ;; 2) discriminant list
    ;; 3) parenthesized expression (including if, case)
    ;; 4) subprogram call
    ;; 5) type constraint
    ;; 6) parameter list
    ;;
    ;; Discriminant list and parameter list are the only ones that
    ;; contain ":-object". We distinguish between them by looking for
    ;; "type".
    (save-excursion
      (let (result
	    token
	    (done nil))
	(ada-goto-open-paren 1)

	(while (not done)
	  (setq token (ada-smie-forward-keyword))
	  (cond
	   ((equal ":-object" token)
	    (setq done t)
	    (setq result t))

	   ((equal ")" token)
	    (setq done t)
	    (setq result nil))))
	(when result
	  (ada-goto-open-paren)
	  (ada-smie-goto-statement-start nil)
	  (not (equal "type" (ada-smie-unrefined-token t)))
	  )))))

(defun ada-smie-scan-paramlist (begin end)
  "For `ada-scan-paramlist'."
  (goto-char begin)
  (let (token
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
      (setq token (ada-smie-forward-token))
      (cond
       ((equal token ",") nil);; multiple identifiers

       ((equal token ":-object")
	;; identifiers done. skip mode; there may be none
	(skip-syntax-forward " ")
	(setq type-begin (point))
	(save-excursion
	  (while (member (ada-smie-unrefined-token t) '("in" "out" "not" "null" "access" "constant" "protected"))
	    (skip-syntax-forward " ")
	    (setq type-begin (point)))))

       ((equal token "in") (setq in-p t))
       ((equal token "out") (setq out-p t))
       ((and (not type-end)
	     (member token '("not" "null")))
	;; "not", "null" could be part of the default expression
	(setq not-null-p t))
       ((equal token "access") (setq access-p t))
       ((equal token "constant") (setq constant-p t))
       ((equal token "protected") (setq protected-p t))

       ((equal token ":=")
	(setq type-end (save-excursion (backward-char 2) (skip-syntax-backward " ") (point)))
	(skip-syntax-forward " ")
	(setq default-begin (point))
	(ada-smie-forward-tokens-unrefined ";")
	(unless (equal token ")")
	  (ada-smie-backward-token-unrefined)))

       ((member token '(";" ""))
	;; one param done
	(if (equal token "")
	    ;; all done
	    (progn
	      (setq done t)
	      (when (not type-end) (setq type-end (point)))
	      (when default-begin (setq default (buffer-substring default-begin (point))))
	      )
	  (when (not type-end) (setq type-end (1- (point))))
	  (when default-begin (setq default (buffer-substring default-begin (1- (point)))))
	  )

	(setq type (buffer-substring type-begin type-end))
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
	      (add-to-list 'identifiers token)
	    (setq identifiers (list token)))))
       ))
    paramlist))

(defun ada-smie-context-clause ()
  "For `ada-fix-context-clause'."
  (save-excursion
    (let ((begin nil)
	  (end nil)
	  keyword)
      (goto-char (point-min))
      (forward-comment (point-max))
      (while (not end)
	(setq keyword (ada-smie-forward-keyword))
	(cond
	 ((member keyword '("use-decl" ";")) nil)
	 ((member keyword '("with-context-1" "limited-context" "private-context-1"))
	  (when (not begin)
	    (setq begin (point-at-bol))))
	 (t
	  ;; start of compilation unit
	  (setq end (progn (smie-default-backward-token) (point)))
	  (unless begin
	    (setq begin end)))
	 ))
      (cons begin end)
    )))

(defun ada-smie-make-subprogram-body ()
  "For `ada-make-subprogram-body'."
  (let ((name (progn (ada-smie-forward-token) (ada-smie-forward-name)))
	(token (ada-smie-forward-tokens-unrefined "is" ";"))
	begin)

    (cond
     ((equal token "is")
      (error "%s is a subprogram body, not spec" name))

     (t
      (delete-char -1); ';'
      (newline)
      (setq begin (point))
      (insert " is begin\n\nend ")
      (insert name)
      (insert ";\n")
      (indent-region begin (point))
      (forward-line -2)
      ;; indent-region does not indent blank lines
      (smie-indent-line)
      )
    )))

;;;; parser debug
(defvar ada-smie-debug nil
  "When non-nil, `ada-smie-show-keyword-forward' and
`ada-smie-show-keyword-backward' invalidate cache first, so
they always run the refine algorithm.  In addition,
`ada-smie-refine-error' throws errors instead of failing more
gracefully.")

(defun ada-smie-show-keyword-forward ()
  "Show the grammar info for word following point, and move across it."
  (interactive)
  (when ada-smie-debug
      (setq ada-smie-cache-max (min ada-smie-cache-max (- (point) 1))))
  (let ((token (ada-smie-forward-token)))
    (while
	(cond
	 ((equal token "")
	  (cond
	   ((equal (char-after) ?\ ) (skip-syntax-forward " ") t)

	   ((or
	     (equal (char-after) ?\()
	     (equal (char-after) ?\"))
	    (ada-smie-skip-lowlevel-sexp t)
	    t)

	   ((equal (char-after) ?\)) (forward-char 1) t)

	   (t nil); eob
	   ))
	 ((message "%s" (assoc token ada-smie-grammar))
	  nil)))))

(defun ada-smie-show-keyword-backward ()
  "Show the grammar info for word preceding point, and move across it."
  (interactive)
  (when ada-smie-debug
    (save-excursion
      (smie-default-backward-token)
      (setq ada-smie-cache-max (min ada-smie-cache-max (- (point) 1)))))
  (message "%s" (assoc (ada-smie-backward-token) ada-smie-grammar)))

(defun ada-smie-show-prev-keyword ()
  "Move to previous keyword in same statement."
  (interactive)
  (ada-smie-backward-statement-keyword (save-excursion (ada-smie-forward-token))))

(defun ada-smie-show-parent ()
  "Move backward to the parent of the word following point, and show its refined keyword and grammar levels."
  (interactive)
  (let* ((token (save-excursion (ada-smie-forward-token)))
	 (count (if (ada-smie-opener-p token) 2 1))
	 (toklevels (ada-smie-goto-parent token count)))
    (message "%s => %s" (assoc token ada-smie-grammar) toklevels)))

(defun ada-smie-show-child ()
  "Move forward to the child of the word following point, and show its refined keyword and grammar levels."
  (interactive)
  (let* ((token (save-excursion (ada-smie-forward-token)))
	 (toklevels (smie-forward-sexp token)))
    (message "%s => %s" (assoc token ada-smie-grammar) toklevels)))

(defun ada-smie-show-sexp-token ()
  "Move backward one sexp, starting with refined token after point."
  (interactive)
  (let* ((token (save-excursion (ada-smie-forward-token)))
	 (toklevels (smie-backward-sexp token)))
    (message "%s => %s" (assoc token ada-smie-grammar) toklevels)))

(defun ada-smie-show-statement-start ()
  "Move to the start of the current statement."
  (interactive)
  ;; this is the way ada-smie-goto-statement-start is called during indentation; better for testing
  (let ((token (save-excursion (ada-smie-forward-token)))
	parent)
    (setq parent (ada-smie-goto-statement-start (if (equal token "") "(" token)))
    (message "%s => %s" token (nth 1 parent))))

(defun ada-smie-show-cache ()
  "Show cache at point."
  (interactive)
  (message "%s" (ada-smie-get-cache (point))))

(defun ada-smie-wrapper (indent-function)
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

(defmacro ada-smie-wrap (func)
  `(lambda() (ada-smie-wrapper ',func)))

(defun ada-smie-wrap-indent-functions (functions)
  "If `ada-smie-debug' is non-nil, wrap contents of FUNCTIONS with `ada-smie-wrapper'. Return new list.
This lets us know which indentation function succeeded."
  (if ada-smie-debug
      (let (res func)
	(while (setq func (pop functions))
	  (let ((newfunc (cadr (macroexpand `(ada-smie-wrap ,func)))))
	    (setq res (cons newfunc res))))
	(reverse res))
    functions))

(defun ada-smie-debug-keys ()
  "Add debug key definitions to `ada-mode-map'."
  (interactive)
  (define-key ada-mode-map "\M-o" 'ada-smie-show-parent)
  (define-key ada-mode-map "\M-9" 'ada-smie-show-prev-keyword)
  (define-key ada-mode-map "\M-p" 'ada-smie-show-statement-start)
  (define-key ada-mode-map "\M-i" 'ada-smie-show-keyword-backward)
  (define-key ada-mode-map "\M-j" 'ada-smie-show-cache)
  (define-key ada-mode-map "\M-k" 'ada-smie-show-keyword-forward)
  )

;;;; parser setup

(defun ada-smie-setup ()

  ;; We don't need most of the functions in the default value for
  ;; smie-indent-functions, so we specify it directly here.
  ;;
  ;; smie-indent-comment lines up comments with following code. That
  ;; means they line up with 'end', which is wrong.
  ;;
  ;; There are times (like at 'end') when it is very simple to figure
  ;; out the indent when looking at a keyword, and much harder when
  ;; looking at the previous keyword, so we do
  ;; ada-smie-before-keyword before ada-smie-after-keyword.
  ;;
  ;; We started out trying to use smie-indent-keyword and
  ;; smie-indent-after-keyword. However, there are too many cases when
  ;; they do the wrong thing for Ada. We keep the same overall
  ;; structure of the code; ada-smie-before-keyword calls
  ;; ada-smie-rules in the same way, except it assumes
  ;; ada-smie-rules returns a column, not an offset.
  ;;
  ;; ada-smie-default works better for Ada code than
  ;; smie-indent-exps.

  (make-local-variable 'smie-indent-functions)

  (setq smie-indent-functions
     (ada-smie-wrap-indent-functions
      '(smie-indent-bob; handle first non-comment line in buffer
	ada-smie-blank
	ada-smie-label
	ada-smie-comment
	ada-smie-record
	ada-smie-before-keyword
	ada-smie-after-keyword
	ada-smie-default)
       ))

  (smie-setup ada-smie-grammar #'ada-smie-rules
	      :forward-token #'ada-smie-forward-token
	      :backward-token #'ada-smie-backward-token)

  (set (make-local-variable 'smie-skip-associative) t)
  ;; we don't want `smie-backward-sexp' to stop at weird places
  ;; FIXME (later): this var is in a local patch that won't be in main; do something else
  ;; either cope with the weird places in goto-parent, or rewrite smie-backward-sexp

  (setq post-self-insert-hook (delete 'smie-blink-matching-open post-self-insert-hook))
  ;; smie-setup puts smie-blink-matching-open on
  ;; post-self-insert-hook; it is broken when used interactively!

  (add-hook 'after-change-functions 'ada-smie-after-change nil t)

  (set (make-local-variable 'comment-indent-function) 'ada-smie-comment-indent)

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
     '(2 (if (member (progn
		       (when (not (ada-in-string-or-comment-p))
			 (ada-smie-validate-cache (match-beginning 1))
			 (ada-smie-get-cache (match-beginning 1))))
		    '("return-spec" "return-formal"))
	     font-lock-type-face
	   'default)
	 nil t)
     )))

  (set (make-local-variable 'ada-which-function) 'ada-smie-which-function)
  (set (make-local-variable 'ada-in-paramlist-p) 'ada-smie-in-paramlist-p)
  (set (make-local-variable 'ada-scan-paramlist) 'ada-smie-scan-paramlist)
  (set (make-local-variable 'ada-goto-declaration-start) 'ada-smie-goto-declaration-start)
  (set (make-local-variable 'ada-goto-declarative-region-start) 'ada-smie-goto-declarative-region-start)
  (set (make-local-variable 'ada-next-statement-keyword) 'ada-smie-forward-statement-keyword-1)
  (set (make-local-variable 'ada-prev-statement-keyword) 'ada-smie-backward-statement-keyword-1)
  (set (make-local-variable 'ada-make-subprogram-body) 'ada-smie-make-subprogram-body)
  )

(add-hook 'ada-mode-hook 'ada-smie-setup)

(provide 'ada-smie)
(provide 'ada-indent-engine)

;; end of file
