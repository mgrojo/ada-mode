;;; Ada mode indentation engine, based on SMIE
;;
;; FIXME (later): not using lexical-binding because we might port this back to Emacs 23
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

;;;; code

(eval-when-compile (require 'cl)); 'case'
(require 'smie)

;;; user variables

(defgroup ada-indentation nil
  "Indentation options for Ada source."
  :group 'ada)

(defcustom ada-indent 3
  "*Size of Ada default indentation, when no other indentation is used.

Example :
procedure Foo is
begin
>>>null;"
  :type 'integer  :group 'ada-indentation)

(defvar ada-broken-indent nil)
(make-obsolete-variable
 'ada-broken-indent
 'ada-indent-broken
 "Emacs 24.4, Ada mode 5.0"
 'set)
;; FIXME (later): this doesn't warn user at runtime, but at least they should
;; notice something broke, and the help will be useful.

(defcustom ada-indent-broken 2
  "*Indentation for the continuation of a broken line.

Example :
   My_Var : My_Type :=
   >>(Field1 => Value);"
  :type 'integer :group 'ada-indentation)
(defun ada-indent-broken () (or ada-indent-broken ada-broken-indent))

(define-obsolete-variable-alias
 'ada-label-indent
 'ada-indent-label
 "Emacs 24.4, Ada mode 5.0")
;; FIXME (later): this doesn't warn user at runtime, and they won't notice
;; something is wrong until we delete it, and then there won't be any
;; useful help.

(defcustom ada-indent-label -3
  ;; Ada mode 4.01 and earlier default this to -4. But that is
  ;; incompatible with the default gnat indentation style check, which
  ;; wants all indentations to be a multiple of 3 (with some
  ;; exceptions). So we default this to -3.
  "*Indentation for a loop, block, or statement label, relative to the item it labels.

Example :
   Label_1 :
   <<<<declare

   <<Label_2>>
   <<<<Foo := 0;"
  :type 'integer :group 'ada-indentation)

(defcustom ada-indent-record-rel-type 3
  "*Indentation for 'record' relative to 'type' or 'use'.

An example is:
   type A is
   >>>record"
  :type 'integer :group 'ada-indent)

(defcustom ada-indent-renames 2 ;; FIXME (fixed in sjw): not currently used
  "*Indentation of 'renames' relative to the matching subprogram declaration start.
If `ada-indent-renames' is zero or less, the indentation is done relative to
the open parenthesis (if there is no parenthesis, `ada-indent-broken' is used).

An example is:
   function A (B : Integer)
       return C;
   >>renames Foo;"
:type 'integer :group 'ada-indent)

(defcustom ada-indent-return 0
  "*Indentation for 'return' relative to the matching 'function' statement.
If `ada-indent-return' is zero or less, the indentation is done relative to
the open parenthesis (if there is no parenthesis, `ada-indent-broken' is used).

An example is:
   function A (B : Integer)
   >>>>>>>>>>>return C;"
:type 'integer :group 'ada-indent)

(defvar ada-use-indent nil)
(make-obsolete-variable
 'ada-use-indent
 'ada-indent-use
 "Emacs 24.4, Ada mode 5.0"
 'set)
(defcustom ada-indent-use ada-indent-broken
  "*Indentation for the lines in a 'use' statement.

An example is:
   use Ada.Text_IO,
   >>Ada.Numerics;"
  :type 'integer :group 'ada)

(defvar ada-when-indent nil)
(make-obsolete-variable
 'ada-when-indent
 'ada-indent-when
 "Emacs 24.4, Ada mode 5.0"
 'set)

(defcustom ada-indent-when (or ada-when-indent 3)
  "*Indentation for 'when' relative to 'exception', 'case', 'or' in select.

An example is:
   case A is
   >>>when B =>"
  :type 'integer :group 'ada-indent)

(defvar ada-with-indent nil)
(make-obsolete-variable
 'ada-with-indent
 'ada-indent-with
 "Emacs 24.4, Ada mode 5.0"
 'set)

(defcustom ada-indent-with ada-indent-broken
  "*Indentation for the lines in a 'with' statement.

An example is:
   with Ada.Text_IO,
   >>Ada.Numerics;"
  :type 'integer :group 'ada)

;;; grammar

(defconst ada-indent-grammar
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
       ("for-attribute" name "use-attribute" expression))
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
       ("limited-context" "private-context-2" "with-context")
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
       ;; affect indentation.
       (name)
       (aggregate)
       (name "with-agg" name))
      ;; The actual syntax for extension_aggregate is more complex,
      ;; but all we really need is for "with-agg" to be in the
      ;; grammar.

      ;; Formal generic parameters. Most formal_* are covered in this
      ;; grammar by the equivalent non-formal syntax.

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
	"package-generic" identifier "is-package" declarations "private-body" declarations "end-block")
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
       ;; ada-indent-label can handle it.

       ("for-loop" identifier "in" name "loop-body" statements "end-loop" "loop-end")
       ;; FIXME (later): container iterators allow ":" in a for-loop iteration scheme (not tested yet)
       ("while" identifier "in" name "loop-body" statements "end-loop" "loop-end")
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
       ("package-plain" name "is-package" declarations "private-body" declarations "end-block")
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
       ;; identifier. FIXME (later): if expressions.

       ;; label_statement
       ("<<" identifier ">>")
       ;; In Ada, this is _not_ followed by ";"; it labels the
       ;; following statement. This grammar doesn't care;
       ;; `ada-indent-goto-statement-start' from the terminal ";" of a
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
       ("function-spec" name "return-spec" "is-subprogram_body" "separate-stub")
       ("procedure-spec" name "is-subprogram_body" declarations "begin-body" statements "end-block")
       ("procedure-spec" name "is-subprogram_body" "separate-stub")
       ("separate-unit" "function-separate" name "return-spec" name "is-subprogram_body" declarations "begin-body"
	statements "end-block")
       ("separate-unit" "procedure-separate" name "is-subprogram_body" declarations "begin-body"
	statements "end-block"))

      (subprogram_declaration
       ("function-spec" name "return-spec" name)
       ;; trailing name makes "return-spec" have the same binding as
       ;; in subprogram_body; that avoids recursion between refine-is
       ;; and refine-return. Covers subprogram_renaming_declaration function

       ("overriding" "function-overriding" name "return-spec" name)
       ("overriding" "procedure-overriding" name)
       ;; We need "overriding" as a keyword, because the indentation
       ;; policy for it is an exception to the hanging policy:
       ;;
       ;;    overriding
       ;;    procedure (...);

       ("procedure-spec" name); same as 'procedure name is-subprogram_body'
       ;; Covers subprogram_renaming_declaration procedure.
       ;;
       ;; We leave out ("procedure" name "is" "null") here; we
       ;; are treating a couple of occurences of "is", and most
       ;; occurences of "null", as identifiers.
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
       ("type-other" identifier "is-type" "new-type" name "with-record")
       ;; We refine "with" to "with-record" when it is followed
       ;; by "record", so that it is a closer to match
       ;; "type", since "record-open" is an opener.
       ;;
       ;; We don't include record-definition in
       ;; derived_type_definition, because we want to indent "end
       ;; record" relative to "record".

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
       ;; only one keyword.  We need "is-type-record" when this is
       ;; followed by a record_definition; that's covered below. We don't need "tagged" as a keyword.
       ("type-other" identifier "is-type-tagged")

       ;; interface_type_definition, formal_interface_type_definition
       ("type-other" identifier "is-type" "interface-plain")
       ("type-other" identifier "is-type" "interface-and" "and-interface" interface_list)

       ;; modular_type_definition
       ("type-other" identifier "is-type" "mod-type")
       ;; FIXME (later): this is the only occurance of "mod" in the grammar at
       ;; the moment. It also occurs in other syntax that is not
       ;; implemented yet; wait until we test those to decide to leave
       ;; it as an identifier.

       ;; private_extension_declaration
       ("type-other" identifier "is-type" "new-type" name "with-new" "private-with")
       ("type-other" identifier "is-type" "new-type" interface_list "with-new" "private-with")
       ;; leaving 'with' and 'private' as separate tokens causes conflicts

       ;; private_type_declaration
       ("type-other" identifier "is-type" "private-type-spec")

       ;; protected_type_declaration, single_protected_declaration
       ;;
       ("protected-type" "type-protected" identifier "is-type-block" declarations
	"private-body" declarations "end-block")
       ("protected-type" "type-protected" identifier "is-type-block" declarations "end-block")
       ("protected-type" "type-protected" identifier "is-type" "new-type" interface_list "with-new" declarations
	"private-body" declarations "end-block")

       ;; record_type_definition
       ("type-other" identifier "is-type" "record-null")
       ("type-other" identifier "is-type-record")
       ;; We refine "is-type" to "is-type-record" when it is followed
       ;; by "record-open", so that it is a closer to match "type", since
       ;; "record-open" is an opener.

       ;; record_definition
       ("record-open" declarations "end-record" "record-end")
       ("record-open" declarations "end-record" "record-end-aspect" "with-aspect" aspect_list)
       ;; No need to distinguish between 'declarations' and
       ;; 'component_list'. We don't include record_definition in
       ;; record_type_definition or derived_type_definition, because
       ;; we want to indent "end record" relative to "record", not
       ;; "type".

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

      )); smie-bnf->prec2
    ))

;;; utils for refine-*, forward/backward token

(defconst ada-indent-block-keywords
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
    "private-body"
    "record-open"
    "select-open"
    "then-select"
    "then-if")
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
    "function-generic"
    "or-select"
    "package-generic"
    "procedure-generic"
    "when-case",
    "when-select"
    )
  "Keywords that always end indented blocks.")

(defconst ada-indent-pre-begin-tokens
  '("declare"
    "is-subprogram_body"
    "is-package"
    "is-task_body"
    "is-entry_body")
  ;; found by searching [1] Annex P for "begin", then checking for
  ;; refinements. Thus in Annex P order.
  "All refined tokens that are followed by \"begin\" in an Ada declaration.")

(defconst ada-indent-labeled-unrefined-keywords
  '("begin"
    "declare"
    "for"
    "loop"
    "while")
  "Unrefined keywords that can be preceded by a label.")

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

(defun ada-indent-error (message)
  (error
   "%s: %s %d"
   message
   (buffer-substring-no-properties
    (progn (beginning-of-line) (point))
    (progn (end-of-line) (point)))
   (point)))

(defun ada-indent-generic-p ()
  "Assuming point is at the start of a package or subprogram
spec, return t if is a generic, nil otherwise."
  (save-excursion
    ;; Scan back over things that might be generic formal
    ;; parameters. If we find a definite formal param (has -formal in
    ;; the refined keyword), we're done. If we find "generic", we're
    ;; done. If we find something else that can't be a formal
    ;; parameter (ie package start), we're done.
    (let ((token (ada-indent-backward-token))
	  stmt-or-decl
	  (result 'not-found)
	  (first t))
      (while (eq result 'not-found)
	(cond
	 ((equal token ""); bob
	  (setq result nil))

	 ((equal token ";")
	  (if first
	      (progn
		(setq first nil)
		;; try again
		(ada-indent-goto-statement-start token)
		(setq token (ada-indent-backward-token)))

	    ;; we've skipped a statement or declaration; see if we can tell which
	    (save-excursion
	      (smie-default-forward-token)
	      (setq stmt-or-decl (ada-indent-statement-or-decl)))

	    (ecase (car stmt-or-decl)
	      (statement
	       (ada-indent-error "found statement preceding package or subprogram spec"))

	      (formal
	       (setq result t))

	      (declaration
	       ;; There are only a two non-formal declarations that
	       ;; can occur in a generic formal parameter list; formal_object_declaration, formal_type_declaration.
	       (if (not (member (cadr stmt-or-decl) '(":-object" "type-other")))
		   (setq result nil)
		 ;; try again
		(ada-indent-goto-statement-start token)
		(setq token (ada-indent-backward-token))))

	      (unknown
	       ;; try again
	       (ada-indent-goto-statement-start token)
	       (setq token (ada-indent-backward-token)))
	      )))

	 ((equal token "private-library")
	  (setq result nil))

	 ((equal token "generic")
	  (setq result t))

	 ((member token ada-indent-block-keywords)
	  (setq result nil))

	 (t
	  (if ada-indent-debug-refine
	      (ada-indent-error "ada-indent-generic-p: unexpected statement or prev keyword")
	    ;; user is probably editing code
	    (setq result nil)))

	 ));; while
      result
      )))

(defun ada-indent-statement-or-decl ()
  "Assuming point is at the start of a statement, a normal
declaration, or a generic formal declaration, examine a few
refined tokens following point to see if we can determine which.
Return (class token), where `class' is 'statement, 'declaration,
'formal, or 'unknown; `token' is the determining token or nil.
Preserves point."
  ;; Note the only way to know we are at the start of a statement or
  ;; decl is because we've just skipped over it backwards. For
  ;; example, while refining "begin", or "package" looking for "generic".
  (save-excursion
    (catch 'quit
      ;; find the first keyword
      (let ((token (ada-indent-forward-token)))
	(when (not (ada-indent-keyword-p token))
	  (cond
	   ((equal token "pragma"); not worth making this a keyword
	    (throw 'quit (list 'declaration token)))
	   (t (setq token (ada-indent-forward-keyword)))))

	;; lists compiled by going thru (statement ..) and (declaration
	;; ...) in the ada-indent-grammar declaration above.

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
	     "task-type" "type-other" "use-attribute" "use-decl"
	     "limited-context" "private-context" "with-context"))
	      ;; context clause is not really a declaration, but this works for us.
	  (list 'declaration token))

	 ((equal token "with-formal")
	  (list 'formal token))

	 (t (list 'unknown nil)))))))

(defun ada-indent-skip-param_list (forward)
  ;; While refining tokens, we don't want to call smie-next-sexp,
  ;; because it relies on refined tokens. So we call the C scanner
  ;; directly when we need to skip a parenthesis (see the lisp source
  ;; for forward-sexp).
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

(defun ada-indent-next-keyword (next-token forward)
  "Skip tokens function NEXT-TOKEN, until a keyword is found (a
token defined in the grammar).  Skips string literals, character
literals, paired parens.  Stops at left paren going backwards,
right paren going forwards.  Return the keyword or paren (which
may be the first token found); point is beyond the keyword (after
for `forward' t, before otherwise).  Return empty string if
encounter beginning or end of buffer."
  (let (token)
    (catch 'quit
      (while
	  (progn
	    (setq token (funcall next-token))
	    (if (equal "" token)
		;; We hit a paren, string, character literal, bob, eob
		(progn
		  (when (or (bobp) (eobp)) (throw 'quit nil))
		  (if forward
		      (when (eq (char-after) ?\))
			(forward-char 1)
			(throw 'quit ")"))
		    (when (eq (char-before) ?\()
		      (backward-char 1)
		      (throw 'quit "(")))
		  (ada-indent-skip-param_list forward)
		  ;; the next token might be another paren, so we loop
		  t)
	      ;; a token
	      (setq token (nth 0 (assoc token smie-grammar)))
	      (not token); not a keyword
	      )))
      token)
    ))

(defun ada-indent-backward-keyword ()
  (ada-indent-next-keyword 'ada-indent-backward-token nil))

(defun ada-indent-forward-keyword ()
   (ada-indent-next-keyword 'ada-indent-forward-token t))

(defun ada-indent-next-token-unrefined (next-token forward)
  "Move to the next token using function NEXT-TOKEN. Skips parentheses.
Return the token, or wrong paren, or empty string if encounter beginning of
buffer."
  (let (token)
    (while
	(progn
	  (setq token (funcall next-token))
	  (if (equal "" token)
	      ;; We hit a parenthesis, bob, eob, string, char literal
	      (progn
		(when (bobp) (throw 'quit nil))
		(when (eobp) (throw 'quit nil))
		(if forward
		    (when (eq (char-after) ?\)) (throw 'quit ")"))
		  (when (eq (char-before) ?\() (throw 'quit "(")))
		(ada-indent-skip-param_list forward); also skips strings, char literals
		))))
    token))

(defun ada-indent-backward-token-unrefined ()
  (ada-indent-next-token-unrefined 'smie-default-backward-token nil))

(defun ada-indent-backward-tokens-unrefined (&rest targets)
  "Move backward over unrefined tokens, strings and parens. Stop
when found token is an element of TARGETS, return that token."
  (let (result)
    (while (not (member (setq result (ada-indent-backward-token-unrefined))
			targets)))
    result))

(defun ada-indent-forward-tokens-unrefined (&rest targets)
  "Move forward over unrefined tokens, strings and parens. Stop
when found token is an element of TARGETS, return that token."
  (let (result)
    (while (not (member (setq result (ada-indent-next-token-unrefined 'smie-default-forward-token t))
			targets)))
    result))

(defconst ada-indent-type-modifiers '("abstract" "tagged" "limited"))

(defun ada-indent-forward-type-modifiers ()
  "Skip forward tokens that are in `ada-indent-type-modifiers', return the following token."
  (let (result)
    (while (member (setq result (smie-default-forward-token)) ada-indent-type-modifiers))
    result))

(defun ada-indent-backward-type-modifiers ()
  "Skip backward tokens that are in `ada-indent-type-modifiers', return the preceding token."
  (let (result)
    (while (member (setq result (smie-default-backward-token)) ada-indent-type-modifiers))
    result))

(defun ada-indent-refine-error (token)
  (if ada-indent-debug-refine
      (ada-indent-error (concat "unrecognized '" token "'"))
    ;; else return the unrefined keyword. Indentation will be wrong,
    ;; but this is more friendly to the user.
    token))

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
    (if (member token ada-indent-labeled-unrefined-keywords)
	":-label"
      ":-object")))

(defun ada-indent-refine-=> (token forward)
  (let ((token (save-excursion
		 (when forward (smie-default-backward-token))
		 (ada-indent-backward-keyword))))

    (if (member token '(":-object" ; in exception handler
			"|"
			"when-case"
			"when-select"))
	"=>-when"
      "=>-other")))

(defun ada-indent-refine-abort (token forward)
  (let ((token (save-excursion
		 (when forward (smie-default-backward-token))
		 (ada-indent-backward-token))))

    (if (equal token "then-select")
	"abort-select"
      "abort")))

(defun ada-indent-refine-accept (token forward)
  (let ((token (save-excursion
		 (when (not forward) (smie-default-forward-token))
		 (ada-indent-forward-keyword);; identifier, entry family, parameters; returns ";", "do"
		 )))
    (cond
     ((equal token "do") "accept-open")
     ((equal token ";")  "accept"); identifier
     (t (ada-indent-refine-error "accept")))))

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
     (when (equal "interface" (save-excursion (smie-default-backward-token)))
       "and-interface"); 2

     (let ((token (ada-indent-backward-keyword)))
       (cond
	((or (equal token "and-interface_list"); 3
	     (equal token "new-type")); 1, 4, 5, 6, 7
	   "and-interface_list")
	(t "and"))); operator identifier
     )))

(defun ada-indent-refine-begin (token forward)
  ;; If "begin" follows a declaration, or immediately follows a block
  ;; start (see ada-indent-pre-begin-tokens), it is begin-body. If it
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
    (let ((token (ada-indent-backward-token))
	  (result nil)
	  (first t))
      (while (not result)
	(cond
	 ((equal token ";")
	  (if first
	      (progn
		(setq first nil)
		;; try again
		(ada-indent-goto-statement-start token)
		(setq token (ada-indent-backward-token)))

	    ;; we've skipped a statement or declaration; see if we can tell which
	    (ecase (car (save-excursion
			  (smie-default-forward-token)
			  (ada-indent-statement-or-decl)))
	      (statement
	       (setq result "begin-open"))
	      (declaration
	       (setq result "begin-body"))
	      (formal
	       (ada-indent-error "found generic formal parameter preceding begin spec"))
	      (unknown
	       ;; try again
	       (ada-indent-goto-statement-start token)
	       (setq token (ada-indent-backward-token)))
	      )))

	 ((member token ada-indent-pre-begin-tokens)
	  (setq result "begin-body"))
	 (t
	  (setq result "begin-open")))
	)
      result
      )))

(defun ada-indent-refine-case (token forward)
  (let ((token (save-excursion
		 (when forward (smie-default-backward-token))
		 (smie-default-backward-token))))

    (if (equal token "end")
	"case-end"
      "case")))

(defun ada-indent-refine-else (token forward)
  (let ((token (save-excursion
		 (when forward (smie-default-backward-token))
		 (smie-default-backward-token))))

    (if (equal token "or")
	"else"; identifier
      "else-other")))

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
     (t (ada-indent-refine-error "exception"))
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

(defun ada-indent-refine-for (token forward)
  (let ((token (save-excursion
		 (when (not forward) (smie-default-forward-token)); for
		 (smie-default-forward-token); identifier
		 (smie-default-forward-token))))
    (if (equal token "in")
	"for-loop"
      "for-attribute")))

;; ada-indent-refine-function see ada-indent-refine-subprogram

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
       ;; not keywords, so ada-indent-backward-keyword doesn't find
       ;; them. Fortunately, single tasks and protected objects cannot
       ;; have discriminants. FIXME (later): they can have aspect specs.
       (let ((token (progn
		      (smie-default-backward-token); identifier
		      (smie-default-backward-token))))
	 ;; "protected", "task", "body", "type", "subtype": handled here
	 ;;
	 ;; "": discriminant or parameter list; below
	 ;;
	 ;; "function" "procedure": generic instantiation,
	 ;;   parameter-less subprogram body, parameter-less
	 ;;   generic_formal_parameter; below
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
	    (token (save-excursion (prog1 (ada-indent-backward-keyword) (setq pos (point))))))
       (cond
	((equal token "case") "is-case")

	((member token '("function-inst"))
	 ;; function name is new ...;  generic_instantiation declaration
	 ;; function .. return ...;  see below at "return-*"
	 "is"); identifier

	((member token '("package-generic" "package-plain" "package-separate")) "is-package")

	((equal token "package-formal") "is"); identifier

	((equal token "procedure-formal") "is"); identifier

	((member token '("procedure-inst"))
	 ;;  procedure name is new ...;  generic_instantiation declaration
	 "is"); identifier

	((member token '("procedure-spec" "procedure-overriding" "procedure-separate"))
	 ;;  procedure name is abstract; declaration
	 ;;  procedure name is null;     declaration
	 ;;  procedure name is declarations begin statements end;  body
	 ;;  separate procedure name is declarations begin statements end; separate body
	 (let ((token (save-excursion
			(smie-default-forward-token); is
			(smie-default-forward-token))))
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
		      (smie-default-forward-token)))
	     "is"
	 "is-subprogram_body"))

	((member token '("type-protected" "type-task")) "is-type-block")

	((equal token "type-other")
	 (let* (pos
		(token
		 (save-excursion
		   (smie-default-forward-token); is
		   (prog1 (smie-default-forward-token)
		     (setq pos (point))))))
	   (cond
	    ((equal token ""); paren, string, or end of buffer; assume paren
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
	     ;;    type Private_Type_2 is abstract tagged limited private [with aspect_mark];
	     ;;    type Private_Type_2 is abstract tagged limited record ...
	     (let ((token (save-excursion (goto-char pos) (ada-indent-forward-type-modifiers))))
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

     (ada-indent-refine-error "is")
     )))

(defun ada-indent-refine-limited (token forward)
  (let ((token (save-excursion
		 (when (not forward) (smie-default-forward-token)); limited
		 (smie-default-forward-token))))

    (cond
     ((member token '("private" "with")) "limited-context")
     (t "limited"); identifier
     )))

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
	      (ada-indent-backward-keyword)))

      (if (member token '("in" "while" "of"))
	  ;; FIXME (later): iterators not tested yet; "of" will probably be refined.
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

(defun ada-indent-refine-new (token forward)
  (let ((token (save-excursion
		 (when forward (smie-default-backward-token))
		 (smie-default-backward-token); is
		 (ada-indent-backward-keyword))))

    (cond
     ((member token '("package-formal" "procedure-formal" "function-formal")) "new-formal")
     ((member token '("package-inst" "procedure-inst" "function-inst")) "new-inst")
     (t "new-type")
     )))

(defun ada-indent-refine-of (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    (let ((token (save-excursion
		   (ada-indent-backward-keyword); is-type array, :-object array
		 )))
      (cond
       ((equal "is-type" token) "of-type")
       (t "of-object"))
      )))

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
    (let ((token (save-excursion (smie-default-backward-token))))
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

       (progn
	 (setq token (save-excursion (ada-indent-forward-tokens-unrefined "body" "is" "renames")))
	 (cond
	  ((equal token "renames") "package-renames")
	  ((and (equal token "is")
		(equal "new" (save-excursion (smie-default-forward-token))))
	   "package-inst")
	  ))

       (if (ada-indent-generic-p) "package-generic")

       "package-plain")
    )))

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
    ;;    succeeding unrefined tokens: "with aspect_clause;" or ";"
    ;;    preceeding unrefined token: is
    ;;    preceeding refined token: is-type
    ;;       need to check for is-type to distinguish from package with null public part
    ;;    skip: ada-indent-type-modifiers
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
    ;; 4) library_item
    ;; 5) package_specification
    ;;
    ;;    package defining_program_unit_name [aspect_specification] is
    ;;       {basic_declarative_item}
    ;;    [private
    ;;       {basic_declarative_item}]
    ;;    end [[parent_unit_name.]identifier]
    ;;
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
    ;; 9) library_item ::= [private] library_unit_declaration
    ;;
    ;;    token: private-library
    ;;
    (cond
     ((equal "with" (save-excursion (smie-default-backward-token)))
      "private-with"); 6

     ((equal "is-type" (save-excursion (ada-indent-backward-type-modifiers)
				       (ada-indent-forward-token)))
      "private-type-spec"); 1

     ((let ((token (save-excursion
		     (smie-default-forward-token); private
		     (smie-default-forward-token))))
	(cond
	 ((member token '("with" ";"))
	  ;; 2
	  (if (equal "limited" (save-excursion (smie-default-backward-token)))
	      "private-context-2"
	    "private-context-1"))

	 ((member token '("package" "procedure" "function" "generic")) "private-library"); 9
	 )))

     (t "private-body")); all others
  ))

;; ada-indent-refine-procedure see ada-indent-refine-subprogram

(defun ada-indent-refine-protected (token forward)
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
  ;;    same as protected_type_declaration, without "type-other"
  ;;
  ;; 6) protected_body, subunit :=
  ;;
  ;;    a) protected body defining_identifier ...
  ;;
  ;;    b) separate (parent_unit_name) protected body defining_identifier ...
  ;;
  ;; 7) protected_body_stub  ::=
  ;;      protected body defining_identifier is separate [aspect_specification];
  ;;
  (save-excursion
    (when forward (smie-default-backward-token))
    (cond
     ((equal "separate-unit" (save-excursion (ada-indent-backward-keyword)))
      "protected-separate"); 6b

     ((equal "body" (save-excursion
		      (smie-default-forward-token)
		      (smie-default-forward-token)))
      "protected-body"); 6a, 7

     (t "protected-type")); 5, 4
    ))

(defun ada-indent-refine-raise (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    (let ((token (save-excursion (ada-indent-forward-tokens-unrefined "with" ";"))))
      (cond
       ((equal token ";") "raise"); identifier
       ((equal token "with") "raise-stmt")
       ))
    ))

(defun ada-indent-refine-record (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    (let ((token (save-excursion (smie-default-backward-token))))
      (cond
       ((equal token "end")
	(if (equal "with" (save-excursion
			    (smie-default-forward-token); record
			    (smie-default-forward-token)))
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
    ;;    preceding refined token: "function-spec"  _not_ overriding or generic
    ;;    token: "return-spec"
    ;;
    ;; 6) a subprogram_specification in a subprogram_renaming_declaration
    ;;    same as 1a.
    ;;
    ;; So we have to look both forward and backward to resolve this.
    (or
     (if (equal "end" (save-excursion (smie-default-backward-token))) "return-end"); 4c

     ;; Do this now, otherwise we can't distinguish between:
     ;;
     ;; function F1 return Integer;
     ;; return 0;
     ;;
     (let ((token (save-excursion (ada-indent-backward-keyword))))
       (cond
	((member token '("function-spec" "function-overriding" "function-generic" "function-separate"))
	 "return-spec"); 1a, 2, 5

	((equal token "function-formal")
	 "return-formal"); 1b
	))

     (save-excursion
       (let ((token (progn
		      (smie-default-forward-token); return
		      (smie-default-forward-token))))
	 (cond
	  ((equal token ";") "return"); 3a
	  (t
	   (setq token (smie-default-forward-token)); identifier
	   (cond
	    ((member token '(":" ":-do")) "return-ext"); 4a, 4b
	    (t "return"); 3b
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

(defun ada-indent-refine-separate (token forward)
  (let ((token (save-excursion
		 (when (not forward) (smie-default-forward-token))
		 (smie-default-forward-token))))

    (if (equal token ";")
	"separate-stub"
      "separate-unit")))

(defun ada-indent-refine-subprogram (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))

    (let ((prev-token (save-excursion (smie-default-backward-token))))
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
	   (equal "is" (ada-indent-forward-tokens-unrefined "return" "is" ";")); end of subprogram-spec
	   (equal "new" (smie-default-forward-token))))
	;; generic_instantiation. We have to check for "new" without
	;; refining it.
	(concat token "-inst"))

       ((ada-indent-generic-p) (concat token "-generic"))

       (t (concat token "-spec"))
  ))))

(defun ada-indent-refine-task (token forward)
  (save-excursion
    (when forward (smie-default-backward-token))
    (cond
     ((equal (save-excursion (ada-indent-backward-keyword)) "separate-unit")
      "task-separate"); separate (name) task body

     ((equal (save-excursion
               (smie-default-forward-token)
               (smie-default-forward-token))
	     "body")
      "task-body")

     ((equal (save-excursion
	       (smie-default-forward-token)
	       (ada-indent-forward-keyword)) ";")
      "task-single")

     (t "task-type"))
    ))

(defun ada-indent-refine-then (token forward)
  (let ((token (save-excursion
		 (when forward (smie-default-backward-token))
		 (smie-default-backward-token))))

    (cond
     ((equal token "and") "then"); identifier
     ((member (nth 2 (save-excursion (ada-indent-goto-parent token 1)))
	      '("if-open" "elsif"))
      "then-if")
     (t "then-select")
     )))

(defun ada-indent-refine-type (token forward)
  (let ((token (save-excursion
		 (when forward (smie-default-backward-token))
		 (smie-default-backward-token))))

    (cond
     ((equal token "protected") "type-protected")
     ((equal token "task") "type-task")
     (t "type-other")
     )))

(defun ada-indent-refine-use (token forward)
  ;; 1) use_clause:
  ;;
  ;;    a) use_package_clause ::= use package_name {, package_name};
  ;;
  ;;    b) use_type_clause ::= use [all] type subtype_mark {, subtype_mark};
  ;;
  ;;    it's not easy to tell these apart, so they are both use-decl
  ;;
  ;; 2) attribute_definition_clause ::=
  ;;      for local_name'attribute_designator use expression;
  ;;    | for local_name'attribute_designator use name;
  ;;
  ;; token: use-decl

  (let ((token (save-excursion
		 (when forward (smie-default-backward-token))
		 (ada-indent-backward-keyword))))

    (if (equal token "for-attribute")
	"use-attribute"
      "use-decl")))

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

     (if (equal "entry" (ada-indent-backward-keyword))
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
    ;;    ancestor_part can be an aggregate for the parent type, or the parent type name.
    ;;    ada-indent-backward-keyword : "(" for either (aggregate paired parens are skipped)
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
    ;;    preceding unrefined token: "record", FIXME (later): others?
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

     (let ((token (save-excursion (ada-indent-backward-keyword))))
       (cond
	((equal token "(") "with-agg"); 2
	((equal token "raise-stmt") "with-raise"); 7
	))

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
    ("abort" 	 ada-indent-refine-abort)
    ("accept" 	 ada-indent-refine-accept)
    ("and" 	 ada-indent-refine-and)
    ("begin" 	 ada-indent-refine-begin)
    ("case" 	 ada-indent-refine-case)
    ("end" 	 ada-indent-refine-end)
    ("else" 	 ada-indent-refine-else)
    ("exception" ada-indent-refine-exception)
    ("exit" 	 ada-indent-refine-exit)
    ("for" 	 ada-indent-refine-for)
    ("function"  ada-indent-refine-subprogram)
    ("interface" ada-indent-refine-interface)
    ("if" 	 ada-indent-refine-if)
    ("is" 	 ada-indent-refine-is)
    ("limited" 	 ada-indent-refine-limited)
    ("loop" 	 ada-indent-refine-loop)
    ("mod" 	 ada-indent-refine-mod)
    ("new" 	 ada-indent-refine-new)
    ("of" 	 ada-indent-refine-of)
    ("or" 	 ada-indent-refine-or)
    ("package" 	 ada-indent-refine-package)
    ("private" 	 ada-indent-refine-private)
    ("procedure" ada-indent-refine-subprogram)
    ("protected" ada-indent-refine-protected)
    ("raise" 	 ada-indent-refine-raise)
    ("record" 	 ada-indent-refine-record)
    ("return" 	 ada-indent-refine-return)
    ("select" 	 ada-indent-refine-select)
    ("separate"  ada-indent-refine-separate)
    ("task" 	 ada-indent-refine-task)
    ("then" 	 ada-indent-refine-then)
    ("type" 	 ada-indent-refine-type)
    ("use" 	 ada-indent-refine-use)
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

(defvar ada-indent-refining nil
  "t when parsing forward to validate cache.")

(defun ada-indent-invalidate-cache()
  "Invalidate the ada-indent token cache for the current buffer."
  (interactive)
  (setq ada-indent-cache-max 0))

(defun ada-indent-validate-cache (pos)
  "Update cache from `ada-indent-cache-max' to at least POS."
  (save-excursion
    (let ((ada-indent-refining t))
      (goto-char ada-indent-cache-max)
      (while (> pos (point))
	(ada-indent-forward-keyword))
      ;; If pos is inside a paren, but ada-indent-cache-max was
      ;; outside it, this just skipped us.
      (when (not (ada-indent-get-cache pos))
	(goto-char pos)
	(ada-indent-validate-cache-parens))
    )))

(defun ada-indent-validate-cache-parens ()
  "Assuming point is inside parens, validate cache within the parens."
  (save-excursion
    ;; syntax-ppss sometimes tells us we are not in a paren, even when
    ;; we are; not debugging that.
    (let ((prev-cache-max ada-indent-cache-max)
	  (ada-indent-cache-max 0)
	  (done nil)
	  (ada-indent-refining t)
	  token)

      (condition-case err
	  (progn
	    (goto-char (+ 1 (scan-lists (point) -1 1)));; just after opening paren
	    (setq ada-indent-cache-max (point));; force calling refine-*
	    (while (not done)
	      (setq token (ada-indent-forward-token))
	      (cond
	       ((and (equal token "")
		     (eq (char-after) ?\)))
		(setq done t))

	       ((or (not (ada-indent-keyword-p token))
		    (ada-indent-closer-p token))
		(smie-forward-sexp nil))

	       (t
		(smie-forward-sexp token))
	       ))
	    (setq ada-indent-cache-max prev-cache-max)
	    )
	(scan-error
	 ;; from scan-lists; can happen when user is typing code
	 (setq ada-indent-cache-max prev-cache-max)))
  )))

(defun ada-indent-get-cache (pos)
  "Return refined token string from the `ada-indent-cache' text property at POS."
  (get-text-property pos 'ada-indent-cache))

(defun ada-indent-put-cache (pos token)
  "Set TOKEN as the refined token string in the `ada-indent-cache' text property at POS.
Return TOKEN."
  (put-text-property pos (+ 1 pos) 'ada-indent-cache token)
  (setq ada-indent-cache-max (max ada-indent-cache-max pos))
  token)

(defun ada-indent-after-change (begin end length)
  ;; We only need to move ada-indent-cache-max to `begin' if this
  ;; change affects code, ie non-whitespace non-comment. Otherwise, we
  ;; could just adjust it by the change amount. But we're keeping it
  ;; simple, so we always move cache-max to `begin' or earlier;
  ;; optimize later if needed.
  ;;
  ;; However, we cannot set cache-max to inside a comment or string!
  (when (> ada-indent-cache-max begin)
    (save-excursion
      ;; (info "(elisp)Parser State")
      (let ((state (syntax-ppss begin)))
	(cond
	 ((or
	   (nth 3 state); in string
	   (nth 4 state)); in comment
	  (setq ada-indent-cache-max (nth 8 state)))

	 (t (setq ada-indent-cache-max begin))
	 )))))

(defun ada-indent-next-token (forward)
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
	 (refine (cadr (assoc token ada-indent-next-token-alist))))
    (cond
     ((stringp refine) refine)

     ((functionp refine)
      (if (<= cache-pos ada-indent-cache-max)
	  (or (ada-indent-get-cache cache-pos)
	      ;; cache can be nil inside parens; those are skipped on
	      ;; the first pass, but we may now be indenting inside
	      ;; one.
	      (progn
		(ada-indent-validate-cache-parens)
		(ada-indent-get-cache cache-pos)))
	(if ada-indent-refining
	    (ada-indent-put-cache cache-pos (funcall refine token forward))
	  (ada-indent-validate-cache cache-pos)
	  (ada-indent-get-cache cache-pos)
	  )))

     (t token))
    ))

(defun ada-indent-forward-token () (ada-indent-next-token t))
(defun ada-indent-backward-token () (ada-indent-next-token nil))

;;; indent rules

(defun ada-indent-when (base)
  "Return indentation offset to use after \"when\", \"=>-when\".
BASE should be `ada-indent' or `ada-indent-broken'."
  ;; Use ada-indent-when if "when" is at start of line. It may not be
  ;; in a select statement.
  (save-excursion
    (unless (looking-at "when")
      (ada-indent-backward-tokens-unrefined "when"))
    (if (smie-indent--bolp)
	(+ base ada-indent-when)
      base)))

(defun ada-indent-rule-current (offset)
  "Indent relative to the current line"
  (cons 'column (+ (save-excursion (back-to-indentation) (current-column)) offset)))

(defun ada-indent-keyword-p (token)
  (assoc token ada-indent-grammar))

(defun ada-indent-opener-p (token)
  (let ((association (assoc token ada-indent-grammar)))
    (when association (listp (nth 1 association)))))

(defun ada-indent-closer-p (token)
  (let ((association (assoc token ada-indent-grammar)))
    (when association (listp (nth 2 association)))))

(defun ada-indent-goto-parent (child up)
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
	  ((ada-indent-keyword-p child)
	   child)
	  (t (ada-indent-backward-keyword)))))

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
		(setq token (ada-indent-backward-keyword))
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
CHILD. Preserves point.  CHILD must be non-nil and a keyword
or \"(\", and point must be at the start of CHILD."
  (save-excursion
    (let
	((parent
	  (ada-indent-goto-parent
	   child
	   (if (and child
		    (ada-indent-opener-p child))
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

(defun ada-indent-skip-identifier-list (forward)
  "Skip forward/backward over {identifier,}.
Return (preceding-pos preceding-string) for backward, succeeding
for forward."
  (let (parent parent-pos)
    (while
	(progn
	  (setq parent (if forward (ada-indent-forward-keyword) (ada-indent-backward-keyword)))
	  (setq parent-pos (point))
	  (cond
	   ((equal "," parent) t)
	   ((equal "(" parent) (forward-char 1) nil)
	   ((equal ")" parent) (backward-char 1) nil)
	   (t
	    (if forward (smie-default-backward-token) (smie-default-forward-token))
	    nil))))
    (forward-comment (if forward (- (point)) (point-max)))
    (list parent-pos parent)))

(defun ada-indent-goto-statement-start (child)
  "Move point to the start of the statement/declaration
containing point.  If point is in a parenthesized list, move to
the start of the current list element.  Return (preceding-pos
preceding-string), where `preceding-string' is the text of the
token preceding the statement start, and `preceding-pos' is the
position of its first character.  If point is at statement or
list element start, does nothing.  If CHILD is non-nil, it must
be a keyword, and point must be at the start of CHILD."
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
  ;; ada-indent-backward-keyword.
  ;;
  ;; Object declarations can declare multiple objects:
  ;;
  ;;   A,
  ;;    B : type;
  ;;
  ;; The first keyword is ":"; we use ada-indent-backward-keyword.
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
      ;; (assignment), so we have to use use ada-indent-backward-keyword
      ;; to find the previous keyword.
      (when (not (member child ada-indent-block-keywords))
	(setq pos (point));; not using save-excursion so we can do throw 'done from here.
	(setq parent (ada-indent-backward-keyword))
	(setq parent-pos (point))
	(cond
	 ((or
	   (equal parent ";")
	   (member parent ada-indent-block-keywords))
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
	  (throw 'done (ada-indent-skip-identifier-list nil)))
	 ))

      (when pos (goto-char pos))

      (if (equal child ";")
	  ;; we want to go to the start of the statement preceding
	  ;; ";". goto-parent with child = ";" will go to the next ";"
	  ;; or block open, which is not the same. In particular,
	  ;; block and statement labels get in the way.
	  (setq child (ada-indent-backward-keyword)))

      (setq parent-count
	    (if (member child '("record-open" "<<"))
		2
	      1))
      ;; If child is "(" the first run thru smie-backward-sexp
      ;; produces no motion; that causes the second run to call the
      ;; lower level scanner to skip the parens, because
      ;; forward-sexp-function is let-bound to nil.
      ;;
      ;; If child is record, that is its own parent, and we want the
      ;; next one up. Same for "<<".

      (setq parent (ada-indent-goto-parent child parent-count))

      (when (or
	     (member (nth 2 parent) '("(" ";"))
	     (and
	      (not (equal (nth 2 parent) "record-open"))
	      (member (nth 2 parent) ada-indent-block-keywords))
	     )
	;; goto-parent left point on token following parent; point is at statement start
	(throw 'done (list (nth 1 parent) (nth 2 parent))))

      (when (equal (nth 2 parent) ",")
	(cond
	 ((equal orig-child ",")
	  (throw 'done (list (nth 1 parent) (nth 2 parent))))
	 ((equal orig-child ";")
	  ;; we're in a parameter list or object declaration, with
	  ;; multiple identifiers before the ":".
	  (while (equal (nth 2 parent) ",")
	    (setq parent (ada-indent-goto-parent "," 1)))
	  (throw 'done (list (nth 1 parent) (nth 2 parent))))
	 ))

      (if (not (= (point) (nth 1 parent)))
	  ;; goto-parent stopped because of a lower-precedence token,
	  ;; not a closer; that might be because:
	  ;;
	  ;; 1) a parameter list object:
	  ;;
	  ;;    A, B : in Integer;
	  ;;
	  ;;    stops on "in"
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
		 (and (member (nth 2 parent) `("procedure-spec" "function-spec")) ; _not -overriding -generic
		      (member (save-excursion (smie-default-backward-token)) '("access" "protected"))))
	(setq parent (ada-indent-goto-parent parent 2)))

      (cond
       ((member (nth 2 parent) '(":=" ":-object"))
	;; We don't need 'skip-identifier-list' for :-object, but it
	;; does the right thing.
	;;
	;; Simple types in object declarations were handled above; for
	;; complex types and parameter lists, we get here.
	(ada-indent-skip-identifier-list nil))

       (t (list (nth 1 parent) (nth 2 parent))))
      )))

(defun ada-indent-rule-statement (offset child)
  "Find the start of the statement/declaration containing point (using
`ada-indent-goto-statement-start'), return an indent by OFFSET relevant
to it. Preserves point.  If CHILD is non-nil, point must be at
the start of CHILD, which must be a keyword."
  (save-excursion
    (let ((parent (ada-indent-goto-statement-start child)))
      (cond
       ((equal (nth 1 parent) "(")
	nil); indent to the (, not the line it is on
       (t (back-to-indentation)))
      (cons 'column (+ (current-column) offset))
      )))

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
      ((equal arg "(")
       ;; parenthesis occur in expressions, and after names, as array
       ;; indices, subprogram parameters, type constraints.
       ;;
       ;; "(" is not in the smie grammar, but
       ;; ada-indent-before-keyword handles is specially.
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

       (save-excursion
	 (let ((pos (point)))
	   (if (member (smie-default-backward-token) '("procedure" "function"))
	       ;; 1, 2
	       (progn (goto-char pos) (ada-indent-rule-parent ada-indent-broken arg))
	     ;; 3, 4, 5
	     (progn (goto-char pos) (ada-indent-rule-statement ada-indent-broken arg))))
	 ))

      ((equal arg ")")
       ;; find the open paren
       (save-excursion
	 (forward-char 1)
	 (backward-sexp)
	 (cons 'column (current-column))))

      ((equal arg "=>-when")
       ;; exception to block statement rule.
       (ada-indent-rule-statement (ada-indent-when ada-indent-broken) arg))

      ((equal arg "abort-select")
       ;; exception to block-keyword indentation; preserve Ada mode 4.01 behavior
       (save-excursion
	 (ada-indent-goto-parent arg 1)
	 (back-to-indentation)
	 (cons 'column (+ (current-column) ada-indent-broken))))

      ((equal arg "end-record")
       ;; goto-parent leaves point on "record-open".
       (save-excursion
	 (ada-indent-goto-parent arg 1)
	 (back-to-indentation)
	 (cons 'column (current-column))))

      ((member arg '("record-open" "record-null"))
       ;; This indents the first line. The components are indented
       ;; relative to the line containing "record-open"; see "record-open" in
       ;; :after.
       (ada-indent-rule-statement ada-indent-record-rel-type arg))

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
		(ada-indent-goto-parent arg 1)
		;; this can leave point on 'overriding' or 'with'
		(while (not (equal
			     (smie-default-forward-token)
			     "function"))
                               nil)
		(smie-default-backward-token)
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

      ((member arg '("procedure-overriding" "function-overriding"))
       (save-excursion
	 (smie-default-backward-token)
	 (cons 'column (current-column))))

      ((member arg '("function-separate" "package-separate" "procedure-separate" "protected-separate"
		     "task-separate"))
       (ada-indent-rule-statement 0 arg))

      ((equal arg "when-case")
       (ada-indent-rule-statement ada-indent-when arg))

      ((equal arg "when-select")
       (save-excursion
	 (smie-default-backward-token)
	 ;; we are now before "or-select" or "select-open"
	 (cons 'column (+ (current-column) ada-indent-when))))

      ((equal arg "with-agg")
       (ada-indent-rule-statement 0 arg))

      ((member arg '("limited-context"
		     "private-context-1"
		     "with-context-1"))
       (cons 'column 0))

      ((or (member arg ada-indent-block-end-keywords)
	   (member arg ada-indent-block-keywords))
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
	  ((ada-indent-opener-p arg)
	   ;; Indenting first keyword; declare, begin-open,
	   ;; etc. If there is a previous statement, indent 0 relative
	   ;; to it. Otherwise indent ada-indent relative to the block
	   ;; opening. That is handled by :after, so return nil here.
	   nil)

	  (t
	   ;; Indenting second or third keyword; declare, begin-body,
	   ;; etc. Indent relative to block start. We use
	   ;; goto-statement, not goto-parent, to handle function
	   ;; returning anonymous access to function
	   ;; (test/ada_mode-nominal.adb Function_Access_11)
	   (ada-indent-rule-statement 0 arg))
	  )
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

      ((equal arg ",")
       ;; ada-mode 4.01 uses broken indent for multi-identifier in
       ;; parameter list declaration, and object declarations. But
       ;; aggregates use no indent after ",". "with", "use" have their
       ;; own controlling options. We have to check for ":-object" first,
       ;; since otherwise a parameter list looks like an aggregate.
       (let (temp pos
	     (token (nth 1 (save-excursion (ada-indent-skip-identifier-list t)))))
	 (cond
	  ((equal token ":-object")
	   (ada-indent-rule-statement ada-indent-broken arg))

	  ((and (setq temp (save-excursion (ada-indent-skip-identifier-list nil)))
		(setq token (nth 1 temp))
		(setq pos   (nth 0 temp))
		(member token '("(" "with-agg")))
	   (ada-indent-rule-statement 0 arg))

	  ((equal token "use-decl"); also use-context
	   (ada-indent-rule-statement ada-indent-use arg))

	  ((member token '("with-context-1" "with-context-2"))
	   ;; match 4.01; indent by ada-indent-with relative to "with"
	   (cons 'column (+ (save-excursion (goto-char pos) (current-column)) ada-indent-with)))
	  )))

      ((member arg '(";" "private-library"))
       (ada-indent-rule-statement 0 arg))

      ((equal arg "=>-when")
       ;; exception to block statement rule
       (ada-indent-rule-statement (ada-indent-when ada-indent) arg))

      ((equal arg "|")
       (ada-indent-rule-statement (ada-indent-when ada-indent-broken) arg))

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

      ((equal arg "when-case")
       ;; exception to block statement rule
       (ada-indent-rule-statement (ada-indent-when ada-indent-broken) arg))

      ((equal arg "with-agg")
       (ada-indent-rule-statement 0 arg))

      ((member token '("with-context-1" "with-context-2"))
       (ada-indent-rule-statement ada-indent-with arg))

      ((member arg ada-indent-block-keywords)
       (ada-indent-rule-statement ada-indent arg))

      (t (ada-indent-rule-statement ada-indent-broken arg))
      ))
    ))

;;;; smie-indent-functions
;;
;; each must not move point, and must return a column (as an integer) or nil.
;;
;; declared in order called

(defun ada-indent-comment-indent ()
  "For `comment-indent-function'."
  ;; This should only be called by comment-indent-new-line or
  ;; fill-comment-paragraph, so there will be a preceding comment line
  ;; that we can trust.
  (save-excursion
    (forward-comment -1)
    (if (looking-at comment-start)
	(current-column)
      (error "ada-indent-comment-indent called after non-comment"))))

(defun ada-indent-comment ()
  "Compute indentation of a comment."
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
      ((or
	(save-excursion (forward-line -1) (looking-at "\\s *$"))
	(save-excursion (forward-comment -1)(not (looking-at comment-start))))
       ;; comment is after a blank line or code; indent as if code
       ;;
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

      (t
       ;; comment is after a comment
       (forward-comment -1)
       (current-column))
      ))
    ))

(defun ada-indent-label ()
  (cond
   ((save-excursion
      (or
       (equal "<<" (ada-indent-forward-token))
       (equal ":-label" (ada-indent-forward-token))))
    ;; before a label
    (let (offset
	  (token (save-excursion (ada-indent-backward-token))))
      (cond
       ((equal token "=>-when")
	(setq offset (ada-indent-when (+ ada-indent ada-indent-label))))

       ((member token ada-indent-block-keywords)
	(setq offset (+ ada-indent ada-indent-label)))

       (t (setq offset ada-indent-label)))

      (cdr (save-excursion
	     (ada-indent-backward-token)
	     (ada-indent-rule-statement offset token)))
      ))

   ((member (save-excursion (ada-indent-backward-token))
	    '(">>" ":-label"))
    ;; after a statement or block label
    (save-excursion
      (ada-indent-backward-token)
      (back-to-indentation)
      (- (current-column) ada-indent-label)))
   ))

(defun ada-indent-record()
  "Indent a line containing the \"record\" that starts a record component list."
  (catch 'quit
    (when (> (nth 0 (syntax-ppss)) 0)
      ;; inside discriminant parens; let other rules handle it
      (throw 'quit nil))

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

	    (cdr indent)
	    )))
    ))

(defun ada-indent-before-keyword()
  "Replacement for `smie-indent-keyword', tailored to Ada.
It requires `ada-indent-rule' to return nil or ('column column),
never just an offset (since we would not know what the offset was
relative to)."
  (save-excursion
    (forward-comment (point-max));; handle indenting blank line before code
    (let*
	((token (save-excursion (ada-indent-forward-token)))
	 char)

      (cdr
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
	 (ada-indent-rules :before token)))
       ))))

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
      (cdr indent)
      ))

(defun ada-indent-default ()
  "Unconditionally indent by `ada-indent-broken' from the current
statement start.  Intended to be the last item in
`smie-indent-functions', used when no indentation decision was
made."
  (cdr (ada-indent-rule-statement ada-indent-broken nil)))

;;; debug
(defvar ada-indent-debug-refine nil
  "When non-nil, `ada-indent-show-keyword-forward' and
`ada-indent-show-keyword-backward' invalidate cache first, so
they always run the refine algorithm.  In addition,
`ada-indent-refine-error' throws errors instead of failing more
gracefully.")

(defun ada-indent-show-keyword-forward ()
  "Show the grammar info for word following point, and move across it."
  (interactive)
  (when ada-indent-debug-refine
      (setq ada-indent-cache-max (min ada-indent-cache-max (- (point) 1))))
  (message "%s" (assoc (ada-indent-forward-token) smie-grammar)))

(defun ada-indent-show-keyword-backward ()
  "Show the grammar info for word preceding point, and move across it."
  (interactive)
  (when ada-indent-debug-refine
    (save-excursion
      (smie-default-backward-token)
      (setq ada-indent-cache-max (min ada-indent-cache-max (- (point) 1)))))
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

(defun ada-indent-show-sexp-token ()
  "Move backward one sexp, starting with refined token after point."
  (interactive)
  (let* ((token (save-excursion (ada-indent-forward-token)))
	 (toklevels (smie-backward-sexp token)))
    (message "%s => %s" (assoc token ada-indent-grammar) toklevels)))

(defun ada-indent-show-statement-start ()
  "Move to the start of the current statement."
  (interactive)
  ;; this is the way ada-indent-goto-statement-start is called during indentation; better for testing
  (let ((token (save-excursion (ada-indent-forward-token)))
	parent)
    (setq parent (ada-indent-goto-statement-start (if (equal token "") "(" token)))
    (message "%s => %s" token (nth 1 parent))))

(defun ada-indent-show-cache ()
  "Show cache at point."
  (interactive)
  (message "%s" (ada-indent-get-cache (point))))

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
	  ada-indent-label
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

  (set (make-local-variable 'smie-skip-associative) t)
  ;; we don't want `smie-backward-sexp' to stop at weird places
  ;; FIXME (later): this var is in a local patch that won't be in main; do something else
  ;; either cope with the weird place in goto-parent, or rewrite smie-backward-sexp

  (set (make-local-variable 'smie-blink-matching-inners) nil); too annoying to blink to 'package' on 'is', etc.

  (if debug-on-error
      (set (make-local-variable 'blink-matching-paren) nil))
  ;; smie-setup puts smie-blink-matching-open on
  ;; post-self-insert-hook; that uses uses blink-matching to blink on
  ;; all opener/closer pairs.  This is just annoying while we are
  ;; working on this code (it tries to run a broken parser), so we
  ;; turn it off. But that also disables blink actual parens, which is
  ;; useful.

  (add-hook 'after-change-functions 'ada-indent-after-change nil t)

  (set (make-local-variable 'comment-indent-function) 'ada-indent-comment-indent)

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
      ada-font-lock-name-regexp "?")
     '(1 font-lock-keyword-face)
     '(2 (if (member (progn
		       (ada-indent-validate-cache (match-beginning 1))
		       (ada-indent-get-cache (match-beginning 1)))
		    '("return-spec" "return-formal"))
	     font-lock-type-face
	   'default)
	 nil t)
     )))

  (define-key ada-mode-map "\t" 'indent-for-tab-command)
  ;; TAB will now use smie indentation in Ada mode buffers
  ;; FIXME (later): possibly not the best place to bind a key
  )

(add-hook 'ada-mode-hook 'ada-indent-setup)

(provide 'ada-indent)
(provide 'ada-indent-engine)

;;; end of file
