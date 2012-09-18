;;; Ada mode indentation engine, based on SMIE
;;
;; [1] ISO/IEC 8652:201z (draft 18); Ada 2012 reference manual

(require 'smie)
(eval-when-compile (require 'cl))

(defcustom ada-indent 3
  "*Size of Ada default indentation, when no other indentation is used.

An example is :
procedure Foo is
begin
>>>null;"
  :type 'integer  :group 'ada)

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

       ;; alphabetical order, since there isn't any more reasonable order
       ;; we use the same names as [1] Annex P as much as possible
       (array_type_definition ("type" identifier "is-type" "array" expression "of" name))

       (component (identifier ":" name)) ; discriminants are a balanced paren; FIXME: constraints?

       (components
	(component)
	(component ";" component))

       (declaration
	(array_type_definition)
	(protected_body)
	(protected_type_declaration)
	(record_type_definition)
	(subprogram_declaration)
	(subprogram_body)
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
       ;; FIXME: membership_choice_list?
       ;; FIXME: unary operators?
       ;; FIXME: if expressions?

       (name
	(identifier)
	(name "." identifier) ; selected_component
	)

       (package_declaration
	("package" name "is-package_declaration" declarations "begin" statements "end"))

       (package_body
	;; Leaving 'package body' as separate tokens causes problems in refine-is
	("package_body" name "is-package_body" declarations "begin" statements "end"))

       (protected_body ("protected_body" identifier "is-protected_body" declarations "end"))

       (protected_type_declaration
	;; prefixing "protected" gives a precedence conflict: 'token
	;; type is both neither and opener', so "protected type" is
	;; combined into one token in
	;; ada-indent-forward/backward-token.
	("protected_type" identifier "is-type" declarations "private" declarations "end"))
       ;; also covers single_protected_declaration
       ;; FIXME: [new interface_list with]
       ;; FIXME: [aspect_specification]

       (record_type_definition ("type" identifier "is-type" "record" components "end"))

       (statement
	(expression); matches procedure calls, assignment
	("return")
	("return-exp" expression)
	("return-do" identifier ":" name)
	("return-do" identifier ":" name "do" statements "end_return")
	)

       (statements
	(statement)
	(statement ";" statement))

       (subprogram_body
	;; factoring out subprogram_specification here breaks something.
	("function" name "return-spec" name "is-subprogram_body" declarations "begin" statements "end")
	("procedure" name "is-subprogram_body" declarations "begin" statements "end"))
       ;; FIXME: test overriding_indicator
       ;; FIXME: test aspect_specification
       ;; FIXME: test exception handler

       (subprogram_declaration
	;; factoring out subprogram_specification here breaks something.
	("function" name "return-spec" name)
	("procedure" name))
       ;; FIXME: is abstract
       ))

    ;; operators and similar things
    ;; FIXME: ref RM, get it right
    (smie-precs->prec2
     '((nonassoc "-operator-")
       ;; The structure of this table is stolen from the modula2 smie grammar.
       ;;
       ;; We can merge the relational, math, and other operators in
       ;; these levels, because we don't care about legality.
       ;; FIXME: do we need any precedence hierarchy for these at all (ask Stefan)?
       ;;
       (nonassoc "=" "/=" "<" "<=" ">" ">=" "in") ; relational_operator, membership
       (assoc "or" "or_else" "xor" "+" "-" "&")
       (assoc "and" "and_then" "mod" "rem" "*" "/")
       (right "abs" "not")
       (left "'" "." "**") ; Qualifier, selector, exponent
       (nonassoc ":=") ; assignment statement (always done last)
       ))
    )))

(defun ada-indent-next-unit_name (next-token)
  "Skip over a unit_name using NEXT-TOKEN, consisting of just
identifiers and dots. Return the token that isn't part of the
name (may be before any name is seen)."
  (let (token)
    (while
	(progn
	  (setq token (funcall next-token))
	  (if (equal "" token)
	      ;; we hit a parameter list or something similar
	      nil
	    (setq token (nth 0 (assoc token smie-grammar)))
	    (or (not token); not a keyword, so it must be an identifier
		(equal token ".")))))
    token))

(defun ada-indent-backward-unit_name ()
  (ada-indent-next-unit_name 'ada-indent-backward-token))

(defun ada-indent-forward-unit_name ()
  (ada-indent-next-unit_name 'ada-indent-forward-token))

(defun ada-indent-refine-is (direction)
  (save-excursion
    ;; ada-indent-forward-token calls us with point after token;
    ;; ada-indent-backward with point before token.
    ;;
    (when (eq direction 'forward) (smie-default-backward-token))

    (or
     ;; First try simple, common constructs.  We don't use
     ;; smie-backward-sexp for these, because it is often too greedy;
     ;; it leads to horrible recursive parsing with wrong guesses,
     ;; and ends up reporting no match.
     (save-excursion
       (let ((token (ada-indent-backward-unit_name)))
	 (pcase token
	   (`"package" "is-package_declaration")
	   ;; "package" name ^ "is" FIXME: "name" could have dots!

	   (`"package_body" "is-package_body")
	   ;; "package" "body" name ^ "is" FIXME: "name" could have dots!

	   (`"procedure" "is-subprogram_body")
	   ;; "procedure" name ^ "is"
	   ;; FIXME: test procedure with parameter list

	   (`"protected_type" "is-type")
	   ;; "protected" identifier ^ "is"

	   (`"protected_body" "is-protected_body")
	   ;; "protected" "body" identifier ^ "is"

	   (`"return-spec" "is-subprogram_body")
	      ;; "function" identifier "return" name ^ "is"

	   (`"type" "is-type")
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

(defun ada-indent-refine-return (direction)
  (save-excursion
    (when (eq direction 'forward) (smie-default-backward-token))

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
    ;; So we have to look both forward and backward to resolve this.
    (or
     (save-excursion (if (equal "end" (smie-default-backward-token)) "end_return")); 4c

     ;; do this before parsing forward, otherwise can't distinguish between:
     ;; function F1 return Integer;
     ;; return 0;
     ;;
     ;; It would be simpler to do (smie-backward-sexp "return-spec")
     ;; here, but if we are wrong, we'd get totally confused.
     (save-excursion
	(if (equal "function"
		   ;; no parameter list
		   (ada-indent-backward-unit_name))
	    "return-spec")); 1 or 2

     (save-excursion
	(if (equal "function"
		   (progn
		     (smie-backward-sexp); parameter list
		     (ada-indent-backward-unit_name)))
	    "return-spec")); 1 or 2

     (save-excursion
       ;; FIXME: test this at end of buffer (not very
       ;; likely to happen, but possible while entering code)
       (if (equal ";"
		  (progn
		    (smie-default-forward-token); return
		    (smie-default-forward-token)))
	   "return"; 3a
	 (pcase (smie-default-forward-token)
	   (`";" "return-exp") ; special case of 3b with expression = identifier or literal

	   (`":"
	     (if (equal (ada-indent-forward-unit_name) ";")
		 "return"; 4a
	       "return-do")); 4b

	   (`"is" "return-spec"); special case of 2, with name = identifier

	   (_ "return-exp"); 3b
	   )))
     )))

(defun ada-indent-forward-token ()
  (pcase (smie-default-forward-token)
    ;; FIXME: and_then, or_else
    (`"end"
     (if (equal "return" (save-excursion (smie-default-forward-token)))
	 (progn
	   (smie-default-forward-token)
	   "end_return")
       "end"))

    (`"is" (ada-indent-refine-is 'forward))

    (`"protected"
     (if (equal "body" (save-excursion (smie-default-forward-token)))
	 (progn
	   (smie-default-forward-token)
	   "protected_body")
       "protected_type"))

    (`"return" (ada-indent-refine-return 'forward))

    (token token)))

(defun ada-indent-backward-token ()
  (pcase (smie-default-backward-token)
    ;; FIXME: and_then, or_else
    (`"body"
     (pcase (save-excursion (smie-default-backward-token))
       (`"package"
	 (progn
	   (smie-default-backward-token)
	   "package_body"))

       (`"protected"
	 (progn
	   (smie-default-backward-token)
	   "protected_body"))
       (token (error "unrecognized 'body': %s" token))))

    (`"is" (ada-indent-refine-is 'backward))

    (`"protected" "protected_type")
    ;; single_protected_declaration. we don't have to check for 'body'
    ;; here; we would have already hit that.

    (`"return" (ada-indent-refine-return 'backward))

    (`"type"
     (if (equal "protected" (save-excursion (smie-default-backward-token)))
	 (progn
	   (smie-default-backward-token)
	   "protected_type")
       "type"))

    (token token)))

(defun ada-indent-rules (method arg)
  (case method
    (:elem
     (case arg
       (basic ada-indent)
       (args 0))
     )
    (:before
     (pcase arg
       (`"end"
	(smie-rule-parent 0))))
    (:after
     (pcase arg
       ((or `"is-type"
	    `"is-package_body"
	    `"is-package_declaration"
	    `"is-protected_body"
	    `"is-subprogram_body")
	;; indent relative to the start of the declaration or body,
	;; which is the parent of 'is'.
	(smie-rule-parent ada-indent))
       (`";"
	(if (smie-rule-sibling-p)
	    nil
	  ;; indent relative to the start of the block or parameter
	  ;; list, which is the parent of ';'
	  (smie-rule-parent ada-indent)))
       ))))

(defun ada-indent-setup ()
  (smie-setup ada-indent-grammar #'ada-indent-rules
	      :forward-token #'ada-indent-forward-token
	      :backward-token #'ada-indent-backward-token))

(add-hook 'ada-mode-hook 'ada-indent-setup)

(define-key ada-mode-map "\t" 'indent-for-tab-command)
;; TAB will now use smie indentation in Ada mode buffers

(provide 'ada-indent)

;;; end of file
