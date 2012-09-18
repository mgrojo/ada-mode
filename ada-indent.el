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
	)

       (statements
	(statement)
	(statement ";" statement))

       (subprogram_body
	;; factoring out subprogram_specification here breaks something.
	("function" name "return" name "is-subprogram_body" declarations "begin" statements "end")
	("procedure" name "is-subprogram_body" declarations "begin" statements "end"))
       ;; FIXME: test overriding_indicator
       ;; FIXME: test aspect_specification
       ;; FIXME: test exception handler

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

(defun ada-indent-backwards-unit_name ()
  "Skip backwards over a unit_name, consisting of just
identifiers and dots. Return the token before the name."
  (let (token)
    (while
	(progn
	  (setq token (assoc (ada-indent-backward-token) smie-grammar))
	  (or (not token); not a keyword, so it must be an identifier
	      (equal (nth 0 token) "."))))
    (nth 0 token)))

(defun ada-indent-refine-entry_body ()
  "Return token if current token is the 'is' of an entry body, else nil"
  (save-excursion
    ;; entry body with params: "entry" identifier "("...")" "when" exp "is"
    ;; We have to use smie-backward-sexp to parse the exp.
    (let* ((token (nth 2 (smie-backward-sexp "is-entry_body"))))
	(if (equal "entry" token)
	    "is-entry_body"
	  nil))))

(defun ada-indent-refine-is (direction)
  (save-excursion
    ;; ada-indent-forward-token calls us with point after token;
    ;; ada-indent-backward with point before token.
    ;;
    (when (eq direction 'forward) (smie-default-backward-token))

    (or
     ;; first try simple, common constructs.  we don't use
     ;; smie-backward-sexp for these, because it is often too greedy;
     ;; it leads to horrible recursive parsing with wrong guesses,
     ;; and ends up reporting no match.
     (save-excursion
       (let ((token (ada-indent-backwards-unit_name)))
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

	   (`"return"
	    (setq token
		  (progn
		    (smie-default-backward-token)
		    (smie-default-backward-token)))
	    (pcase token
	      (`"function" "is-subprogram_body")
	      ;; "function" identifier "return" identifier  ^ "is"
	      ;; FIXME: test procedure with parameter list
	      (t nil)))

	   (`"type" "is-type")
	   ;; "type" identifier ^ "is"
	   ;; covers "protected type"; that's lexed as the token "type"
	   )))

     ;; now more complicated things
     (ada-indent-refine-entry_body)

     (error "ada-indent-backward-token: unrecognized 'is'"))))

(defun ada-indent-forward-token ()
  (pcase (smie-default-forward-token)
    ;; FIXME: and_then, or_else
    (`"is" (ada-indent-refine-is 'forward))

    (`"protected"
     (if (equal "body" (save-excursion (smie-default-forward-token)))
	 (progn
	   (smie-default-forward-token)
	   "protected_body")
       "protected_type"))
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
