;;; Ada mode indentation engine, based on SMIE
;;
;; [1] ISO/IEC 8652:201z (draft 18); Ada 2012 reference manual

(require 'smie)

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
       (exp) ; FIXME: expand this

       ;; BNF from [1] appendix P, vastly simplified
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
       ;; add rules as we uncover a need for them.
       ;;
       ;; We start from the top down; first the compilation units.
       ;;
       ;; SMIE automatically allows balanced parens anywhere, so we
       ;; don't need to declare argument lists or discriminant lists
       ;; in the grammar

       ;; 'is' is used at several levels, so the lexer returns several
       ;; different tokens for it, so that smie-indent--parent will
       ;; identify the correct parent.
       ;;
       ;; ';' has a similar problem; it is used in several different
       ;; constructs, where we need to correctly identify the
       ;; parent. We solve that by declaring it as the separator for
       ;; those constructs.

       (package ("package" "body" identifier "is-package" declarations "begin" statements "end"))

       (declarations
	(declaration)
	(declaration ";" declaration))

       (declaration
	(array_type_definition)
	(record_type_definition)
	(protected_type_definition)
	;(entry_declaration)
	)

       (array_type_definition ("type" identifier "is-type" "array" exp "of" identifier))

       (record_type_definition ("type" identifier "is-type" "record" components "end"))

       (components
	(component)
	(component ";" component))

       (component (identifier ":" identifier)) ; discriminants are a balanced paren; FIXME: constraints?

       (protected_type_definition
	;; prefixing "protected" gives a precedence conflict: 'token
	;; type is both neither and opener', so "protected type" is
	;; combined into one token in
	;; ada-indent-forward/backward-token.
	("protected_type" identifier "is-type" declarations "private" declarations "end"))

;       (entry_declaration ("entry" identifier)); family subtype, parameter_profile are balanced parens

       (statements
	(statement)
	(statement ";" statement))

       (statement
	(exp); matches procedure calls
	; FIXME: more
	)

       ))

    ;; operators and similar things
    ;; FIXME: ref RM, get it right
    (smie-precs->prec2
     '((assoc ",")
       (nonassoc "<" "<=" ">=" ">" "/=" "in")
       (assoc "or" "+" "-")
       (assoc "and" "mod" "rem" "*" "/" "&")
       (nonassoc "not")
       (left "." "^")
       ))
    )))

(defun ada-indent-goto-parent ()
  "Go to the parent of the current token.
Return value has the same structure as smie-backward-sexp"
  (let (smie--parent smie--after smie--token)
    (smie-indent--parent)))

(defun ada-token-refine-is ()
  (cond
   ;; using ada-indent-goto-parent to get to the parent token screws
   ;; up here; not clear why. FIXME: because that relies on
   ;; recognizing 'is', which is what we are trying to do; try going
   ;; back one token, then call goto-parent.
   ;;
   ;; "package" "body" identifier "is" FIXME: "identifier" could have dots!
   ((equal "package" (save-excursion
		       (smie-default-backward-token)
		       (smie-default-backward-token)
		       (smie-default-backward-token)))
    "is-package")
   ;; "type" identifier "is"
   ((equal "type" (save-excursion
		    (smie-default-backward-token)
		    (smie-default-backward-token)))
    "is-type")
   (t
    (error "ada-indent-backward-token: unrecognized 'is'"))))

(defun ada-indent-forward-token ()
  (pcase (smie-default-forward-token)
    (`"is" (ada-token-refine-is))
    (`"protected" (if (equal "type" (save-excursion (smie-default-forward-token))) "protected_type" "type"))
    (token token)))

(defun ada-indent-backward-token ()
  (pcase (smie-default-backward-token)
    (`"is" (ada-token-refine-is))

    (`"type" (if (equal "protected" (save-excursion (smie-default-backward-token)))
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
    (:after
     (pcase arg
       ((or `"is-type" `"is-package")
	;; indent relative to the start of the declaration, which is the parent of 'is'
	(smie-rule-parent ada-indent))
       (`";"
	;; indent relative to the start of the block or parameter list
	(smie-rule-parent ada-indent)))
     )))

(defun ada-indent-setup ()
  (smie-setup ada-indent-grammar #'ada-indent-rules
	      :forward-token #'ada-indent-forward-token
	      :backward-token #'ada-indent-backward-token))

(add-hook 'ada-mode-hook 'ada-indent-setup)

(define-key ada-mode-map "\t" 'indent-for-tab-command)
;; TAB will now use smie indentation in Ada mode buffers

(provide 'ada-indent)

;;; end of file
