;; Exploring different ways of declaring Ada statements, to see how that
;; affects smie-backward-sexp behavior

(require 'smie)

;; the comments give the result of calling smie-backward-sexp with
;; various args (via the keystrokes defined below) at various points
;; in the following Ada statement:
;;
;; if A then     -- then-1
;;    B;
;;
;; elsif C then  -- elsif-1, then-2
;;    D;
;;
;; elsif         -- elsif-1
;;   E then      -- then-3
;;    F;
;;
;; else
;;   G;
;;
;; end fi;

(defconst grammar-1
  (smie-prec2->grammar
   (smie-bnf->prec2
    '(
      (identifier)

      (expression
       (identifier)
       (expression "+" identifier))

      (statements
       (statement)
       (statement ";" statement))

      (statement
       ;; Ada syntax requires "end if;". We use "end fi;" here, to
       ;; avoid the need for refine-if code in this simple example.
       ("if" expression "then" statements "end" "fi")
       ("if" expression "then" statements "else" statements "end" "fi")
       ("if" expression "then" statements
	;; Ada syntax allows indefinite repeat of "elseif then". This
	;; does not express that explicitly, but it is allowed FIXME:
	;; precisely what does "allowed" mean here? The parser doesn't
	;; return a good/bad verdict? "the parser" is
	;; smie-backward-sexp; how does it behave for this?
	"elsif" expression "then" statements
	"else" statements "end" "fi")
       )))))
;(("elsif" 1 0) ("else" 1 1) ("end" 1 25) ("then" 0 1) ("if" (26) 0) (";" 13 12) ("fi" 25 (27)) ("+" 14 28))
; "else" is optional, so it can't change levels; left = right
; "elsif ... then" is optional; left elsif = right then

(defun use-grammar-1 () (interactive) (example-setup grammar-1))
;; calling smie-backward-sexp
;;
;; from          arg        result       point
;; after fi      nil        if           before if
;;               'halfsexp  if           before if
;;               ;          bob          bob
;;
;; before end    nil        ;            before end
;;               'halfsexp  else         before G
;;               end        if           before if
;;
;; before else   nil        ;            before else
;;               'halfsexp  then         before F
;;               else        if          before if
;;
;; before F      nil        then         before F
;;               'halfsexp  if           before if
;;               F          error
;;
;; before then-3 nil        nil          before E
;;               'halfsexp  nil          before E
;;               then       if           before if

(defconst grammar-2
  (smie-prec2->grammar
   (smie-bnf->prec2
    '(
      (identifier)

      (expression
       (identifier)
       (expression "+" identifier))

      (statements
       (statement)
       (statement ";" statement))

      (statement
       ;; variant suggested by Stefan Monnier; does not explicitly
       ;; express allowed repeats of "elsif exp then", but changes the
       ;; levels assigned by the compiler, thus changing the behavior
       ;; of smie-backward-sexp.
       ("if" exp-then-stmt "end" "fi")
       ("if" exp-then-stmt "else" statements "end" "fi")
       ("if" exp-then-stmt "elsif" exp-then-stmt "end" "fi")
       ("if" exp-then-stmt "elsif" exp-then-stmt "else" statements "end" "fi"))
      (exp-then-stmt
       (expression "then" statements))
      ))))
; ("elsif" 0 0) ("else" 0 0) ("end" 0 36) ("then" 12 11) ("if" (37) 0) (";" 25 13) ("fi" 36 (38)) ("+" 14 39)
; grammar compiler sees "elsif" as optional, even though it is actually "elsif then" that is optional

(defun use-grammar-2 () (interactive) (example-setup grammar-2))

(defconst grammar-3
  (smie-prec2->grammar
   (smie-bnf->prec2
    '(
      (identifier)

      (expression
       (identifier)
       (expression "+" identifier))

      (statements
       (statement)
       (statement ";" statement))

      (statement
       (if_statement))

      (if_statement
       ;; This explicitly expresses the allowed repeats of elsif_clause

       ("if" exp-then-stmt-s "end" "fi")
       ("if" exp-then-stmt-s "else" statements "end" "fi"))

      (exp-then-stmt-s
       (exp-then-stmt)
       (exp-then-stmt-s "elsif" exp-then-stmt))

      (exp-then-stmt
       (expression "then" statements))
      ))))

(defun example-rules (method arg)
  ;; just meets the rules API, never intended to be called
  nil)

(defun example-show-keyword-backward ()
  "Show the smie grammar info for word preceding point, and move across it."
  (interactive)
  (message "%s" (assoc (smie-default-backward-token) smie-grammar)))

(defun example-show-sexp-nil ()
  (interactive)
  (message "%s" (smie-backward-sexp nil)))

(defun example-show-sexp-halfsexp ()
  (interactive)
  (message "%s" (smie-backward-sexp 'halfsexp)))

(defun example-show-sexp-token ()
  (interactive)
  (message
   "%s"
   (smie-backward-sexp
    (save-excursion (smie-default-forward-token)))))

(defvar example-mode-map (make-sparse-keymap)
  "Local keymap used for Example mode.")

(define-key example-mode-map "\M-i" 'example-show-keyword-backward)
(define-key example-mode-map "\M-o" 'example-show-sexp-nil)
(define-key example-mode-map "\M-p" 'example-show-sexp-halfsexp)
(define-key example-mode-map "\M-[" 'example-show-sexp-token)

(defun example-setup (grammar)
  (smie-setup grammar
	      #'example-rules
	      :forward-token #'smie-default-forward-token
	      :backward-token #'smie-default-backward-token)

  (setq major-mode 'example-mode
	mode-name "Example")

  (use-local-map example-mode-map)
)


;; end of file
