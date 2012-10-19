;; Exploring different ways of declaring Ada statements, to see how that
;; affects smie-backward-sexp behavior

(require 'smie)

;; the comments give the result of calling smie-backward-sexp with
;; various args (via the keystrokes defined below) at various points
;; in the following Ada statement:
;;
;; procedure Debug is
;; begin
;;   if A then     -- then-1
;;      B;
;;
;;   elsif C then  -- elsif-1, then-2
;;      D;
;;
;;   elsif         -- elsif-2
;;     E then      -- then-3
;;      F;
;;
;;   else
;;     G;
;;   end fi;
;;
;;   select
;;       accept A1;
;;       B1;
;;    or
;;       accept A2;
;;       B2;
;;    or
;;       accept A3;
;;
;;    else
;;      B4;
;;    end tceles;
;; end;

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
       (if_statement))

      (if_statement
       ;; Ada syntax requires "end if;". We use "end fi;" here, to
       ;; avoid the need for refine-if code in this simple example.
       ("if" expression "then" statements "end" "fi")
       ("if" expression "then" statements "else" statements "end" "fi")
       ("if" expression "then" statements
	;; Ada syntax allows indefinite repeat of "elseif then". This
	;; does not express that explicitly, but it is allowed by
	;; smie-backward-sexp
	"elsif" expression "then" statements
	"else" statements "end" "fi")
       )
      ))))
;;("if" (26) 0) ("then" 0 1) ("elsif" 1 0) ("else" 1 1) ("end" 1 25) ("fi" 25 (27)) (";" 13 12)
;; "else" is optional, so it can't change levels; left = right. Therefore "else" can also be repeated.
;; "elsif ... then" is optional; left elsif = right then. Therefore "elsif ... then" can be repeated.

(defun use-grammar-1 () (interactive) (example-setup grammar-1))
;; calling smie-backward-sexp
;;
;; from          arg        result       point
;; after fi      nil        if           before if
;;               'halfsexp  if           before if
;;               ;          nil          bob
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

(defconst grammar-1a
  ;; add select
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
       (if_statement)
       (select_statement))

      (if_statement
       ;; Ada syntax requires "end if;". We use "end fi;" here, to
       ;; avoid the need for refine-if code in this simple example.
       ("if" expression "then" statements "end" "fi")
       ("if" expression "then" statements "else" statements "end" "fi")
       ("if" expression "then" statements
	;; Ada syntax allows indefinite repeat of "elseif then". This
	;; does not express that explicitly, but it is allowed by
	;; smie-backward-sexp
	"elsif" expression "then" statements
	"else" statements "end" "fi")
       )

      (select_statement
       ;; accept_statement, delay_statement are covered here by
       ;; "statements". "terminate" looks like a procedure call, so
       ;; we leave it as an identifier.
       ("select" statements
	"or" statements
	"else" statements "end" "tceles")
       ("select" statements
	"else" statements "end" "tceles")
       )
      ))))
;; ("if" (28) 1) ("then" 1 0) ("elsif" 0 1) ("else" 0 0) ("end" 0 25) ("fi" 25 (29))
;; same structure as grammar-1
;;
;; ("select" (27) 0) ("or" 0 0) ("else" 0 0) ("end" 0 25) ("tceles" 25 (28))
;; "or" is optional and repeater, as is "else"
;;
;; (";" 13 12) ("+" 14 30)

(defun use-grammar-1a () (interactive) (example-setup grammar-1a))
;; calling smie-backward-sexp on select statement, smie-skip-associative nil
;;
;; from          arg        result       point
;; after tceles  nil        select       before select
;;               'halfsexp  select       before select
;;               ;          select       before select
;;
;; before end    nil        ;            before end
;;               'halfsexp  else         before B4
;;               end        select       before select
;;
;; before else   nil        ;            before else
;;               'halfsexp  or           before accept A3
;;               else       or           before accept A3
;;
;; before accept A3 nil        or        before accept A3
;;                  'halfsexp  or A2     before accept A2
;;                  accept     error
;;
;; before or A3   nil        nil         before or
;;               'halfsexp  ; A2         before B2
;;               or         or A2        before accept
;;
;; before or A2   nil        nil         before or
;;               'halfsexp  ; A1         before B1
;;               or         select       before select

;; calling smie-backward-sexp on select statement, smie-skip-associative t
;;
;; from          arg        result       point
;; after tceles  nil        select       before select
;;               'halfsexp  select       before select
;;               ;          select       before select
;;
;; before end    nil        ;            before end
;;               'halfsexp  else         before B4
;;               end        select       before select
;;
;; before else   nil        ;            before else
;;               'halfsexp  or           before accept A3
;;               else       select       before accept select
;;
;; before accept A3 nil        or        before accept A3
;;                  'halfsexp  select    before select
;;                  accept     error
;;
;; before or A3   nil       ; B2         before or
;;               'halfsexp  ; A2         before B2
;;               or         select       before select
;;
;; before or A2   nil       ; B1         before or
;;               'halfsexp  ; A1         before B1
;;               or         select       before select

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
;; calling smie-backward-sexp
;;
;; from          arg        result       point
;; after fi      nil        if           before if
;;               'halfsexp  if           before if
;;               ;          nil          bob
;;
;; before end    nil        ;            before end
;;               'halfsexp  else         before G
;;               end        if           before if
;;
;; before else   nil        ;            before else
;;               'halfsexp  then         before F
;;               else       elsif-1      before E
;;
;; before F      nil        then         before F
;;               'halfsexp  elsif-2      before E
;;               F          error
;;
;; before then-3 nil        nil          before E
;;               'halfsexp  nil          before E
;;               then       elsif-1      before E

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
;; ("else" 0 0) ("end" 0 48) ("elsif" 11 23) ("then" 35 22) ("if" (49) 0) (";" 36 24) ("fi" 48 (50)) ("+" 37 51)
;; only "else" is a repeater
;; even though "then" is optional, its levels are not equal, and they also don't match elsif or any other token

(defun use-grammar-3 () (interactive) (example-setup grammar-3))
;; calling smie-backward-sexp
;;
;; from          arg        result       point
;; after fi      nil        if           before if
;;               'halfsexp  if           before if
;;               ;          nil          bob
;;
;; before end    nil        ;            before end
;;               'halfsexp  else         before G
;;               end        if           before if
;;
;; before else   nil        ;            before else
;;               'halfsexp  then         before F
;;               else       if           before if
;;
;; before F      nil        then         before F
;;               'halfsexp  elsif-1      before E
;;               F          error
;;
;; before then-3 nil        nil          before E
;;               'halfsexp  nil          before E
;;               then       elsif-1      before E

;; modula-2 grammar fragment for "if";
;; ("IF" exp "THEN" insts "END")
;; ("IF" exp "THEN" insts "ELSE" insts "END")
;; ("IF" exp "THEN" insts
;;  "ELSIF" exp "THEN" insts "ELSE" insts "END")
;; ("IF" exp "THEN" insts
;;  "ELSIF" exp "THEN" insts
;;  "ELSIF" exp "THEN" insts "ELSE" insts "END"))
;;
;; ("ELSIF" 0 1) ("THEN" 1 0) ("ELSE" 0 0) ("IF" (172) 1) (";" 45 45) ("END" 0 (187))
;; "else", "elsif ... then" optional, repeater
;;
;; (define-key m2-mode-map "\M-o" 'example-show-sexp-nil)
;; (define-key m2-mode-map "\M-p" 'example-show-sexp-halfsexp)
;; (define-key m2-mode-map "\M-[" 'example-show-sexp-token)
;;
;; calling smie-backward-sexp
;;
;; from          arg        result       point
;; after end     nil        if           before if
;;               'halfsexp  if           before if
;;               ;          begin        before if
;;
;; before end    nil        ;            before end
;;               'halfsexp  else         before G
;;               end        if           before if
;;
;; before else   nil        ;            before else
;;               'halfsexp  then         before F
;;               else       if           before if
;;
;; before F      nil        then         before F
;;               'halfsexp  if           before if
;;               F          error
;;
;; before then-3 nil        nil          before E
;;               'halfsexp  nil          before E
;;               then       if           before if

;; Ada mode 5.0 grammar fragment for "if"
;;
;; ("if-open" expression "then-if" statements "end-if" "if-end")
;; ("if-open" expression "then-if" statements "else-other" statements "end-if" "if-end")
;; ("if-open" expression "then-if" statements
;; 	"elsif" expression "then-if" statements
;; 	"else-other" statements "end-if" "if-end")
;;
;; ("if-open" (184) 3) ("then-if" 3 25) ("elsif" 25 3) ("else-other" 25 25) ("end-if" 25 127) ("if-end" 127 (174))
;; (";" 37 36)
;; "else-other" is optional and repeater
;; "then-if .. elsif" is optional and repeater
;;
;; (define-key ada-mode-map "\M-o" 'example-show-sexp-nil)
;; (define-key ada-mode-map "\M-p" 'example-show-sexp-halfsexp)
;; (define-key ada-mode-map "\M-[" 'ada-indent-show-sexp-token)
;;
;; calling smie-backward-sexp
;;
;; from          arg        result       point
;; after end     nil        if           before if
;;               'halfsexp  if           before if
;;               ;          begin        before if
;;
;; before end    nil        ;            before end
;;               'halfsexp  else         before G
;;               end        if           before if
;;
;; before else   nil        ;            before else
;;               'halfsexp  then         before F
;;               else       if           before if
;;
;; before F      nil        then         before F
;;               'halfsexp  if           before if
;;               F          error
;;
;; before then-3 nil        nil          before E
;;               'halfsexp  nil          before E
;;               then       if           before if


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

  (set (make-variable-buffer-local 'smie-skip-associative) t)

  (use-local-map example-mode-map)
)


;; end of file
