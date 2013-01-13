;;; Utilities for implementing an indentation engine using the wisent LALR parser
;;
;; Copyright (C) 2012, 2013  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
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
;;; History: first experimental version Oct 2012
;;
;;;; indentation algorithm overview
;;
;; This design is inspired in part by experience writing a SMIE
;; indentation engine for Ada; see ada-smie.el.
;;
;; The general approach to indenting a given token is to find the
;; start of the statement it is part of (or some other relevant point
;; in the statement), and indent relative to that.  So we need a
;; parser that lets us find statement starts from arbitrary places in
;; the code.
;;
;; wisent provides an LALR parser, which in general wants to start
;; from the first token in a non-terminal. The grammar has
;; non-terminals for Ada statements and declarations. The beginning of
;; the buffer always works as a starting point for the parser; it must
;; be the start of a declaration (a compilation unit).
;;
;; One of the data the parser returns is the range of each token in
;; the buffer.  An indentation engine moves text in the buffer, as
;; does user editing, so we can't rely on that range remaining
;; constant. So the parser actions create a marker at each keyword
;; token, and store other semantic information as text properties of
;; the keyword. That way, as indentation is changed, the information
;; stays in the right place.
;;
;; The other semantic information includes the marker of the first
;; keyword in the inner-most Ada statement or declaration the keyword
;; is part of, and the keyword symbol. Thus, the indentation algorithm
;; is: find the nearest keyword, fetch from it the marker for the
;; statement start, compute the indentation relative to that.
;;
;; FIXME: maybe store a list of start keywords, for all of the
;; containing statements? that would allow starting a parse one level
;; up when needed.
;;
;; Note that the first keyword in a statement is often not the first
;; token; the only keyword in a procedure call statement is the
;; terminating semicolon, assignment statements have names before the
;; ":=" keyword. Thus we also need to store information on where the
;; actual statement start is, relative to the first statement
;; keyword. We store a function symbol; the function scans a small
;; portion of Ada syntax, typically an Ada name (which match a
;; procedure call) or small number of tokens, using the low-level
;; elisp scanning functions.
;;
;; Since we have a cache, we need to consider when to invalidate
;; it. Ideally, we invalidate only when a change to the buffer would
;; change the result of a parse that crosses that change, or starts
;; after that change. Changes in whitespace (indentation and newlines)
;; do not affect an Ada parse. Other languages are sensitive to
;; newlines (Bash for example) or indentation (Python). Adding
;; comments does not change a parse, unless code is commented out. In
;; order to be conservative, for now we invalidate the cache after the
;; edit point if the change involves anything other than whitespace.
;;
;;; comparison to the SMIE parser
;;
;; The central problem to be solved in building the SMIE parser is
;; grammar precedence conflicts; the general solution is refining
;; keywords so that each new keyword can be assigned a unique
;; precedence. This means ad hoc code must be written to determine the
;; correct refinement for each language keyword from the surrounding
;; tokens. In effect, the knowledge of the language grammar is mostly
;; embedded in the refinement code; only a small amount is in the
;; refined grammar. Implementing a SMIE parser for a new language
;; involves the same amount of work as the first language.
;;
;; Using an LALR parser avoids that particular problem; since the Ada
;; language is already defined in terms of the grammar, it is only a
;; matter of a format change to teach the wisent parser the
;; language. The problem in a wisent indentation engine is caching the
;; output of the parser in a useful way, since we can't run the parser
;; from arbitrary places in the code (as we can with the SMIE
;; parser). A second problem is determining when to invalidate the
;; cache. But these problems are independent of the language being
;; parsed, so once we have one wisent indentation engine working,
;; adapting it to new languages should be quite simple.
;;
;;;; grammar compiler and parser
;;
;; wisent on its own does not provide a way to process a plain text
;; representation of BNF into an LALR parser table. It does provide
;; `wisent-compile-grammar' for compiling a lisp representation of BNF
;; into a parser table. semantic provides
;; `semantic-grammar-create-package', which parses plain text BNF
;; parser and outputs the lisp forms that `wisent-compile-grammar'
;; expects. The plain text format is closer than the lisp format to
;; the original Ada and gpr BNF, so we use that as our grammar source.
;;
;; wisent also does not provide a lexer. Semantic provides a complex
;; lexer, way overkill for our needs. So we use the elisp lexer, which
;; consists of `forward-comment', `skip-syntax-forward', and
;; `scan-sexp'. We wrap that in functions that return tokens in the
;; form wisent expects.
;;
;;; code style
;;
;; 'wisi' is short for "wisent indentation engine".
;;
;; not using lexical-binding or cl-lib because we support Emacs 23
;;
;;;;;

(require 'cl)

;;;; lexer

(defvar wisi-keyword-table nil)

(defvar wisi-string-term nil)
(defvar wisi-symbol-term nil)

(defun wisi-forward-token ()
  "Move point forward across one token, skipping whitespace and comments.
Return the corresponding token (symbol text (start end)),
where:
`symbol' is a token symbol as defined in the tokens section of the
grammar definition file,
`text' is the token text from the buffer,
`(start end)' are the character positions in the buffer of the start
and end of the token text.
If at end of buffer, returns `wisent-eoi-term'."
  ;; FIXME: handle parens
  (forward-comment (point-max))
  ;; skips leading whitespace, comment, trailing whitespace.

  (let ((start (point))
	token-id token-text)
    (cond
     ((eobp)
      (setq token-text "")
      (setq token-id wisent-eoi-term))

     ((eq 7 (syntax-class (syntax-after (point))))
      ;; a string quote. we assume we are before the start quote, not the end quote
      (let ((forward-sexp-function nil))
	(forward-sexp))
      (setq token-text (buffer-substring-no-properties start (point)))
      (setq token-id wisi-string-term))

     (t
      (if (zerop (skip-syntax-forward "."))
	  (skip-syntax-forward "w_'"))
      (setq token-text (buffer-substring-no-properties start (point)))
      (setq token-id
	    (or (symbol-value (intern-soft token-text wisi-keyword-table))
		wisi-symbol-term)))
     );; cond

    (unless token-id
      (error "unrecognized token %s" (setq token-text (buffer-substring-no-properties start (point)))))
    (list token-id token-text (list start (point)))
    ))

(defun wisi-backward-token ()
  "Move point backward across one token, skipping whitespace and comments."
  ;; FIXME: handle parens, strings
  (forward-comment (- (point-max)))
  ;; skips leading whitespace, comment, trailing whitespace.

  (if (zerop (skip-syntax-backward "."))
      (skip-syntax-backward "w_'"))
  )

;;;; token info cache
;;
;; the cache stores the results of parsing as text properties on
;; keywords, for use by the indention and motion engines.

(defstruct
  (wisi-cache
   (:constructor wisi-cache-create)
   (:copier nil))
  symbol

  class ;; 'block-start 'block-end 'block-both 'statement-end 'other

  start
  ;; a) if this is not first keyword; mark at the first keyword of the current statement
  ;; b) if this is first keyword; function to move from this keyword to first token or nil

  ;; parent FIXME: not needed for indentation yet; needed for motion?
  ;; mark at the first keyword of the containing statement.  Non-nil only
  ;; on first keyword in statement

  ;; child FIXME: not needed for indentation yet; needed for motion?
  ;; a) if this keyword is a block-start, mark at the first keyword of the first contained statement
  ;; b) if this keyword is a block-end, mark at the first keyword of the last contained statement
  ;; otherwise nil

  )

(defvar wisi-cache-max 0
  "Maximimum position in buffer where wisi token cache is valid.")
(make-variable-buffer-local 'wisi-cache-max)

(defvar wisi-parse-table nil)

(defun wisi-invalidate-cache()
  "Invalidate the wisi token cache for the current buffer."
  (interactive)
  (setq wisi-cache-max 0)
  (with-silent-modifications
    (remove-text-properties (point-min) (point-max) '(wisi-cache))))

(defvar wisi-inhibit-invalidate nil)

(defun wisi-after-change (begin end length)
  "For `after-change-functions'."
  ;; FIXME: semantic-parse-region supports incremental parse, which
  ;; means only invalidate cache after change point?
  (when wisi-inhibit-invalidate
    (wisi-invalidate-cache)))

(defun wisi-get-cache (pos)
  ;; FIXME: is `pos' ever not (point)?
  "Return info from the `wisi-cache' text property at POS.
Info is '(keyword-symbol first-keyword-mark) or nil."
  (get-text-property pos 'wisi-cache))

(defun wisi-validate-cache (pos)
  "Ensure cached data is valid at least up to POS.
Parse buffer if necessary."
  (if (< wisi-cache-max pos)
      ;; FIXME: could try to resume parse at wisi-cache-max. Let's see
      ;; how slow this is.
      (save-excursion
	(goto-char (point-min))
	;; FIXME: tell lexer to stop at pos
	(wisent-parse wisi-parse-table 'wisi-forward-token))))

;;;; defining parse actions

(defun wisi-cache-keywords (&rest items)
  "Cache information used by wisi in text properties of keywords.
Intended as a wisent grammar non-terminal action.  ITEMS is a
list [symbol class start-func (start end)] ... for keywords
that should receive cached information. In a wisent grammar file, this
is used as:

subprogram
  : PROCEDURE SYMBOL
    (wisi-cache-keywords
      $1 'procedure nil $region1
      $2 'other nil $region2)
"
  ;; FIXME: update docstring with use of start-func

  (save-excursion
    (let (first-keyword-mark)
      (while items
	(let* ((symbol (pop items))
	       (class (pop items))
	       (start-func (pop items))
	       (region (car (pop items)))
	       (mark
		(progn (goto-char (car region))
		       (point-marker))))

	  ;; The first keyword in a statement should have a start-func
	  ;; that goes to the containing block statement start; the
	  ;; others just store the marker for the first keyword.
	  ;; FIXME: maintain a stack of block start markers?
	  (put-text-property
	   (car region)
	   (cadr region)
	   'wisi-cache
	   (wisi-cache-create
	    :symbol symbol
	    :class class
	    :start (or first-keyword-mark start-func)
	    ))

	  (unless first-keyword-mark (setq first-keyword-mark mark))
	))
      )))

(defmacro wisi-cache-action (&rest pairs)
  "Macro to define a wisi grammar action, which is a call to `wisi-cache-keywords'.
PAIRS is of the form [TOKEN-NUMBER CLASS] ...  where TOKEN-NUMBER
is the token number in the production, CLASS is the wisi class of
that token; see `wisi-cache'. Must be in backquotes in a wisent
grammar action."
  (let (class list number region)
    (while pairs
      (setq number (pop pairs))
      (setq region (intern (concat "$region" (number-to-string number))))
      (setq number (intern (concat "$" (number-to-string number))))
      (setq class (pop pairs))
      (setq list (cons (list 'quote (list number class nil region)) list)))
    (cons 'wisi-cache-keywords list)
  ))

;; FIXME: overriding semantic/wisent/comp
;; wisent-semantic-action-expand-body to allow macros in actions to
;; work
(defun wisent-semantic-action-expand-body (body n &optional found)
  "Parse BODY of semantic action.
N is the maximum number of $N variables that can be referenced in
BODY.  Warn on references out of permitted range.
Optional argument FOUND is the accumulated list of '$N' references
encountered so far.
Return a cons (FOUND . XBODY), where FOUND is the list of $N
references found in BODY, and XBODY is BODY expression with
`backquote' forms expanded."
  (if (not (listp body))
      ;; BODY is an atom, no expansion needed
      (progn
        (if (wisent-check-$N body n)
            ;; Accumulate $i symbol
            (add-to-list 'found body))
        (cons found body))
    ;; BODY is a list, expand inside it
    (let (xbody sexpr)
      ;; If backquote expand it first
      (if (wisent-backquote-p (car body))
          (setq body (macroexpand (eval body))))
      (while body
        (setq sexpr (car body)
              body  (cdr body))
        (cond
         ;; Function call excepted quote expression
         ((and (consp sexpr)
               (not (wisent-quote-p (car sexpr))))
          (setq sexpr (wisent-semantic-action-expand-body sexpr n found)
                found (car sexpr)
                sexpr (cdr sexpr)))
         ;; $i symbol
         ((wisent-check-$N sexpr n)
          ;; Accumulate $i symbol
          (add-to-list 'found sexpr))
         )
        ;; Accumulate expanded forms
        (setq xbody (nconc xbody (list sexpr))))
      (cons found xbody))))

;;(defun wisi-update-children ()
    ;;   goto last token (";")
    ;; find previous ";"
    ;; (wisi-goto-statement-start nil); the first keyword, not the actual start
    ;; set child marker on parent ";" - last contained child leading keyword

    ;; loop
    ;;     add parent marker
    ;;     (wisi-goto-statement-start t)
    ;;     backward-token - ";"
    ;;         exit when none; begin or declare
    ;;     (wisi-goto-statement-start nil)
    ;; end loop

    ;; goto parent token ("declare" for declarations, "begin" for statements, "then" for if, etc)
    ;; set child marker on parent - first contained child leading keyword
;;)

;;;; motion
(defun wisi-prev-keyword ()
  "Move point to the beginning of the keyword preceding point, skipping over non-keyword tokens.
Return cache from the found keyword."
  (let (cache)
    (while (and (not (bobp))
		(not cache))
      (wisi-backward-token)
      (setq cache (wisi-get-cache (point))))))

(defun wisi-goto-statement-start (cache &optional keyword)
  "Move point to statement start of statement containing CACHE.
If KEYWORD non-nil, only go to first keyword."
  (cond
   ((markerp (wisi-cache-start cache))
    (goto-char (wisi-cache-start cache))
    (wisi-goto-statement-start (wisi-get-cache (point)) keyword))

   ((and (not keyword)
	 (functionp (wisi-cache-start cache)))
    (funcall (wisi-cache-start cache)))

   (t nil)))

;;;; indentation

(defun wisi-indent-statement-start (offset cache)
  "Return indentation OFFSET relative to line containing start of current statement.
CACHE contains cache info from a keyword in the current statement."
  (save-excursion
    (wisi-goto-statement-start cache)
    (back-to-indentation)
    (+ (current-column) offset)))

(defvar wisi-indent-calculate-function nil
  "Function to calculate indentation. Called with point before a
  token; return indentation column for that token, or
  'noindent."
  )

(defun wisi-indent-line ()
  "Indent current line using the wisi indentation engine."
  ;; copied shamelessly from smie
  (interactive)

  (wisi-validate-cache (point))

  (let* ((savep (point))
	 (indent (or (with-demoted-errors
                       (save-excursion
                         (forward-line 0)
                         (skip-chars-forward " \t")
                         (if (>= (point) savep) (setq savep nil))
                         (or (funcall wisi-indent-calculate-function) 0)))
                     0)))
    (if (not (numberp indent))
        ;; If something funny is used (e.g. `noindent'), return it.
        indent
      (if (< indent 0) (setq indent 0)) ;Just in case.

      (let ((wisi-inhibit-invalidate t))
	;; We are only changing whitespace, so tell wisi-after-change
	;; not to invalidate cache.
	(if savep
	    ;; point was inside line text; leave it there
	    (save-excursion (indent-line-to indent))
	  ;; point was before line text; move to start of text
	  (indent-line-to indent)))
      )))

;;;; debug
(defun wisi-parse-buffer ()
  (interactive)
  (wisent-parse-region (point-min) (point-max)))

(defun wisi-show-cache ()
  "Show cache at point."
  (interactive)
  (message "%s" (wisi-get-cache (point))))

;;;; setup

(defun wisi-setup (indent-calculate keyword-table token-table parse-table)
  "Set up a buffer for parsing files with wisi."

  (unless (setq wisi-string-term (car (symbol-value (intern-soft "string" token-table))))
    (error "grammar does not define <string> token"))

  (unless (setq wisi-symbol-term (car (symbol-value (intern-soft "symbol" token-table))))
    (error "grammar does not define <symbol> token"))

  (set (make-local-variable 'wisi-indent-calculate-function) indent-calculate)
  (set (make-local-variable 'wisi-keyword-table) keyword-table)
  (set (make-local-variable 'wisi-parse-table) parse-table)
  (set (make-local-variable 'indent-line-function) 'wisi-indent-line)

  ;; FIXME: do we want this?
  ;; (semantic-make-local-hook 'wisi-discarding-token-functions)
  ;; (add-hook 'wisi-discarding-token-functions
  ;; 	    'wisi-collect-unmatched-syntax nil t))

  ;; FIXME: (set (make-local-variable 'forward-sexp-function) 'wisi-forward-sexp-command)
  ;; This is nice for user navigation; do we need it for wisi?

  )

(provide 'wisi)

;;; end of file
