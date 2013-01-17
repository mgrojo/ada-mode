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
  (forward-comment (point-max))
  ;; skips leading whitespace, comment, trailing whitespace.

  (let ((start (point))
	;; (info "(elisp)Syntax Table Internals" "*info elisp syntax*")nil
	(syntax (syntax-class (syntax-after (point))))
	token-id token-text)
    (cond
     ((eobp)
      (setq token-text "")
      (setq token-id wisent-eoi-term))

     ((memq syntax '(4 5)) ;; open, close parenthesis
      (forward-char 1)
      (setq token-text (buffer-substring-no-properties start (point)))
      (setq token-id (symbol-value (intern-soft token-text wisi-keyword-table))))

     ((eq syntax 7)
      ;; string quote. we assume we are before the start quote, not the end quote
      (let ((forward-sexp-function nil))
	(forward-sexp))
      (setq token-text (buffer-substring-no-properties start (point)))
      (setq token-id wisi-string-term))

     (t
      (if (zerop (skip-syntax-forward "."))
	  (skip-syntax-forward "w_'"))
      (setq token-text (buffer-substring-no-properties start (point)))
      (setq token-id
	    (or (symbol-value (intern-soft (downcase token-text) wisi-keyword-table))
		wisi-symbol-term)))
     );; cond

    (unless token-id
      (error "unrecognized token '%s'" (setq token-text (buffer-substring-no-properties start (point)))))
    (list token-id token-text (list start (point)))
    ))

(defun wisi-backward-token ()
  "Move point backward across one token, skipping whitespace and comments."
  (forward-comment (- (point)))
  ;; skips leading whitespace, comment, trailing whitespace.

  ;; (info "(elisp)Syntax Table Internals" "*info elisp syntax*")
  (let ((syntax (syntax-class (syntax-after (1- (point))))))
    (cond
     ((bobp) nil)

     ((memq syntax '(4 5)) ;; open, close parenthesis
      (backward-char 1))

     ((eq syntax 7)
      ;; a string quote. we assume we are after the end quote, not the start quote
      (let ((forward-sexp-function nil))
	(forward-sexp -1)))

     (t
      (if (zerop (skip-syntax-backward "."))
	  (skip-syntax-backward "w_'")))
   )))

;;;; token info cache
;;
;; the cache stores the results of parsing as text properties on
;; keywords, for use by the indention and motion engines.

(defstruct
  (wisi-cache
   (:constructor wisi-cache-create)
   (:copier nil))
  symbol

  class ;; list of classes is language-specific

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

(defun wisi-before-change (begin end)
  "For `before-change-functions'."
  (save-excursion
    (let (;; (info "(elisp)Parser State")
	  (state (syntax-ppss begin)))
      ;; syntax-ppss has moved point to "begin".
      (cond
       ((or
	 (nth 3 state); in string
	 (nth 4 state)); in comment
	(setq wisi-change-need-invalidate nil))

       ((progn
	  (skip-syntax-forward " " end)
	  (eq (point) end))
	(setq wisi-change-need-invalidate nil))

       (t (setq wisi-change-need-invalidate t))
       ))))

(defun wisi-after-change (begin end length)
  "For `after-change-functions'."
  ;; FIXME: semantic-parse-region supports incremental parse, which
  ;; means only invalidate cache after change point?
  (when (>= wisi-cache-max begin)
    (save-excursion
      (let ((need-invalidate t)
	    ;; (info "(elisp)Parser State")
	    (state (syntax-ppss begin)))
	;; syntax-ppss has moved point to "begin".
	(cond
	 ((or
	   (nth 3 state); in string
	   (nth 4 state)); in comment
	  (setq need-invalidate nil))

	 ((progn
	    (skip-syntax-forward " " end)
	    (eq (point) end))
	  (setq need-invalidate nil))

	 (t nil)
	 )
	(if (or wisi-change-need-invalidate
		need-invalidate)
	    (wisi-invalidate-cache)
	  ;; else move cache-max by the net change length
	  (setq wisi-cache-max
		(+ wisi-cache-max (- end begin length))))
	))))

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
	(wisent-parse wisi-parse-table 'wisi-forward-token)
	(setq wisi-cache-max (point)))))

;;;; defining parse actions

(defun wisi-cache-tokens (&rest items)
  "Cache information used by wisi in text properties of tokens.
Intended as a wisent grammar non-terminal action.  ITEMS is a
list [symbol class (start end)] ... for tokens that should
receive cached information. See macro `wisi-cache-action' for
using this in a wisent grammar file."
  (save-excursion
    (let (first-keyword-mark)
      (while items
	(let* ((symbol (pop items))
	       (class (pop items))
	       (region (car (pop items)))
	       (mark
		(progn (goto-char (car region))
		       ;; Marker one char into token, so
		       ;; indent-line-to inserts space before the
		       ;; mark, not after!
		       (copy-marker (1+ (point))))))

	  ;; :start is a marker to the first keyword in a
	  ;; statement. The first keyword in a contained statement has
	  ;; a marker to the first keyword in the containing
	  ;; statement; the outermost containing statement has
	  ;; nil. `wisi-update-start' sets the start markers in
	  ;; contained statements.
	  (with-silent-modifications
	    (put-text-property
	     (car region)
	     (cadr region)
	     'wisi-cache
	     (wisi-cache-create
	      :symbol symbol
	      :class class
	      :start first-keyword-mark)
	     ))

	  (unless first-keyword-mark (setq first-keyword-mark mark))
	))
      )))

(defmacro wisi-cache-action (&rest pairs)
  "Macro to define a wisi grammar action that is a call to `wisi-cache-tokens'.
PAIRS is of the form [TOKEN-NUMBER CLASS] ...  where TOKEN-NUMBER
is the token number in the production, CLASS is the wisi class of
that token. Use in a grammar action as:

`,(wisi-cache-action 1 'statement-start 7 'statement-end)"
  (let (class list number region)
    (while pairs
      (setq number (pop pairs))
      (setq region (intern (concat "$region" (number-to-string number))))
      (setq number (intern (concat "$" (number-to-string number))))
      (setq class (pop pairs))
      (setq list (append list (list number class region))))
    (cons 'wisi-cache-tokens list)
  ))

(defun wisi-update-start (start-region end-region class)
  "Set start marks in all tokens in (caar START-REGION) to (cadar
END-REGION) with class CLASS to marker pointing to start of
region.  See `wisi-update-action' for use in grammar action."
  (save-excursion
    (goto-char (nth 1 (car end-region)))
    (let ((token (wisi-prev-cache))
	  (mark (copy-marker (1+ (nth 0 (car start-region)))))
	  token-region)
      (while token
	(setq cache (car token))
	(setq token-region (cadr token))
	(when (eq (wisi-cache-class cache) class)
	  (setf (wisi-cache-start cache) mark)
	  (with-silent-modifications
	    (put-text-property (car token-region) (cadr token-region) 'wisi-cache cache)))
	(setq token (wisi-prev-cache))
	(when (< (point) (cadar start-region))
	  (setq token nil))
	))))

(defmacro wisi-update-action (start-token end-token class)
  "Macro to define a wisi grammar action that is a call to `wisi-update-start'.
START-TOKEN is token number of first token in containing statement,
END-TOKEN is token number of last token in containing statement,
CLASS is the class of tokens to update."
  ;; $region is often nil where we want to use this, so we use two token regions.
  (list 'wisi-update-start
	(intern (concat "$region" (number-to-string start-token)))
	(intern (concat "$region" (number-to-string end-token)))
	class))

;; patch wisent grammar; allow "`,(macro ...)" actions
(require 'semantic)
(define-lex-sexp-type-analyzer semantic-grammar-wy--<qlist>-sexp-analyzer
  "sexp analyzer for <qlist> tokens."
  "\\s'\\s'?\\s-*("
  'PREFIXED_LIST)

;;;; motion
(defun wisi-prev-cache ()
  "Move point to the beginning of the first token preceding point that has a cache.
Returns (cache region); the found cache, and the text region
containing it; or nil if at beginning of buffer."
  (let (begin end)
    (when (not (bobp))
      (setq end (previous-single-property-change (point) 'wisi-cache))
      (when end
	(if (get-text-property (1- end) 'wisi-cache)
	    (setq begin (previous-single-property-change end 'wisi-cache))
	  ;; else single-character token
	  (setq begin end))
	(goto-char begin)
	(list (get-text-property begin 'wisi-cache)
	      (list begin end))))
    ))

(defun wisi-goto-statement-start (cache)
  "Move point to statement start of statement containing CACHE.
If at start of a statement, goto start of containing statement"
  (cond
   ((markerp (wisi-cache-start cache))
    (goto-char (1- (wisi-cache-start cache))))

   (t
    ;; at start of outermost containing statement
    nil)))

;;;;; indentation

(defun wisi-comment-indent ()
  "For `comment-indent-function'. Indent single line comment to
the comment on the previous line."
  ;; This should only be called by comment-indent-new-line or
  ;; fill-comment-paragraph, so there will be a preceding comment line
  ;; that we can trust.
  (save-excursion
    (forward-comment -1)
    (if (looking-at comment-start)
	(current-column)
      (error "wisi-comment-indent called after non-comment"))))

(defun wisi-indent-current (offset)
  "Return indentation OFFSET relative to indentation of current line."
  (save-excursion
    (back-to-indentation)
    (+ (current-column) offset)))

(defun wisi-indent-paren (offset)
  "Return indentation OFFSET relative to preceding open paren."
  (save-excursion
    (let ((done nil)
	  cache)
      (while (not done)
	(setq cache (car (wisi-prev-cache)))
	(setq done
	      (or (not cache);; bob
		  (eq (wisi-cache-class cache) 'open-paren)))))
    (+ (current-column) offset)))

(defun wisi-indent-statement-start (offset cache)
  "Return indentation OFFSET relative to line containing start of current statement.
CACHE contains cache info from a keyword in the current statement."
  (save-excursion
    (wisi-goto-statement-start cache)
    (back-to-indentation)
    (+ (current-column) offset)))

(defvar wisi-indent-calculate-functions nil
  "Functions to calculate indentation. Each called with point before a
  token; return indentation column for that token, or
  nil. Calling stops when first function returns non-nil.")

(defun wisi-indent-line ()
  "Indent current line using the wisi indentation engine."
  (interactive)

  (wisi-validate-cache (point))

  (let* ((savep (point))
	 (indent (or (with-demoted-errors
                       (save-excursion
                         (forward-line 0)
                         (skip-chars-forward " \t")
                         (if (>= (point) savep) (setq savep nil))
                         (or (run-hook-with-args-until-success 'wisi-indent-calculate-functions) 0)))
                     0)))
    (if savep
	;; point was inside line text; leave it there
	(save-excursion (indent-line-to indent))
      ;; point was before line text; move to start of text
      (indent-line-to indent))
    ))

;;;; debug
(defun wisi-parse-buffer ()
  (interactive)
  (wisi-invalidate-cache)
  (wisi-validate-cache (point-max)))

(defun wisi-show-cache ()
  "Show cache at point."
  (interactive)
  (message "%s" (wisi-get-cache (point))))

(defun wisi-show-token ()
  "Move forward across one keyword, show token_id."
  (interactive)
  (let ((token (wisi-forward-token)))
    (message "%s" (car token))))

;;;; setup

(defun wisi-setup (indent-calculate keyword-table token-table parse-table)
  "Set up a buffer for parsing files with wisi."

  (setq wisent-parse-verbose-flag t); to get syntax error messages

  (unless (setq wisi-string-term (car (symbol-value (intern-soft "string" token-table))))
    (error "grammar does not define <string> token"))

  (unless (setq wisi-symbol-term (car (symbol-value (intern-soft "symbol" token-table))))
    (error "grammar does not define <symbol> token"))

  (set (make-local-variable 'wisi-indent-calculate-functions) indent-calculate)
  (set (make-local-variable 'wisi-keyword-table) keyword-table)
  (set (make-local-variable 'wisi-parse-table) parse-table)
  (set (make-local-variable 'indent-line-function) 'wisi-indent-line)

  (add-hook 'before-change-functions 'wisi-before-change nil t)
  (add-hook 'after-change-functions 'wisi-after-change nil t)

  ;; FIXME: do we want this?
  ;; (semantic-make-local-hook 'wisi-discarding-token-functions)
  ;; (add-hook 'wisi-discarding-token-functions
  ;; 	    'wisi-collect-unmatched-syntax nil t))

  ;; FIXME: (set (make-local-variable 'forward-sexp-function) 'wisi-forward-sexp-command)
  ;; This is nice for user navigation; do we need it for wisi?

  )

(provide 'wisi)

;;; end of file
