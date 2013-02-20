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
;; An indentation engine moves text in the buffer, as does user
;; editing, so we can't rely on character positions remaining
;; constant. So the parser actions use markers to store positions, and
;; store other semantic information as text properties of the
;; tokens. That way, as indentation is changed, the information stays
;; in the right place.
;;
;; The other semantic information includes a marker at the first token
;; of the statement. Thus, the indentation algorithm is: find the
;; nearest token with cached information, fetch from it the marker for
;; the statement start, compute the indentation relative to that.
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
;; Using an LALR parser avoids that particular problem; since the
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
;; The SMIE parser does not find the start of each statement, only the
;; first language keyword in each statement; additional code must be
;; written to find the statement start. The wisent parser finds the
;; statement start directly.
;;
;; It is easier to use nested non-terminals in a wisi parser. In SMIE,
;; it is best if each grammar rule is a complete statement, so
;; forward-sexp will traverse the entire statement. If nested
;; non-terminals are used, forward-sexp may stop inside one of the
;; nested non-terminals. This problem does not occur with the wisi
;; parser.
;;
;; A downside of the wisi parser is conflicts in the grammar; they can
;; be much more difficult to resolve than in the SMIE parser.
;;
;;;; grammar compiler and parser
;;
;; wisent on its own does not provide a way to process a plain text
;; representation of BNF into an LALR parser table. It does provide
;; `wisent-compile-grammar' for compiling a lisp representation of BNF
;; into a parser table. semantic provides
;; `semantic-grammar-create-package', which parses plain text BNF in
;; near-bison format and outputs the lisp forms that
;; `wisent-compile-grammar' expects.
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
(defvar wisi-punctuation-table-max-length 0)
(defvar wisi-punctuation-table nil)
(defvar wisi-string-term nil)
(defvar wisi-symbol-term nil)

(defun wisi-error (message &rest args)
  (error
   "%s:%d: %s"
   (file-name-nondirectory (buffer-file-name))
   (line-number-at-pos)
   (apply 'format message args)))

(defun wisi-forward-token (&optional text-only lower)
  "Move point forward across one token, skipping leading whitespace and comments.
Return the corresponding token, in a format determined by TEXT-ONLY:
TEXT-ONLY t:          text
TEXT-ONLY nil:        (symbol text start . end)
where:
`symbol' is a token symbol (not string) from `wisi-punctuation-table',
`wisi-keyword-table', `wisi-string-term' or `wisi-symbol-term'.

`text' is the token text from the buffer (lowercase if LOWER),

`start, end' are the character positions in the buffer of the start
and end of the token text.

If at end of buffer, returns `wisent-eoi-term'."
  (forward-comment (point-max))
  ;; skips leading whitespace, comment, trailing whitespace.

  (let ((start (point))
	;; (info "(elisp)Syntax Table Internals" "*info elisp syntax*")
	(syntax (syntax-class (syntax-after (point))))
	token-id token-text)
    (cond
     ((eobp)
      (setq token-text "")
      (setq token-id wisent-eoi-term))

     ((eq syntax 1)
      ;; punctuation. Find the longest matching string in wisi-punctuation-table
      (forward-char 1)
      (let ((next-point (point))
	    temp-text temp-id done)
	(while (not done)
	  (setq temp-text (buffer-substring-no-properties start (point)))
	  (setq temp-id (car (rassoc temp-text wisi-punctuation-table)))
	  (when temp-id
	    (setq token-text temp-text
		  token-id temp-id
		  next-point (point)))
	  (if (= (- (point) start) wisi-punctuation-table-max-length)
	      (setq done t)
	    (forward-char 1))
	  )
	(goto-char next-point)))

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

     (t ;; assuming word syntax
      (skip-syntax-forward "w_'")
      (setq token-text (buffer-substring-no-properties start (point)))
      (setq token-id
	    (or (symbol-value (intern-soft (downcase token-text) wisi-keyword-table))
		wisi-symbol-term)))
     );; cond

    (unless token-id
      (wisi-error "unrecognized token '%s'" (buffer-substring-no-properties start (point))))

    (if text-only
	(if lower
	    (lowercase token-text)
	  token-text)
      (cons token-id (cons token-text (cons start (point)))))
    ))

(defun wisi-backward-token ()
  "Move point backward across one token, skipping whitespace and comments.
Return token text."
  ;; FIXME: not used?
  (forward-comment (- (point)))
  ;; skips leading whitespace, comment, trailing whitespace.

  ;; (info "(elisp)Syntax Table Internals" "*info elisp syntax*")
  (let ((end (point))
	(syntax (syntax-class (syntax-after (1- (point))))))
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
     )
    (buffer-substring-no-properties (point) end)
    ))

;;;; token info cache
;;
;; the cache stores the results of parsing as text properties on
;; keywords, for use by the indention and motion engines.

(defstruct
  (wisi-cache
   (:constructor wisi-cache-create)
   (:copier nil))
  symbol
  ;; symbol from wisi-keyword-table, wisi-punctuation-table, or non-terminal
  ;; otherwise token text

  class
  ;; some classes are defined by wisi:
  ;; 'block-start
  ;; 'statement-start
  ;; 'open-paren
  ;;
  ;; rest are language-specific

  start
  ;; a) if this is not first keyword; mark at the first keyword of the current statement
  ;; b) if this is first keyword; function to move from this keyword to first token or nil

  prev ;; marker at previous motion token in statement; nil if none
  next ;; marker at next motion token in statement; nil if none
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
	;; FIXME: check that entire range is in comment or string
	(setq wisi-change-need-invalidate nil))

       ((progn
	  (skip-syntax-forward " " end);; does not skip newline
	  (eq (point) end))
	(setq wisi-change-need-invalidate nil))

       (t (setq wisi-change-need-invalidate t))
       ))))

(defun wisi-after-change (begin end length)
  "For `after-change-functions'."
  (when (>= wisi-cache-max begin)
    (save-excursion
      ;; FIXME: if wisi-change-need-invalidate, don't do any more checks.
      (let ((need-invalidate t)
	    ;; (info "(elisp)Parser State")
	    (state (syntax-ppss begin)))
	;; syntax-ppss has moved point to "begin".
	(cond
	 ((or
	   (nth 3 state); in string
	   (nth 4 state)); in comment
	  ;; FIXME: insert newline in comment to create non-comment!?
	  ;; or paste a chunk of code
	  ;; => check that all of change region is comment or string
	  (setq need-invalidate nil))

	 ((progn
	    (skip-syntax-forward " " end);; does not skip newlines
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
  "Return `wisi-cache' struct from the `wisi-cache' text property at POS.
If accessing cache at a marker for a token as set by `wisi-cache-tokens', POS must be (1- mark)."
  (get-text-property pos 'wisi-cache))

(defvar wisi-parse-error nil)

(defun wisi-parse-error (msg)
  "For `wisent-parse-error-function'."
  ;; just capture the error, so wisi-parse-buffer can jump to it
  (setq wisi-parse-error msg))

(defvar wisi-debug nil
  "Operate wisi in debug mode; don't handle errors, so debug-on-error works.")

(defun wisi-validate-cache (pos)
  "Ensure cached data is valid at least up to POS in current buffer."
  (when (< wisi-cache-max pos)
    (if wisi-debug
	;; let debug-on-error work
	(save-excursion
	  (goto-char wisi-cache-max)
	  (wisent-parse wisi-parse-table 'wisi-forward-token 'wisi-parse-error)
	  (setq wisi-cache-max (point)))
      ;; else handle errors nicely
      (let (err-pos)
	(condition-case err
	    (save-excursion
	      ;; FIXME: if more than one start non-terminal in buffer,
	      ;; wisent-parse will stop after the next one; need loop
	      (condition-case err
		  (progn
		    (goto-char wisi-cache-max)
		    ;; invalid syntax is silently reported via
		    ;; wisi-parse-error, and the parser aborts (FIXME:
		    ;; we don't have any error recovery in the grammar
		    ;; yet).
		    (wisent-parse wisi-parse-table 'wisi-forward-token 'wisi-parse-error))
		(error
		 ;; from broken wisi parse actions
		 (setq err-pos (point))
		 (signal (car err) (cdr err)))))
	  (error
	   ;; from inner handler
	   (let ((msg (cadr err)))
	     (if (stringp msg)
		 (progn
		   (when (string-match ".*:\\([0-9]+\\):" msg)
		     (goto-char (string-to-number (match-string 1 msg))))
		   (message msg))
	       (goto-char err-pos)
	       (message "%s" err)))))))

    (when wisi-parse-error
      (string-match "unexpected \\(\\w+\\)@\\([0-9]+\\)(\"\\(.+\\)\"),\\(.*\\)" wisi-parse-error)
      (let ((symbol (match-string 1 wisi-parse-error))
	    (begin (string-to-number (match-string 2 wisi-parse-error)))
	    (expecting (match-string 4 wisi-parse-error)))
	(setq wisi-parse-error nil)
	(if wisi-debug
	    (progn
	      (goto-char begin)
	      (error "unexpected %s: %s" symbol expecting))
	  (message "%s:%s: unexpected %s: %s"
		   (file-name-nondirectory (buffer-file-name))
		   (line-number-at-pos)
		   symbol expecting))
	))
    ))

;;;; parse actions

(defun wisi-symbol (text)
  "Return token symbol from `wisi-keyword-table' or `wisi-punctuation-table' for TEXT."
  (or
   (symbol-value (intern-soft text wisi-keyword-table))
   (car (rassoc text wisi-punctuation-table))
   ;; else identifier, string_literal, char-literal, numeric_literal, non-terminal symbol
   text))

(defun wisi-statement-tokens (&rest items)
  ;; FIXME: change items to 'token class'; token same struct as returned
  ;; by wisi-forward-token, with text = nil for nonterms. that
  ;; eliminates the need for wisi-symbol and wisi-statement-action
  ;; (rename this to that to preserve .wy)
  "Cache indentation and name information in text properties of tokens.
Intended as a wisent grammar non-terminal action.

ITEMS is a list (symbol/text class (start end)) ... for tokens that
should receive cached information. See macro
`wisi-statement-action' for using this in a wisent grammar file."
  (save-excursion
    (let ((first-item t) first-keyword-mark start)
      (while items
	(let* ((text (pop items))
	       (class (pop items))
	       (region (pop items))
	       (mark
		;; Marker one char into token, so indent-line-to
		;; inserts space before the mark, not after!
		(when region (copy-marker (1+ (car region)))))
	       cache)

	  (when region
	    (if (setq cache (get-text-property (car region) 'wisi-cache))
		;; We are processing a previously set non-terminal; ie package_specification in
		;;
		;; generic_package_declaration : generic_formal_part package_specification SEMICOLON
		;;
		;; just override class and start
		(progn
		  (setf (wisi-cache-class cache) class)
		  (setf (wisi-cache-start cache) first-keyword-mark))

	      ;; else create new cache
	      ;;
	      ;; :start is a marker to the first keyword in a
	      ;; statement. The first keyword in a contained statement
	      ;; has a marker to the first keyword in the containing
	      ;; statement; the outermost containing statement has
	      ;; nil. `wisi-start-tokens' sets the start markers in
	      ;; contained statements.
	      (with-silent-modifications
		(put-text-property
		 (car region)
		 (cdr region)
		 'wisi-cache
		 (wisi-cache-create
		  :symbol (if first-item $nterm (wisi-symbol text))
		  :class class
		  :start first-keyword-mark)
		 ))))

	  (when first-item
	    (setq first-item nil)
	    (when (memq class '(block-start statement-start))
	      (setq first-keyword-mark mark
		    start (car region))))
	))
      )))

(defmacro wisi-statement-action (&rest pairs)
  "Macro to define a wisi grammar action that is a call to `wisi-statement-tokens'.
PAIRS is of the form [TOKEN-NUMBER CLASS] ...  where TOKEN-NUMBER
is the token number in the production, CLASS is the wisi class of
that token. Use in a grammar action as:

`,(wisi-statement-action 1 'statement-start 7 'statement-end)"
  (let (class list number region)
    (while pairs
      (setq number (pop pairs))
      (setq region (intern (concat "$region" (number-to-string number))))
      (setq number (intern (concat "$" (number-to-string number))))
      (setq class (pop pairs))
      (setq list (append list (list number class region))))
    (cons 'wisi-statement-tokens list)
  ))

(defun wisi-start-tokens (start-region contained-region)
  "Set start marks in all tokens in (car START-REGION) to (cdr
CONTAINED-REGION) with null start mark to marker pointing to start of
START-REGION.  See `wisi-start-action' for use in grammar
action."
  (save-excursion
    (goto-char (cdr contained-region))
    (let ((cache (car (wisi-backward-cache)))
	  (mark (copy-marker (1+ (car start-region)))))
      (while cache

	;; skip blocks that are already marked
	(while (markerp (wisi-cache-start cache))
	  (goto-char (1- (wisi-cache-start cache)))
	  (setq cache (wisi-get-cache (point))))

	(if (<= (point) (cdr start-region))
	    ;; done (don't set mark on cache at start; that should
	    ;; point to the containing statement)
	    (setq cache nil)

	  ;; else set mark, loop
	  (setf (wisi-cache-start cache) mark)
	  (setq cache (car (wisi-backward-cache))))
	))))

(defmacro wisi-start-action (start-token contained-token)
  "Macro to define a wisi grammar action that is a call to `wisi-start-tokens'.
START-TOKEN is token number of first token in containing statement,
CONTAINED-TOKEN is token number of the contained non-terminal."
  (list 'wisi-start-tokens
	(intern (concat "$region" (number-to-string start-token)))
	(intern (concat "$region" (number-to-string contained-token)))))

(defun wisi-motion-tokens (&rest items)
  "Set prev/next marks in all ITEMS.

ITEMS is a list [(start end)] ... for tokens that should receive
cached information. See macro `wisi-motion-action' for using this
in a wisent grammar file."
  (save-excursion
    (let (next-keyword-mark
	  prev-keyword-mark
	  prev-cache
	  cache)
      (while items
	(let* ((region (pop items))
	       (mark
		;; Marker one char into token, so indent-line-to
		;; inserts space before the mark, not after!
		(copy-marker (1+ (car region)))))

	  (setq cache (wisi-get-cache (car region)))

	  (when prev-keyword-mark
	    (setf (wisi-cache-prev cache) prev-keyword-mark)
	    (setf (wisi-cache-next prev-cache) mark))

	  (setq prev-keyword-mark mark)
	  (setq prev-cache cache)
	))
      )))

(defmacro wisi-motion-action (&rest token-numbers)
  "Macro to define a wisi grammar action that is a call to `wisi-motion-tokens'.
Token caches must have been created by a previous action.
Use in a grammar action as:

  : generic_formal_part package_specification SEMICOLON
    (progn
      `(wisi-statement-action 1 'block-start 2 'statement-middle 3 'statement-end)
      `(wisi-motion-action 1 2 3))"
  (let (number region list)
    (while token-numbers
      (setq number (pop token-numbers))
      (setq region (intern (concat "$region" (number-to-string number))))
      (setq list (append list (list region))))
    (cons 'wisi-motion-tokens list)
  ))

;;;; motion
(defun wisi-backward-cache ()
  "Move point backward to the beginning of the first token preceding point that has a cache.
Returns (cache region); the found cache, and the text region
containing it; or nil if at beginning of buffer."
  (let (begin end)
    (setq end (previous-single-property-change (point) 'wisi-cache))
    (when end
      (if (get-text-property (1- end) 'wisi-cache)
	  (setq begin
		(or
		 (previous-single-property-change end 'wisi-cache)
		 ;; that fails if first cache is at bob
		 (point-min)))
	;; else single-character token
	(setq begin end))
      (goto-char begin)
      (list (get-text-property begin 'wisi-cache)
	    (list begin end)))
    ))

(defun wisi-forward-cache ()
  "Move point forward to the beginning of the first token after point that has a cache.
Assumes relevant portion of buffer is parsed.
Returns (cache region); the found cache, and the text region
containing it; or nil if at end of buffer."
  (let (begin end)
    (when (get-text-property (point) 'wisi-cache)
      ;; on a cached token; get past it
      (wisi-forward-token t))

    (setq begin (next-single-property-change (point) 'wisi-cache))
    (when begin
      (if (get-text-property (1+ begin) 'wisi-cache)
	  (setq end (next-single-property-change begin 'wisi-cache))
	;; else single-character token
	(setq end begin))
      (goto-char begin)
      (list (get-text-property begin 'wisi-cache)
	    (list begin end)))
    ))

(defun wisi-forward-statement-keyword ()
  "If not at a cached token, move forward to next
cache. Otherwise move to cache-next, or next cache if nil."
  (wisi-validate-cache (point-max))
  (let ((cache (wisi-get-cache (point))))
    (if cache
	(let ((next (wisi-cache-next cache)))
	  (if next
	      (goto-char (1- next))
	    (wisi-forward-token)
	    (wisi-forward-cache)))
      (wisi-forward-cache))
  ))

(defun wisi-goto-statement-start (cache containing)
  "Move point to statement start of statement containing CACHE, return cache at that point.
If at start of a statement and CONTAINING is non-nil, goto start
of containing statement."
  (let ((done nil))
    (while (not done)
      (cond
       ((markerp (wisi-cache-start cache))
	(cond
	 ((memq (wisi-cache-class cache) '(statement-start block-start))
	  (setq done t)
	  (when containing
	    (goto-char (1- (wisi-cache-start cache)))
	    (setq cache (wisi-get-cache (point)))))

	 (t
	  (goto-char (1- (wisi-cache-start cache)))
	  (setq cache (wisi-get-cache (point))))
	 ))
       (t
	;; at outermost containing statement
	(setq done t))))
    cache))

(defun wisi-next-statement-cache (cache)
  "Move point to CACHE-next; no motion if nil."
  (when (markerp (wisi-cache-next cache))
    (goto-char (1- (wisi-cache-next cache)))))

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
    (unless (ada-in-paren-p)
      (back-to-indentation))
    (+ (current-column) offset)))

(defun wisi-indent-paren (offset)
  "Return indentation OFFSET relative to preceding open paren."
  (save-excursion
    (let ((done nil)
	  cache)
      (while (not done)
	(setq cache (car (wisi-backward-cache)))
	(setq done
	      (or (not cache);; bob
		  (eq (wisi-cache-class cache) 'open-paren)))))
    (+ (current-column) offset)))

(defun wisi-indent-statement-start (offset cache containing)
  "Return indentation OFFSET relative to line containing start of current statement,
or containing statement if CONTAINING.
CACHE contains cache info from a keyword in the current statement."
  (save-excursion
    (cond
     ((markerp (wisi-cache-start cache))
      (wisi-goto-statement-start cache containing)
      (unless (ada-in-paren-p)
	(back-to-indentation))
      (+ (current-column) offset))
     (t
      (if (ada-in-paren-p)
	  (progn
	    (ada-goto-open-paren 1)
	    (+ (current-column) offset))

	;; at outermost containing statement; ignore offset
	0))
     )))

(defvar wisi-indent-calculate-functions nil
  "Functions to calculate indentation. Each called with point
  before a token at the beginning of a line (at current
  indentation); return indentation column for that token, or
  nil. Preserve point. Calling stops when first function returns
  non-nil.")

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

  (set (make-local-variable 'wisi-punctuation-table)
       (symbol-value (intern-soft "punctuation" token-table)))
  (set (make-local-variable 'wisi-punctuation-table-max-length) 0)
  (dolist (item wisi-punctuation-table)
    (when item ;; default matcher can be nil

      ;; check that all chars used in punctuation tokens have punctuation syntax
      (mapc (lambda (char)
	      (when (not (= ?. (char-syntax char)))
		(error "in %s, %c does not have punctuation syntax"
		       (car item) char)))
	    (cdr item))

      (when (< wisi-punctuation-table-max-length (length (cdr item)))
	(setq wisi-punctuation-table-max-length (length (cdr item))))))

  (set (make-local-variable 'wisi-keyword-table) keyword-table)
  (set (make-local-variable 'wisi-parse-table) parse-table)

  (set (make-local-variable 'wisi-indent-calculate-functions) indent-calculate)
  (set (make-local-variable 'indent-line-function) 'wisi-indent-line)

  (add-hook 'before-change-functions 'wisi-before-change nil t)
  (add-hook 'after-change-functions 'wisi-after-change nil t)

  ;; FIXME: (set (make-local-variable 'forward-sexp-function) 'wisi-forward-sexp-command)
  ;; This is nice for user navigation; do we need it for wisi?

  )

(provide 'wisi)

;;; end of file
