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
;; start of the statement it is part of, or some other relevant point
;; in the statement (called 'statement indent points'), and indent
;; relative to that.  So we need a parser that lets us find statement
;; indent points from arbitrary places in the code.
;;
;; wisent provides an LALR(1) parser, which in general wants to start
;; from the first token in a non-terminal. The grammar has
;; non-terminals for Ada statements and declarations. The beginning of
;; the buffer always works as a starting point for the parser; it must
;; be the start of a declaration (a compilation unit).
;;
;; Ada as represented by the EBNF in LRM Annex P is not LALR(1), so we
;; use a generalized LALR(1) parser (see wisi-parse, wisi-compile).
;;
;; The parser actions store indentation and other information as text
;; properties of the keywords at statement indent points.
;;
;; An indentation engine moves text in the buffer, as does user
;; editing, so we can't rely on character positions remaining
;; constant. So the parser actions use markers to store
;; positions. Text properties also move with the text.
;;
;; The stored information includes a marker at the first token of the
;; statement. Thus, the indentation algorithm is: find the nearest
;; token with cached information, and either indent from it, or fetch
;; from it the marker for the statement start, and indent relative to
;; that.
;;
;; Since we have a cache (the text properties), we need to consider
;; when to invalidate it. Ideally, we invalidate only when a change to
;; the buffer would change the result of a parse that crosses that
;; change, or starts after that change. Changes in whitespace
;; (indentation and newlines) do not affect an Ada parse. Other
;; languages are sensitive to newlines (Bash for example) or
;; indentation (Python). Adding comments does not change a parse,
;; unless code is commented out. In order to be conservative, for now
;; we invalidate the cache after the edit point if the change involves
;; anything other than whitespace.
;;
;; We always use 'ecase' in the indentation code, to ensure that the
;; symbols expected match those stored in the text properties.
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
;; Using a generalized LALR parser avoids that particular problem;
;; since the language is already defined in terms of the grammar, it
;; is only a matter of a format change to teach the wisi parser the
;; language. The problem in a wisi indentation engine is caching the
;; output of the parser in a useful way, since we can't start the parser
;; from arbitrary places in the code (as we can with the SMIE
;; parser). A second problem is determining when to invalidate the
;; cache. But these problems are independent of the language being
;; parsed, so once we have one wisi indentation engine working,
;; adapting it to new languages should be quite simple.
;;
;; The SMIE parser does not find the start of each statement, only the
;; first language keyword in each statement; additional code must be
;; written to find the statement start. The wisi parser finds the
;; statement start directly.
;;
;; In SMIE, it is best if each grammar rule is a complete statement,
;; so forward-sexp will traverse the entire statement. If nested
;; non-terminals are used, forward-sexp may stop inside one of the
;; nested non-terminals. This problem does not occur with the wisi
;; parser.
;;
;; A downside of the wisi parser is conflicts in the grammar; they can
;; be much more difficult to resolve than in the SMIE parser. The
;; generalized parser helps by handling conflicts, but it does so by
;; running multiple parsers in parallel, persuing each choice in the
;; conflict. If the conflict is due to a genuine ambiguity, both paths
;; will succeed, which causes the parse to fail, since it is not clear
;; which set of text properties to store. So grammar conflicts must
;; still be analyzed and minimized.
;;
;; In addition, the complete grammar must be specified; in smie, it is
;; often possible to specify a subset of the grammar.
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
;; However, since we are using a generalized LALR(1) parser, we cannot
;; use any of the wisent grammar functions. We use the OpenToken Ada
;; package to compile wisent BNF to Elisp source (similar to
;; semantic-grammar-create-package), and wisi-compile-grammar to
;; compile that to the parser table.
;;
;; wisent also does not provide a lexer. Semantic provides a complex
;; lexer, more complicated than we need for indentation. So we
;; use the elisp lexer, which consists of `forward-comment',
;; `skip-syntax-forward', and `scan-sexp'. We wrap that in functions
;; that return tokens in the form wisi-parse expects.
;;
;;; code style
;;
;; 'wisi' is short for "wisent indentation engine".
;;
;; not using lexical-binding or cl-lib because we support Emacs 23
;;
;;;;;

(require 'cl)
(require 'wisi-parse)

;;;; lexer

(defvar wisi-class-list nil)
(make-variable-buffer-local 'wisi-class-list)
(defvar wisi-keyword-table nil)
(make-variable-buffer-local 'wisi-keyword-table)
(defvar wisi-punctuation-table-max-length 0)
(make-variable-buffer-local 'wisi-punctuation-table-max-length)
(defvar wisi-punctuation-table nil)
(make-variable-buffer-local 'wisi-punctuation-table)
(defvar wisi-string-double-term nil) ;; string delimited by double quotes
(make-variable-buffer-local 'wisi-string-double-term)
(defvar wisi-string-single-term nil) ;; string delimited by single quotes
(make-variable-buffer-local 'wisi-string-single-term)
(defvar wisi-symbol-term nil)
(make-variable-buffer-local 'wisi-symbol-term)

(defun wisi-forward-token (&optional text-only lower)
  "Move point forward across one token, skipping leading whitespace and comments.
Return the corresponding token, in a format determined by TEXT-ONLY:
TEXT-ONLY t:          text
TEXT-ONLY nil:        (token text start . end)
where:
`token' is a token symbol (not string) from `wisi-punctuation-table',
`wisi-keyword-table', `wisi-string-double-term', `wisi-string-double-term' or `wisi-symbol-term'.

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
	  (if (or
	       (eobp)
	       (= (- (point) start) wisi-punctuation-table-max-length))
	      (setq done t)
	    (forward-char 1))
	  )
	(goto-char next-point)))

     ((memq syntax '(4 5)) ;; open, close parenthesis
      (forward-char 1)
      (setq token-text (buffer-substring-no-properties start (point)))
      (setq token-id (symbol-value (intern-soft token-text wisi-keyword-table))))

     ((eq syntax 7)
      ;; string quote, either single or double. we assume we are before the start quote, not the end quote
      (let ((delim (char-after (point)))
	    (forward-sexp-function nil))
	(forward-sexp)
	(setq token-text (buffer-substring-no-properties start (point)))
	(setq token-id (if (= delim ?\") wisi-string-double-term wisi-string-single-term))))

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
Return (nil text start . end) - same structure as
wisi-forward-token, but does not look up symbol."
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
    (cons nil (cons (buffer-substring-no-properties (point) end) (cons (point) end)))
    ))

;;;; token info cache
;;
;; the cache stores the results of parsing as text properties on
;; keywords, for use by the indention and motion engines.

(defstruct
  (wisi-cache
   (:constructor wisi-cache-create)
   (:copier nil))
  nonterm;; nonterminal from parse (set by wisi-statement-action)

  token
  ;; terminal symbol from wisi-keyword-table or
  ;; wisi-punctuation-table, or lower-level nonterminal from parse
  ;; (set by wisi-statement-action)

  last ;; pos of last char in token, relative to first (0 indexed)

  class
  ;; arbitrary lisp symbol, used for indentation and navigation.
  ;; some classes are defined by wisi:
  ;;
  ;; 'block-middle - a block keyword (if then else end), not at thestart of a statement
  ;;
  ;; 'block-start - a block keyword at the start of a statement
  ;;
  ;; 'statement-start - the start of a statement
  ;;
  ;; 'open-paren
  ;;
  ;; others are language-specific

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
(make-variable-buffer-local 'wisi-parse-table)

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
  ;; (syntax-ppss-flush-cache begin) is in before-change-functions
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

(defvar wisi-debug 0
  "wisi debug mode:
0 : normal
1 : position point at parse errors
2 : debug-on-error works in parser.")

(defun wisi-validate-cache (pos)
  "Ensure cached data is valid at least up to POS in current buffer."
  (when (< wisi-cache-max pos)
    (let (msg)
      (save-excursion
	;; FIXME: if more than one start non-terminal in buffer,
	;; wisent-parse will stop after the next one; need loop
	(goto-char wisi-cache-max)
	(if (> wisi-debug 0)
	    ;; let debugger stop in wisi-parse
	    (progn
	      (wisi-parse wisi-parse-table 'wisi-forward-token)
	      (setq wisi-cache-max (point)))
	  ;; else capture errors from bad syntax, so higher level functions can try to continue
	  (condition-case err
	      (progn
		(wisi-parse wisi-parse-table 'wisi-forward-token)
		(setq wisi-cache-max (point)))
	    (wisi-parse-error
	     (setq msg (cdr err)))
	    )))
      (when msg
	(when (and (> wisi-debug 0)
		   (string-match ":\\([0-9]+\\):\\([0-9]+\\):" msg))
	  (let ((line (string-to-number (match-string 1 msg)))
		(col (string-to-number (match-string 2 msg))))
	    (goto-char (point-min))
	    (forward-line (1- line))
	    (forward-char col)
	    (error msg)))
	(message msg)))))

;;;; parse actions

(defvar tokens nil);; keep byte-compiler happy; tokens is let-bound in wisi-parse-reduce
(defun wisi-statement-action (&rest pairs)
  "Cache information in text properties of tokens.
Intended as a grammar non-terminal action.

PAIRS is of the form [TOKEN-NUMBER CLASS] ...  where TOKEN-NUMBER
is the (1 indexed) token number in the production, CLASS is the wisi class of
that token. Use in a grammar action as:
  (wisi-statement-action 1 'statement-start 7 'statement-end)"
  (save-excursion
    (let ((first-item t)
	  first-keyword-mark
	  (override-start nil))
      (while pairs
	(let* ((number (1- (pop pairs)))
	       (region (cddr (nth number tokens)));; tokens is let-bound in wisi-parse-reduce
	       (token (car (nth number tokens)))
	       (class (pop pairs))
	       (mark
		;; Marker one char into token, so indent-line-to
		;; inserts space before the mark, not after
		(when region (copy-marker (1+ (car region)))))
	       cache)

	  (unless (memq class wisi-class-list)
	    (error "%s not in wisi-class-list" class))

	  (if region
	      (progn
		(if (setq cache (get-text-property (car region) 'wisi-cache))
		    ;; We are processing a previously set non-terminal; ie package_specification in
		    ;;
		    ;; generic_package_declaration : generic_formal_part package_specification SEMICOLON
		    ;;
		    ;; or simple_statement in
		    ;;
		    ;; statement : label_opt simple_statement
		    ;;
		    ;; just override class and start
		    (progn
		      (case (wisi-cache-class cache)
			(block-start
			 (setf (wisi-cache-class cache)
			       (cond
				 ((eq override-start nil)
				  (cond
				   ((memq class '(block-start statement-start)) 'block-start)
				   (t 'block-middle)))

				 ((memq override-start '(block-start statement-start)) 'block-start)

				 (t (error "unexpected override-start"))
				 )))
			(t
			 (setf (wisi-cache-class cache) (or override-start class)))
			)
		      (setf (wisi-cache-start cache) first-keyword-mark))

		  ;; else create new cache
		  ;;
		  ;; :start is a marker to the first keyword in a
		  ;; statement. The first keyword in a contained statement
		  ;; has a marker to the first keyword in the containing
		  ;; statement; the outermost containing statement has
		  ;; nil. `wisi-start-action' (below) sets the start
		  ;; markers in contained statements.
		  (with-silent-modifications
		    (put-text-property
		     (car region)
		     (1+ (car region))
		     'wisi-cache
		     (wisi-cache-create
		      :nonterm (when first-item $nterm);; $nterm defined in wisi-semantic-action
		      :token   token
		      :last  (- (cdr region) (car region))
		      :class   (or override-start class)
		      :start   first-keyword-mark)
		     )))

		(when first-item
		  (setq first-item nil)
		  (when (or override-start
			    (memq class '(block-start statement-start)))
		    (setq override-start nil)
		    (setq first-keyword-mark mark))))

	    ;; region is nil when a production is empty; if the first
	    ;; token is a start, override the class on the next token.
	    (when (and first-item
		       (memq class '(block-start statement-start)))
	      (setq override-start class)))
	))
      )))

(defun wisi-start-action (start-token contained-token)
  "Set start marks in all tokens in CONTAINED-TOKEN with null start mark to marker pointing to start of START-TOKEN.
START-TOKEN is token number of first token in containing statement (if empty, next token is used).
CONTAINED-TOKEN is token number of the contained non-terminal."
  (let* ((start-region (cddr (nth (1- start-token) tokens))) ;; tokens is let-bound in wisi-parse-reduce
	 (contained-region (cddr (nth (1- contained-token) tokens))))
    (unless start-region
      ;; start-token is empty; use next
      (setq start-region (cddr (nth start-token tokens))))

    (when contained-region
      ;; nil when empty production
      (save-excursion
	(goto-char (cdr contained-region))
	(let ((cache (wisi-backward-cache))
	      (mark (copy-marker (1+ (car start-region)))))
	  (while cache

	    ;; skip blocks that are already marked
	    (while (markerp (wisi-cache-start cache))
	      (goto-char (1- (wisi-cache-start cache)))
	      (setq cache (wisi-get-cache (point))))

	    (if (<= (point) (car contained-region))
		;; done
		(setq cache nil)

	      ;; else set mark, loop
	      (setf (wisi-cache-start cache) mark)
	      (setq cache (wisi-backward-cache)))
	    ))))))

(defun wisi-motion-action (&rest token-numbers)
  "Set prev/next marks in all tokens given by TOKEN-NUMBERS.
Each TOKEN-NUMBERS is one of:

number: the token number; mark that token

list (number token_id): mark all tokens with token_id in the nonterminal given by the number."
  (save-excursion
    (let (next-keyword-mark
	  prev-keyword-mark
	  prev-cache
	  cache)
      (while token-numbers
	(let ((token-number (pop token-numbers))
	      target-token
	      region)
	  (cond
	   ((numberp token-number)
	    (setq target-token nil)
	    (setq region (cddr (nth (1- token-number) tokens)))
	    (setq cache (wisi-get-cache (car region)))
	    (setq mark (copy-marker (1+ (car region))))
	    )

	   ((listp token-number)
	    ;; cannot be empty, but may not contain any token_id
	    (setq target-token (cadr token-number))
	    (setq token-number (car token-number))
	    (setq region (cddr (nth (1- token-number) tokens)))
	    (goto-char (car region))
	    (wisi-forward-find-token target-token (cdr region) t)
	    (setq cache (wisi-get-cache (point)))
	    (setq mark (copy-marker (1+ (point))))
	    )

	   (t
	    (error "unexpected token-number %s" token-number))
	   )

	  (when (and cache prev-keyword-mark)
	    (setf (wisi-cache-prev cache) prev-keyword-mark)
	    (setf (wisi-cache-next prev-cache) mark))

	  (setq prev-keyword-mark mark)
	  (setq prev-cache cache)
	  ))
      )))

;;;; motion
(defun wisi-backward-cache ()
  "Move point backward to the beginning of the first token preceding point that has a cache.
Returns cache, or nil if at beginning of buffer."
  (let (cache pos)
    (setq pos (previous-single-property-change (point) 'wisi-cache))
    ;; There are three cases:
    ;;
    ;; 1) caches separated by non-cache chars: 'if ... then'
    ;;    pos is before 'f', cache is on 'i'
    ;;
    ;; 2) caches not separated: ');'
    ;;    pos is before ';', cache is on ';'
    ;;
    ;; 3) at bob; pos is nil
    ;;
    (if pos
	(progn
	  (setq cache (get-text-property pos 'wisi-cache))
	  (if cache
	      ;; case 2
	      (goto-char pos)
	    ;; case 1
	    (setq cache (get-text-property (1- pos) 'wisi-cache))
	    (goto-char (1- pos))))
      ;; at bob
      (goto-char (point-min))
      (setq cache nil))
    cache
    ))

(defun wisi-forward-cache ()
  "Move point forward to the beginning of the first token after point that has a cache.
Returns cache, or nil if at end of buffer."
  (let (cache pos)
    (when (get-text-property (point) 'wisi-cache)
      ;; on a cache; get past it
      (goto-char (1+ (point))))

    (setq cache (get-text-property (point) 'wisi-cache))
    (if cache
	nil

      (setq pos (next-single-property-change (point) 'wisi-cache))
      (if pos
	  (progn
	    (goto-char pos)
	    (setq cache (get-text-property pos 'wisi-cache)))
	;; at eob
	(goto-char (point-max))
	(setq cache nil))
      )
    cache
    ))

(defun wisi-forward-find-class (class limit)
  "Search forward for a token that has a cache with CLASS.
Return cache, or nil if at end of buffer.
If LIMIT (a buffer position) is reached, throw an error."
  (let ((cache (wisi-forward-cache)))
    (while (not (eq class (wisi-cache-class cache)))
      (setq cache (wisi-forward-cache))
      (when (>= (point) limit)
	(error "cache with class %s not found" class)))
    cache))

(defun wisi-forward-find-token (token limit &optional noerror)
  "Search forward for a token that has a cache with TOKEN.
Return cache, or nil if at end of buffer.
If LIMIT (a buffer position) is reached, then if NOERROR is nil, throw an
error, if non-nil, return nil."
  (let ((cache (wisi-get-cache (point)))
	(done nil))
    (while (not (or done
		    (eq token (wisi-cache-token cache))))
      (setq cache (wisi-forward-cache))
      (when (>= (point) limit)
	(if noerror
	    (progn
	      (setq done t)
	      (setq cache nil))
	  (error "cache with token %s not found" token))))
    cache))

(defun wisi-forward-find-nonterm (nonterm limit)
  "Search forward for a token that has a cache with NONTERM.
Return cache, or nil if at end of buffer.
If LIMIT (a buffer position) is reached, throw an error."
  (let ((cache (wisi-forward-cache)))
    (while (not (eq nonterm (wisi-cache-nonterm cache)))
      (setq cache (wisi-forward-cache))
      (when (>= (point) limit)
	(error "cache with nonterm %s not found" nonterm)))
    cache))

(defun wisi-forward-statement-keyword ()
  "If not at a cached token, move forward to next
cache. Otherwise move to cache-next, or next cache if nil."
  (let ((cache (wisi-get-cache (point))))
    (if cache
	(let ((next (wisi-cache-next cache)))
	  (if next
	      (goto-char (1- next))
	    (wisi-forward-token)
	    (wisi-forward-cache)))
      (wisi-forward-cache))
  ))

(defun wisi-backward-statement-keyword ()
  "If not at a cached token, move backward to prev
cache. Otherwise move to cache-prev, or prev cache if nil."
  (let ((cache (wisi-get-cache (point))))
    (if cache
	(let ((prev (wisi-cache-prev cache)))
	  (if prev
	      (goto-char (1- prev))
	    (wisi-backward-cache)))
      (wisi-backward-cache))
  ))

(defun wisi-goto-statement-start (cache containing &optional error)
  "Move point to start of statement containing CACHE, return cache at that point.
If at start of a statement and CONTAINING is non-nil, goto start
of containing statement. If ERROR and at start and CONTAINING, throw error."
  (cond
   ((markerp (wisi-cache-start cache))
    (cond
     ((memq (wisi-cache-class cache) '(statement-start block-start))
      (when containing
	(goto-char (1- (wisi-cache-start cache)))
	(setq cache (wisi-get-cache (point)))))

     (t
      (goto-char (1- (wisi-cache-start cache)))
      (setq cache (wisi-get-cache (point))))
     ))
   (t
    ;; at outermost containing statement
    (when error
      (error "already at outermost containing statement"))))
  cache)

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
      ;; FIXME: use (current-indentation), don't use save-excursion
      (back-to-indentation))
    (+ (current-column) offset)))

(defun wisi-indent-paren (offset)
  "Return indentation OFFSET relative to preceding open paren."
  (save-excursion
    ;; FIXME: don't move point, so don't need save-excursion
    (ada-goto-open-paren 0)
    (+ (current-column) offset)))

(defvar wisi-indent-calculate-functions nil
  "Functions to calculate indentation. Each called with point
  before a token at the beginning of a line (at current
  indentation); return indentation column for that token, or
  nil. Preserve point. Calling stops when first function returns
  non-nil.")
(make-local-variable 'wisi-indent-calculate-functions)

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
  (syntax-propertize (point-max))
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

(defun wisi-setup (indent-calculate class-list keyword-table token-table parse-table)
  "Set up a buffer for parsing files with wisi."
  (setq wisi-class-list class-list)
  (setq wisi-string-double-term (car (symbol-value (intern-soft "string-double" token-table))))
  (setq wisi-string-single-term (car (symbol-value (intern-soft "string-single" token-table))))
  (setq wisi-symbol-term (car (symbol-value (intern-soft "symbol" token-table))))

  (setq wisi-punctuation-table (symbol-value (intern-soft "punctuation" token-table)))
  (setq wisi-punctuation-table-max-length 0)
  (let (fail)
    (dolist (item wisi-punctuation-table)
      (when item ;; default matcher can be nil

	;; check that all chars used in punctuation tokens have punctuation syntax
	(mapc (lambda (char)
		(when (not (= ?. (char-syntax char)))
		  (setq fail t)
		  (message "in %s, %c does not have punctuation syntax"
			   (car item) char)))
	      (cdr item))

	(when (< wisi-punctuation-table-max-length (length (cdr item)))
	  (setq wisi-punctuation-table-max-length (length (cdr item)))))
      )
    (when fail
      (error "aborting due to punctuation errors")))

  (setq wisi-keyword-table keyword-table)
  (setq wisi-parse-table parse-table)

  (setq wisi-indent-calculate-functions indent-calculate)
  (set (make-local-variable 'indent-line-function) 'wisi-indent-line)

  (add-hook 'before-change-functions 'wisi-before-change nil t)
  (add-hook 'after-change-functions 'wisi-after-change nil t)

  ;; WORKAROUND: sometimes the first time font-lock is run, syntax-propertize is not run properly, so we run it here
  (syntax-propertize (point-max))
  )

(provide 'wisi)

;;; end of file
