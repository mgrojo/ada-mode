;;; wisi.el --- Utilities for implementing an indentation/navigation engine using a generalized LALR parser -*- lexical-binding:t -*-
;;
;; Copyright (C) 2012 - 2017  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@stephe-leake.org>
;; Maintainer: Stephen Leake <stephen_leake@stephe-leake.org>
;; Keywords: parser
;;  indentation
;;  navigation
;; Version: 1.1.6
;; package-requires: ((cl-lib "0.4") (emacs "24.3") (seq "2.3"))
;; URL: http://www.nongnu.org/ada-mode/wisi/wisi.html
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

;;; Commentary:

;;;; History: see NEWS-wisi.text
;;
;;;; Design:
;;
;; 'wisi' was originally short for "wisent indentation engine", but
;; now is just a name. wisi was developed to support Emacs ada-mode
;; 5.0 indentation, font-lock, and navigation, which are parser based.
;;
;; The approach to indenting a given token is to parse the buffer,
;; computing a delta indent at each parse action.
;;
;; The parser actions also cache face and navigation information
;; as text properties on tokens in statements.
;;
;; The three reasons to run the parser (indent, face, navigate) occur
;; at different times (user indent, font-lock, user navigate), so only
;; the relevant parser actions are run.
;;
;; Since we have a cache (the text properties), we need to consider
;; when to invalidate it.  Ideally, we invalidate only when a change
;; to the buffer would change the result of a parse that crosses that
;; change, or starts after that change.  Changes in whitespace
;; (indentation and newlines) do not affect an Ada parse.  Other
;; languages are sensitive to newlines (Bash for example) or
;; indentation (Python).  Adding comments does not change a parse,
;; unless code is commented out.
;;
;; For font-lock and navigate, keeping track of the point after which
;; caches have been deleted is sufficent (see `wisi-cache-max').
;;
;; For indenting, we cache the indent for each line in a text property
;; on the newline char preceding the line. `wisi-indent-region' sets
;; the cache on all the lines computed (normally the whole buffer),
;; but performs the indent only on the lines in the indent
;; region. Subsequent calls to `wisi-indent-region' apply the cached
;; indents. Non-whitespace edits to the buffer invalidate the indent
;; caches in the edited region and after.
;;
;; See `wisi--post-change' for the details of what we check for
;; invalidating.
;;
;;;; Choice of grammar compiler and parser
;;
;; There are two other parsing engines available in Emacs:
;;
;; - SMIE
;;
;;   We don't use this because it is designed to parse small snippets
;;   of code. For Ada indentation, we always need to parse the entire
;;   buffer.
;;
;; - semantic
;;
;;   The Ada grammar as given in the Ada language reference manual is
;;   not LALR(1). So we use a generalized parser. In addition, the
;;   semantic lexer is more complex, and gives different information
;;   than we need.
;;
;; We use wisitoken wisi-generate to compile BNF to Elisp source, and
;; wisi-compile-grammar to compile that to the parser table. See
;; ada-mode info for more information on the developer tools used for
;; ada-mode and wisi.
;;
;; Alternately, to gain speed and error handling, we use wisi-generate
;; to generate Ada source, and run that in an external process. That
;; supports error correction while parsing.
;;
;;;; syntax-propertize
;;
;; `wisi-forward-token' relies on syntax properties, so
;; `syntax-propertize' must be called on the text to be lexed before
;; wisi-forward-token is called.
;;
;; Emacs >= 25 calls syntax-propertize transparently in the low-level
;; lexer functions.
;;
;; In Emacs < 25, we call syntax-propertize in wisi-setup, and in
;; `wisi--post-change'.
;;
;;;;;

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'seq)
(require 'semantic/lex)
(require 'wisi-parse-common)
(require 'wisi-elisp-parse) ;; FIXME: don’t require these til needed
(require 'wisi-process-parse)

;; WORKAROUND: for some reason, this condition doesn't work in batch mode!
;; (when (and (= emacs-major-version 24)
;; 	   (= emacs-minor-version 2))
  (require 'wisi-compat-24.2)
;;)

(defcustom wisi-size-threshold 100000
  "Max size (in characters) for using wisi parser results for syntax highlighting and file navigation."
  :type 'integer
  :group 'wisi
  :safe 'integerp)
(make-variable-buffer-local 'wisi-size-threshold)

(defvar wisi-indent-comment-col-0 nil
  "If non-nil, comments currently starting in column 0 are left in column 0.
Otherwise, they are indented with previous comments or code.
Normally set from a language-specific option.")
(make-variable-buffer-local 'wisi-indent-comment-col-0)

(defvar wisi-indent-hanging-function nil
  "Language-specific implementation of `wisi-hanging', `wisi-hanging%'.
A function taking four args TOK DELTA1 DELTA2 OPTION,
and returning a list either (DELTA1 DELTA2) or (DELTA).
TOK is a `wisi-tok' struct for the token being indented.
DELTA1, DELTA2 are the indents of the first and following lines
within the nonterminal.  OPTION is non-nil if action is `wisi-hanging%'.
point is at start of TOK, and may be moved.")
(make-variable-buffer-local 'wisi-indent-comment-col-0)

(defvar wisi-inhibit-parse nil
  "When non-nil, don't run the parser.
Language code can set this non-nil when syntax is known to be
invalid temporarily, or when making lots of changes.")

(defcustom wisi-disable-face nil
  "When non-nil, `wisi-setup' does not enable use of parser for font-lock.
Useful when debugging parser or parser actions."
  :type 'boolean
  :group 'wisi
  :safe 'booleanp)

(defconst wisi-error-buffer-name "*wisi syntax errors*"
  "Name of buffer for displaying syntax errors.")

(defvar wisi-error-buffer nil
  "Buffer for displaying syntax errors.")

;;;; elisp lexer

(cl-defstruct wisi-lex
  id-alist ;; alist mapping strings to token ids
  keyword-table ;; obarray holding keyword tokens
  punctuation-table ;; obarray holding punctuation tokens
  punctuation-table-max-length ;; max string length in punctuation-table
  string-double-term ;; non-nil if strings delimited by double quotes
  string-quote-escape-doubled ;; Non-nil if a string delimiter is escaped by doubling it
  string-quote-escape
  ;; Cons (delim . character) where `character' escapes quotes in strings delimited by `delim'.
  string-single-term ;; non-nil if strings delimited by single quotes
  symbol-term ;; symbol for a terminal symbol token
  number-term ;; symbol for a terminal number literal token
  number-p ;; function that determines if argument is a number literal
  )

(defvar-local wisi--lexer nil
  "A `wisi-lex' struct defining the lexer for the current buffer.")

(defun wisi-safe-intern (name obtable)
  (let ((var (intern-soft name obtable)))
    (and (boundp var) (symbol-value var))))

(cl-defun wisi-make-elisp-lexer (&key token-table-raw keyword-table-raw string-quote-escape-doubled string-quote-escape)
  "Return a ‘wisi-lex’ object."
  (let* ((token-table (semantic-lex-make-type-table token-table-raw))
	 (keyword-table (semantic-lex-make-keyword-table keyword-table-raw))
	 (punctuation-table (wisi-safe-intern "punctuation" token-table))
	 (punct-max-length 0)
	 (number (cadr (wisi-safe-intern "number" token-table)))
	 (symbol (cadr (wisi-safe-intern "symbol" token-table)))
	 (string-double (cadr (wisi-safe-intern "string-double" token-table)))
	 (string-single (cadr (wisi-safe-intern "string-single" token-table)))
	 id-alist
	 fail)
    (dolist (item punctuation-table)
      ;; check that all chars used in punctuation tokens have punctuation syntax
      (mapc (lambda (char)
	      (when (not (= ?. (char-syntax char)))
		(setq fail t)
		(message "in %s, %c does not have punctuation syntax"
			 (car item) char)))
	    (cdr item))

      ;; accumulate max length
      (when (< punct-max-length (length (cdr item)))
	(setq punct-max-length (length (cdr item))))

      ;; build id-alist
      (push item id-alist)
      )

    (when fail
      (error "aborting due to punctuation errors"))

    (when number
      (push (cons (nth 0 number) "1234") id-alist)
      (when (nth 2 number)
	(require (nth 2 number)))) ;; for number-p

    ;; build rest of id-alist
    (dolist (item keyword-table-raw)
      (push (cons (cdr item) (car item)) id-alist))

    (when symbol
      (push (cons (car symbol) "a_bogus_identifier") id-alist))

    (when string-double
      (push (cons (car string-double) "\"\"") id-alist))

    (when string-single
      (push (cons (car string-single) "''") id-alist))

    (make-wisi-lex
     :id-alist id-alist
     :keyword-table keyword-table
     :punctuation-table punctuation-table
     :punctuation-table-max-length punct-max-length
     :string-double-term (car string-double)
     :string-quote-escape-doubled string-quote-escape-doubled
     :string-quote-escape string-quote-escape
     :string-single-term (car string-single)
     :symbol-term (car symbol)
     :number-term (nth 0 number)
     :number-p (nth 1 number)
     )
    ))

(cl-defstruct wisi-ind
  ;; data used while running parser for indent.
  indent
  ;; vector of indentation for all lines in buffer
  ;; each element can be one of:
  ;; - integer : indent
  ;;
  ;; - list ('anchor (start-id ...) indent)  :
  ;; indent for current line, base indent for following 'anchored
  ;; lines. Start-id is list of ids anchored at this line. For parens
  ;; and other uses.
  ;;
  ;; - list ('anchored id delta) :
  ;; indent = delta + 'anchor id line indent; for lines indented
  ;; relative to anchor.
  ;;
  ;; - list ('anchor (start-id ...) ('anchored id delta))
  ;; for nested anchors

  line-begin ;; vector of beginning-of-line positions in buffer
  last-line ;; index into line-begin of line containing last lexed token
  )

(defvar wisi--indent
  ;; not buffer-local; only let-bound in wisi-indent-region
  "A `wisi-ind' struct holding current indent information.")

;; struct wisi-tok defined in wisi-parse.el

(defun wisi-number-p (token-text)
  "Return t if TOKEN-TEXT plus text after point matches the
syntax for a real literal; otherwise nil.  Point is after
TOKEN-TEXT; move point to just past token."
  ;; Typical literals:
  ;; 1234
  ;; 1234.5678
  ;; _not_ including non-decimal base, or underscores (see ada-wisi-number-p)
  ;;
  ;; Starts with a simple integer
  (when (string-match "^[0-9]+$" token-text)
    (when (looking-at "\\.[0-9]+")
      ;; real number
      (goto-char (match-end 0))
      (when (looking-at  "[Ee][+-][0-9]+")
        ;; exponent
        (goto-char (match-end 0))))

    t
    ))

(defun wisi-forward-token ()
  "Move point forward across one token, then skip whitespace and comments.
Return the corresponding token as a `wisi-tok'.
If at whitespace or comment, throw an error.
If at end of buffer, return `wisi-eoi-term'."
  (let ((start (point))
	;; (info "(elisp)Syntax Table Internals" "*info elisp syntax*")
	end
	(syntax (syntax-class (syntax-after (point))))
	(first nil)
	(comment-line nil)
	(comment-end nil)
	token-id token-text line)
    (cond
     ((eobp)
      (setq token-id wisi-eoi-term))

     ((eq syntax 1)
      ;; punctuation. Find the longest matching string in wisi-punctuation-table
      (forward-char 1)
      (let ((next-point (point))
	    temp-text temp-id done)
	(while (not done)
	  (setq temp-text (buffer-substring-no-properties start (point)))
	  (setq temp-id (car (rassoc temp-text (wisi-lex-punctuation-table wisi--lexer))))
	  (when temp-id
	    (setq token-id temp-id
		  next-point (point)))
	  (if (or
	       (eobp)
	       (= (- (point) start) (wisi-lex-punctuation-table-max-length wisi--lexer)))
	      (setq done t)
	    (forward-char 1))
	  )
	(goto-char next-point)))

     ((memq syntax '(4 5)) ;; open, close parenthesis
      (forward-char 1)
      (setq token-text (buffer-substring-no-properties start (point)))
      (setq token-id (symbol-value (intern-soft token-text (wisi-lex-keyword-table wisi--lexer)))))

     ((eq syntax 7)
      ;; string quote, either single or double. we assume point is
      ;; before the start quote, not the end quote
      (let ((delim (char-after (point)))
	    (forward-sexp-function nil))
	(condition-case err
	    (progn
	      (forward-sexp)

	      ;; point is now after the end quote; check for an escaped quote
	      (while (or
		      (and (wisi-lex-string-quote-escape-doubled wisi--lexer)
			   (eq (char-after (point)) delim))
		      (and (eq delim (car (wisi-lex-string-quote-escape wisi--lexer)))
			   (eq (char-before (1- (point))) (cdr (wisi-lex-string-quote-escape wisi--lexer)))))
		(forward-sexp))
	      (setq token-id (if (= delim ?\")
				 (wisi-lex-string-double-term wisi--lexer)
			       (wisi-lex-string-single-term wisi--lexer))))
	  (scan-error
	   ;; Something screwed up; we should not get here if
	   ;; syntax-propertize works properly.
	   (signal 'wisi-parse-error (format "wisi-forward-token: forward-sexp failed %s" err))
	   ))))

     ((memq syntax '(2 3 6)) ;; word, symbol expression prefix (includes numbers)
      (skip-syntax-forward "w_'")
      (setq token-text (buffer-substring-no-properties start (point)))
      (setq token-id
	    (or (symbol-value (intern-soft (downcase token-text) (wisi-lex-keyword-table wisi--lexer)))
		(and (functionp (wisi-lex-number-p wisi--lexer))
		     (funcall (wisi-lex-number-p wisi--lexer) token-text)
		     (setq token-text (buffer-substring-no-properties start (point)))
		     (wisi-lex-number-term wisi--lexer))
		(wisi-lex-symbol-term wisi--lexer)))
      )

     (t
      (signal 'wisi-parse-error (format "wisi-forward-token: unsupported syntax %s" syntax)))

     );; cond

    (unless token-id
      (signal 'wisi-parse-error
	      (wisi-error-msg "unrecognized token '%s'" (buffer-substring-no-properties start (point)))))

    (setq end (point))

    (forward-comment (point-max))

    (when (and (not (eq token-id wisi-eoi-term))
	       (eq wisi--parse-action 'indent))
      ;; parsing for indent; track line numbers

      (if (wisi-ind-last-line wisi--indent)
	  (progn
	    (setq line (wisi-ind-last-line wisi--indent))
	    (when (>= start (aref (wisi-ind-line-begin wisi--indent) line))
	      ;; first token on next non-blank line
	      (setq line (1+ line))
	      (setq first t))
	    ;; else other token on line
	    )

	;; First token on first non-comment line
	(setq line (line-number-at-pos start))
	(setq first t)
	)
      (setf (wisi-ind-last-line wisi--indent) line)

      ;; set comment-line, comment-end
      (when (and (< (1+ (wisi-ind-last-line wisi--indent)) (length (wisi-ind-line-begin wisi--indent)))
		 (>= (point) (aref (wisi-ind-line-begin wisi--indent)
				 (1+ (wisi-ind-last-line wisi--indent)))))
	(setq comment-line (1+ (wisi-ind-last-line wisi--indent)))
	(setf (wisi-ind-last-line wisi--indent) comment-line)
	(setq comment-end (line-end-position 0)))

      ;; count blank or comment lines following token
      (when comment-end
	(while (and (< (1+ (wisi-ind-last-line wisi--indent)) (length (wisi-ind-line-begin wisi--indent)))
		    (>= comment-end (aref (wisi-ind-line-begin wisi--indent) (wisi-ind-last-line wisi--indent))))
	  (setf (wisi-ind-last-line wisi--indent) (1+ (wisi-ind-last-line wisi--indent))))

      ))

    (make-wisi-tok
     :token token-id
     :region (cons start end)
     :line line
     :first first
     :comment-end comment-end
     :comment-line comment-line)
    ))

(defun wisi-backward-token ()
  "Move point backward across one token, skipping whitespace and comments.
Does _not_ handle numbers with wisi-number-p; just sees
lower-level syntax.  Return a `wisi-tok' - same structure as
wisi-forward-token, but only sets token-id and region."
  (forward-comment (- (point)))
  ;; skips leading whitespace, comment, trailing whitespace.

  ;; (info "(elisp)Syntax Table Internals" "*info elisp syntax*")
  (let ((end (point))
	(syntax (syntax-class (syntax-after (1- (point)))))
	token-id token-text)
    (cond
     ((bobp) nil)

     ((eq syntax 1)
      ;; punctuation. Find the longest matching string in wisi-lex-punctuation-table
      (backward-char 1)
      (let ((next-point (point))
	    temp-text temp-id done)
	(while (not done)
	  (setq temp-text (buffer-substring-no-properties (point) end))
	  (when (setq temp-id (car (rassoc temp-text (wisi-lex-punctuation-table wisi--lexer))))
	    (setq token-id temp-id)
	    (setq next-point (point)))
	  (if (or
	       (bobp)
	       (= (- end (point)) (wisi-lex-punctuation-table-max-length wisi--lexer)))
	      (setq done t)
	    (backward-char 1))
	  )
	(goto-char next-point))
      )

     ((memq syntax '(4 5)) ;; open, close parenthesis
      (backward-char 1)
      (setq token-id
	    (symbol-value
	     (intern-soft (buffer-substring-no-properties (point) end)
			  (wisi-lex-keyword-table wisi--lexer)))))

     ((eq syntax 7)
      ;; a string quote. we assume we are after the end quote, not the start quote
      (let ((delim (char-after (1- (point))))
	    (forward-sexp-function nil))
	(forward-sexp -1)
	(setq token-id (if (= delim ?\")
			   (wisi-lex-string-double-term wisi--lexer)
			 (wisi-lex-string-single-term wisi--lexer)))
	))

     (t ;; assuming word or symbol syntax
      (if (zerop (skip-syntax-backward "."))
	  (skip-syntax-backward "w_'"))
      (setq token-text (buffer-substring-no-properties (point) end))
      (setq token-id
	    (or (symbol-value (intern-soft (downcase token-text) (wisi-lex-keyword-table wisi--lexer)))
		(and (functionp (wisi-lex-number-p wisi--lexer))
		     (funcall (wisi-lex-number-p wisi--lexer) token-text)
		     (setq token-text (buffer-substring-no-properties (point) end))
		     (wisi-lex-number-term wisi--lexer))
		(wisi-lex-symbol-term wisi--lexer))))
     )

    (make-wisi-tok
     :token token-id
     :region (cons (point) end))
    ))

;;;; token info cache
;;
;; the cache stores the results of parsing as text properties on
;; keywords, for use by the indention, face, and motion engines.

(cl-defstruct
  (wisi-cache
   (:constructor wisi-cache-create)
   (:copier nil))
  nonterm;; nonterminal from parse (set by wisi-statement-action)

  token
  ;; terminal symbol from wisi-keyword-table or
  ;; wisi-punctuation-table, or lower-level nonterminal from parse
  ;; (set by wisi-statement-action)

  last ;; pos of last char in token, relative to first (0 indexed)

  class ;; one of wisi-class-list

  containing
  ;; Marker at the start of the containing statement for this token.
  ;; nil only for first token in buffer

  prev ;; marker at previous motion token in statement; nil if none
  next ;; marker at next motion token in statement; nil if none
  end  ;; marker at token at end of current statement
  )

;; FIXME: move all cache ops here

(defvar-local wisi-parse-failed nil
  "Non-nil when a recent parse has failed - cleared when parse succeeds.")

(defvar-local wisi--parse-try
  (list
   (cons 'face t)
   (cons 'navigate t)
   (cons 'indent t))
  "Non-nil when parse is needed - cleared when parse succeeds.")

(defun wisi-parse-try (&optional parse-action)
  (cdr (assoc (or parse-action wisi--parse-action) wisi--parse-try)))

(defun wisi-set-parse-try (value &optional parse-action)
  (setcdr (assoc (or parse-action wisi--parse-action) wisi--parse-try) value))

(defvar-local wisi--cache-max
  (list
   (cons 'face nil)
   (cons 'navigate nil)
   (cons 'indent nil))
  "Alist of maximimum position in buffer where parser text properties are valid.")

(defun wisi-cache-max (&optional parse-action)
  ;; Don't need 'wisi-set-cache-max; (move-marker (wisi-cache-max) foo) works
  (cdr (assoc (or parse-action wisi--parse-action) wisi--cache-max)))

(defvar-local wisi-end-caches nil
  "List of buffer positions of caches in current statement that need wisi-cache-end set.")

(defun wisi--delete-face-cache (after)
  (with-silent-modifications
    (remove-text-properties after (point-max) '(wisi-face nil 'font-lock-face nil))
    ))

(defun wisi--delete-navigate-cache (after)
  (with-silent-modifications
    ;; This text property is 'wisi-cache', not 'wisi-navigate', for
    ;; historical reasons.
    (remove-text-properties after (point-max) '(wisi-cache nil))
    ))

(defun wisi--delete-indent-cache (after)
  (with-silent-modifications
    (remove-text-properties after (point-max) '(wisi-indent nil))
    ))

(defun wisi-invalidate-cache (action after)
  "Invalidate ACTION caches for the current buffer from AFTER to end of buffer."
  (when (< after (wisi-cache-max action))
    (when (> wisi-debug 0) (message "wisi-invalidate-cache %s:%s:%d" action (current-buffer) after))
    (cond
     ((eq 'face action)
      (wisi--delete-face-cache after))

     ((eq 'navigate action)
      ;; We goto statement start to ensure that motion within nested
      ;; structures is properly done (ie prev/next on ’elsif’ is not
      ;; set by wisi-motion-action if already set by a lower level
      ;; statement). We don’t do it for ’face or ’indent, because that
      ;; might require a parse, and they don’t care about nested
      ;; structures.
      (save-excursion
	(goto-char after)

	;; This is copied from ‘wisi-goto-statement-start’; we can’t
	;; call that because it would call ‘wisi-validate-cache’,
	;; which would call ‘wisi-invalidate-cache’; infinite loop.
	;; If this needed a navigate parse to succeed, we would not
	;; get here.
	(let ((cache (or (wisi-get-cache (point))
			 (wisi-backward-cache))))
	  (cond
	   ((null cache)
	    ;; at bob
	    nil)

	   ((eq 'statement-end (wisi-cache-class cache))
	    ;; If the change did affect part of a structure statement,
	    ;; this is a lower level statement. Otherwise, we are
	    ;; invalidating more than necessary; not a problem.
	    (wisi-goto-start cache)
	    (setq cache (wisi-backward-cache))
	    (when cache ;; else bob
	      (wisi-goto-start cache)))

	   (t
	    (wisi-goto-start cache))
	   ))

	(setq after (point)))
      (wisi--delete-navigate-cache after))

     ((eq 'indent action)
      ;; indent cache is stored on newline before line being indented.
      (setq after
	    (save-excursion
	      (goto-char after)
	      (line-beginning-position)))
      (wisi--delete-indent-cache (max 1 (1- after))))
     )
    (move-marker (wisi-cache-max action) after)
    ))

;; wisi--change-* keep track of buffer modifications.
;; If wisi--change-end comes before wisi--change-beg, it means there were
;; no modifications.
(defvar-local wisi--change-beg most-positive-fixnum
  "First position where a change may have taken place.")

(defvar-local wisi--change-end nil
  "Marker pointing to the last position where a change may have taken place.")

(defvar-local wisi--deleted-syntax nil
  "Worst syntax class of characters deleted in changes.
One of:
nil - no deletions since reset
0   - only whitespace or comment deleted
2   - some other syntax deleted

Set by `wisi-before-change', used and reset by `wisi--post-change'.")

(defvar-local wisi-indenting nil
  "Non-nil when `wisi-indent-region' is actively indenting.
Used to ignore whitespace changes in before/after change hooks.")

;; To see the effect of wisi-before-change, you need:
;; (global-font-lock-mode 0)
;; (setq jit-lock-functions nil)
;;
;; otherwise jit-lock runs and overrides it

(defun wisi-before-change (begin end)
  "For `before-change-functions'."
  ;; begin . (1- end) is range of text being deleted
  (unless wisi-indenting
    ;; We set wisi--change-beg, -end even if only inserting, so we
    ;; don't have to do it again in wisi-after-change.
    (setq wisi--change-beg (min wisi--change-beg begin))
    (when (> end wisi--change-end)
      ;; `buffer-base-buffer' deals with edits in indirect buffers
      ;; created by ediff-regions-*
      (move-marker wisi--change-end end (buffer-base-buffer)))

    (unless (= begin end)
      (cond
       ((or (null wisi--deleted-syntax)
	    (= 0 wisi--deleted-syntax))
	(save-excursion
	  (if (or (nth 4 (syntax-ppss begin)) ; in comment, moves point to begin
		  (= end (skip-syntax-forward " " end)));; whitespace
	      (setq wisi--deleted-syntax 0)
	    (setq wisi--deleted-syntax 2))))

       (t
	;; wisi--deleted-syntax is 2; no change.
	)
       ))))

(defun wisi-after-change (begin end _length)
  "For `after-change-functions'"
  ;; begin . end is range of text being inserted (empty if equal);
  ;; length is the size of the deleted text.
  ;;
  ;; This change might be changing to/from a keyword; trigger
  ;; font-lock. See test/ada_mode-interactive_common.adb Obj_1.
  (unless wisi-indenting
    (save-excursion
      (let (word-end)
	(goto-char end)
	(skip-syntax-forward "w_")
	(setq word-end (point))
	(goto-char begin)
	(skip-syntax-backward "w_")
	(with-silent-modifications
	  (remove-text-properties (point) word-end '(font-lock-face nil fontified nil)))
	)
      )))

(defun wisi--post-change (begin end)
  "Update wisi text properties for changes in region BEG END."
  ;; (syntax-ppss-flush-cache begin) is in before-change-functions

  ;; see comments above on syntax-propertize
  (when (< emacs-major-version 25) (syntax-propertize end))

  ;; Remove caches on inserted text, which could have caches from
  ;; before the failed parse (or another buffer), and are in any case
  ;; invalid. No point in removing 'fontified; that's handled by
  ;; jit-lock.

  (with-silent-modifications
    (remove-text-properties begin end '(wisi-cache nil font-lock-face nil)))

  (save-excursion
    (let ((need-invalidate t)
	  (done nil)
	  ;; non-nil if require a parse because the syntax may have
	  ;; changed.

	  (begin-state (syntax-ppss begin))
	  (end-state (syntax-ppss end)))
	  ;; (info "(elisp)Parser State")
	  ;; syntax-ppss has moved point to "end"; might be eob.

      ;; consider deletion
      (cond
       ((null wisi--deleted-syntax)
	;; no deletions
	)

       ((= 0 wisi--deleted-syntax)
	;; Only deleted whitespace; may have joined two words
	(when
	    (and (= begin end) ;; no insertions
		 (or
		  (= (point-min) begin)
		  (= 0 (syntax-class (syntax-after (1- begin))))
		  (= (point-max) end)
		  (= 0 (syntax-class (syntax-after end)))))
	  ;; More whitespace on at least one side of deletion; did not
	  ;; join two words.
	  (setq need-invalidate nil)
	  (setq done t)
	  ))

       (t
	;; wisi--deleted-syntax is 2; need invalidate and parse for all
	;; parse actions
	(setq done t)
	))

      (setq wisi--deleted-syntax nil)

      (unless done
	;; consider insertion
	(cond
	 ((= begin end)
	  ;; no insertions
	  nil)

	 ((and
	   (nth 3 begin-state);; in string
	   (nth 3 end-state)
	   (= (nth 8 begin-state) (nth 8 end-state)));; no intervening non-string
	  (setq need-invalidate nil))

	 ((and
	   (nth 4 begin-state) ; in comment
	   (nth 4 end-state)
	   (= (nth 8 begin-state) (nth 8 end-state))) ;; no intervening non-comment
	  (setq need-invalidate nil))

	 ((and
	   (or
	    (= (point-min) begin)
	    (= 0 (syntax-class (syntax-after (1- begin)))); whitespace
	    (= (point-max) end)
	    (= 0 (syntax-class (syntax-after end))))
	   (progn
	     (goto-char begin)
	     (= (- end begin) (skip-syntax-forward " " end))
	     ))
	  ;; Inserted only whitespace, there is more whitespace on at
	  ;; least one side, and we are not in a comment or string
	  ;; (checked above).  This may affect indentation, but not
	  ;; the indentation cache.
	  (setq need-invalidate nil))
	 ))

      (when need-invalidate
	(wisi-set-parse-try t 'face)
	(wisi-set-parse-try t 'navigate)
	(wisi-set-parse-try t 'indent)

	(wisi-invalidate-cache 'face begin)
	(wisi-invalidate-cache 'navigate begin)
	(wisi-invalidate-cache 'indent begin))
      )))

(defun wisi-get-cache (pos)
  "Return `wisi-cache' struct from the `wisi-cache' text property at POS."
  (get-text-property pos 'wisi-cache))

(defun wisi-goto-error ()
  "Move point to position in last error message (if any)."
  ;; FIXME: next-error goto next etc
  (when (wisi-parser-errors wisi--parser)
    (let ((data (car (wisi-parser-errors wisi--parser))))
      (cond
       ((wisi--error-pos data)
	(push-mark)
	(goto-char (wisi--error-pos data)))

       ((string-match ":\\([0-9]+\\):\\([0-9]+\\):" (wisi--error-message data))
	(let* ((msg (wisi--error-message data))
	       (line (string-to-number (match-string 1 msg)))
	       (col (string-to-number (match-string 2 msg))))
	  (push-mark)
	  (goto-char (point-min))
	  ;; FIXME: lexer can be confused about lines? line > last line in buffer.
	  (condition-case nil
	      (progn
		(forward-line (1- line))
		(forward-char col))
	    (error
	     ;; just stay at eob.
	     nil))))
       ))))

(defun wisi-show-parse-error ()
  "Show current wisi-parse errors."
  (interactive)
  (cond
   ((wisi-parser-errors wisi--parser)
    (if (and (= 1 (length (wisi-parser-errors wisi--parser)))
	     (not
	      (or (wisi--error-popped (car (wisi-parser-errors wisi--parser)))
		  (wisi--error-inserted (car (wisi-parser-errors wisi--parser)))
		  (wisi--error-deleted (car (wisi-parser-errors wisi--parser))))))
	;; If there is error correction information, use a
	;; ’compilation’ buffer, so *-fix-compiler-error will call
	;; wisi-repair-error.
	(progn
	  (wisi-goto-error)
	  (message (wisi--error-message (car (wisi-parser-errors wisi--parser)))))

      ;; else show all errors in a ’compilation’ buffer
      (setq wisi-error-buffer (get-buffer-create wisi-error-buffer-name))

      (let ((errs (nreverse (cl-copy-seq (wisi-parser-errors wisi--parser)))))
	(with-current-buffer wisi-error-buffer
	  (compilation-mode)
	  (setq next-error-last-buffer (current-buffer))
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  ;; compilation-nex-error-function assumes there is not an
	  ;; error at point min, so we need a comment.
	  (insert "wisi syntax errors")
	  (newline)
	  (dolist (err errs)
	    (insert (wisi--error-message err))
	    (put-text-property (line-beginning-position) (1+ (line-beginning-position)) 'wisi-error-data err)
	    (newline)
	    (newline))
	  (compilation--flush-parse (point-min) (point-max))
	  (compilation--ensure-parse (point-max))
	  (setq buffer-read-only t)
	  (goto-char (point-min)))

	(display-buffer wisi-error-buffer
			(cons #'display-buffer-at-bottom
			      (list (cons 'window-height #'shrink-window-if-larger-than-buffer))))
	(next-error))
      ))

   ((wisi-parse-try wisi--last-parse-action)
    (message "need parse"))

   (t
    (message "parse succeeded"))
   ))

(defconst wisi-class-list
  [motion ;; motion-action
   name ;; for which-function
   statement-end
   statement-override
   statement-start
   misc ;; other stuff
   ]
  "array of valid token classes; checked in wisi-statement-action, used in wisi-process-parse.")

(defvar-local wisi--parser nil
  "Choice of wisi parser implementation; a ‘wisi-parser’ object.")

(defun wisi-kill-parser ()
  (interactive)
  (wisi-parse-kill wisi--parser)
  ;; also force re-parse
  (dolist (parse-action '(face navigate indent))
    (wisi-set-parse-try t parse-action)
    (move-marker (wisi-cache-max parse-action) (point-max));; force delete caches
    (wisi-invalidate-cache parse-action (point-min)))
  )

(defun wisi--run-parse ()
  "Run the parser."
  (unless (buffer-narrowed-p)
    (let ((msg (when (> wisi-debug 0)
		 (format "wisi: parsing %s %s:%d ..."
			 wisi--parse-action
			 (buffer-name)
			 (line-number-at-pos (point))))))
      (when (> wisi-debug 0)
	(message msg))

      (unless (eq wisi--parse-action 'face)
	(when (buffer-live-p wisi-error-buffer)
	  (with-current-buffer wisi-error-buffer
	    (setq buffer-read-only nil)
	    (erase-buffer)
	    (setq buffer-read-only t))))

      (condition-case-unless-debug err
	  (save-excursion
	    (wisi-parse-current wisi--parser)
	    (setq wisi-parse-failed nil)
	    (when (memq wisi--parse-action '(face navigate))
	      ;; indent parse does not set caches; they are set in `wisi-indent-region'
	      (move-marker (wisi-cache-max) (point)))
	    )
	(wisi-parse-error
	 (cl-ecase wisi--parse-action
	   (face
	    ;; caches set by failed parse are ok
	    (wisi--delete-face-cache (wisi-cache-max)))

	   (navigate
	    ;; parse partially resets caches before and after wisi-cache-max
	    (move-marker (wisi-cache-max) (point-min))
	    (wisi--delete-navigate-cache (point-min)))

	   (indent
	    ;; parse does not set caches; see `wisi-indent-region'
	    nil))
	 (setq wisi-parse-failed t)
	 ;; parser should have stored this error message in parser-error-msgs
	 )
	(error
	 ;; parser failed for other reason
	 (error (cadr err))))

      (when (> wisi-debug 0)
	(if (wisi-parser-errors wisi--parser)
	    ;; error
	    (progn
	      (message "%s error" msg)
	      (wisi-goto-error)
	      (error (wisi--error-message (car (wisi-parser-errors wisi--parser)))))

	  ;; no error
	  (message "%s done" msg))
	))))

(defvar-local wisi--last-parse-action nil
  "Last value of `wisi--parse-action' when `wisi-validate-cache' was run.")

(defun wisi--check-change ()
  "Process `wisi--change-beg', `wisi--change-end'.
`wisi--parse-action' must be bound."
  (when (<= wisi--change-beg wisi--change-end)
    (wisi--post-change wisi--change-beg (marker-position wisi--change-end))
    (setq wisi--change-beg most-positive-fixnum)
    (move-marker wisi--change-end (point-min))
    ))

(defun wisi-validate-cache (pos error-on-fail parse-action)
  "Ensure cached data for PARSE-ACTION is valid at least up to POS in current buffer."
  (let ((wisi--parse-action parse-action))
    (wisi--check-change)

    ;; Now we can rely on wisi-cache-max.

    ;; If wisi-cache-max = pos, then there is no cache at pos; need parse
    (when (and (not wisi-inhibit-parse)
	       (wisi-parse-try)
	       (<= (wisi-cache-max) pos))

      ;; Don't keep retrying failed parse until text changes again.
      (wisi-set-parse-try nil)
      (setq wisi--last-parse-action wisi--parse-action)

      (setq wisi-end-caches nil);; only used by navigate

      (wisi--run-parse)

      )

    ;; We want this error even if we did not try to parse; it means
    ;; the parse results are not valid.
    (when (and error-on-fail wisi-parse-failed)
      (error "parse %s failed" parse-action))
    ))

(defun wisi-fontify-region (_begin end)
  "For `jit-lock-functions'."
  (when (< (point-max) wisi-size-threshold)
    (wisi-validate-cache end nil 'face)))

(defun wisi-get-containing-cache (cache)
  "Return cache from (wisi-cache-containing CACHE)."
  (when cache
    (let ((containing (wisi-cache-containing cache)))
      (and containing
	   (wisi-get-cache containing)))))

(defun wisi-cache-region (cache &optional start)
  "Return region designated by START (default point) to cache last."
  (unless start (setq start (point)))
  (cons start (+ start (wisi-cache-last cache))))

(defun wisi-cache-text (cache)
  "Return property-less buffer substring designated by cache.
Point must be at cache."
  (buffer-substring-no-properties (point) (+ (point) (wisi-cache-last cache))))

;;;; parse actions

(defun wisi--set-end (start-mark end-mark)
  "Set END-MARK on all caches in `wisi-end-caches' in range START-MARK END-MARK,
delete from `wisi-end-caches'."
  (let ((i 0)
	pos cache)
    (while (< i (length wisi-end-caches))
      (setq pos (nth i wisi-end-caches))
      (setq cache (wisi-get-cache pos))

      (if (and (>= pos start-mark)
	       (<  pos end-mark))
	  (progn
	    (setf (wisi-cache-end cache) end-mark)
	    (setq wisi-end-caches (delq pos wisi-end-caches)))

	;; else not in range
	(setq i (1+ i)))
      )))

(defvar wisi-tokens nil
  "Array of ‘wisi-tok’ structures for the right hand side of the current production.
Let-bound in parser semantic actions.")

(defvar wisi-nterm nil
  "The token id for the left hand side of the current production.
Let-bound in parser semantic actions.")

(defun wisi-statement-action (pairs)
  "Cache navigation information in text properties of tokens.
Intended as a grammar non-terminal action.

PAIRS is a vector of the form [TOKEN-NUMBER CLASS TOKEN-NUMBER
CLASS ...] where TOKEN-NUMBER is the (1 indexed) token number in
the production, CLASS is the wisi class of that token. Use in a
grammar action as:
  (wisi-statement-action [1 statement-start 7 statement-end])"
  (when (eq wisi--parse-action 'navigate)
    (save-excursion
      (let ((first-item t)
	    first-keyword-mark
	    (override-start nil)
	    (i 0))
	(while (< i (length pairs))
	  (let* ((number (1- (aref pairs i)))
		 (region (wisi-tok-region (aref wisi-tokens number)))
		 (token (wisi-tok-token (aref wisi-tokens number)))
		 (class (aref pairs (setq i (1+ i))))
		 (mark (when region (copy-marker (car region) t)))
		 cache)

	    (setq i (1+ i))

	    (unless (seq-contains wisi-class-list class)
	      (error "%s not in wisi-class-list" class))

	    (if region
		(progn
		  (if (setq cache (wisi-get-cache (car region)))
		      ;; We are processing a previously set non-terminal; ie simple_statement in
		      ;;
		      ;; statement : label_opt simple_statement
		      ;;
		      ;; override nonterm, class, containing
		      (progn
			(setf (wisi-cache-class cache) (or override-start class))
			(setf (wisi-cache-nonterm cache) wisi-nterm)
			(setf (wisi-cache-containing cache) first-keyword-mark)
			(if wisi-end-caches
			    (push (car region) wisi-end-caches)
			  (setq wisi-end-caches (list (car region)))
			  ))

		    ;; else create new cache
		    (with-silent-modifications
		      (put-text-property
		       (car region)
		       (1+ (car region))
		       'wisi-cache
		       (wisi-cache-create
			:nonterm    wisi-nterm
			:token      token
			:last       (- (cdr region) (car region))
			:class      (or override-start class)
			:containing first-keyword-mark)
		       ))
		    (if wisi-end-caches
			(push (car region) wisi-end-caches)
		      (setq wisi-end-caches (list (car region)))
		      ))

		  (when first-item
		    (setq first-item nil)
		    (when (or override-start
			      (eq class 'statement-start))
		      (setq override-start nil)
		      (setq first-keyword-mark mark)))

		  (when (eq class 'statement-end)
		    (wisi--set-end first-keyword-mark (copy-marker (car region) t)))
		  )

	      ;; region is nil when a production is empty; if the first
	      ;; token is a start, override the class on the next token.
	      (when (and first-item
			 (eq class 'statement-start))
		(setq override-start class)))
	    ))
	))))

(defun wisi-containing-action (containing-token contained-token)
  "Set containing marks in all tokens in CONTAINED-TOKEN
with null containing mark to marker pointing to CONTAINING-TOKEN.
If CONTAINING-TOKEN is empty, the next token number is used."
  (when (eq wisi--parse-action 'navigate)
    (let* ((containing-tok (aref wisi-tokens (1- containing-token)))
	   (containing-region (wisi-tok-region containing-tok))
	   (contained-tok (aref wisi-tokens (1- contained-token)))
	   (contained-region (wisi-tok-region contained-tok)))

      (unless (or containing-region (wisi-tok-virtual containing-tok))
	(signal 'wisi-parse-error
		(wisi-error-msg
		 "wisi-containing-action: containing-region '%s' is empty. grammar error; bad action"
		 (wisi-tok-token containing-tok))))

      (unless (or (not contained-region) ;; contained-token is empty
		  (wisi-tok-virtual contained-tok)
		  (wisi-tok-virtual containing-tok)
		  (wisi-get-cache (car containing-region)))
	(signal 'wisi-parse-error
		(wisi-error-msg
		 "wisi-containing-action: containing-token '%s' has no cache. grammar error; missing action"
		 (wisi-token-text (aref wisi-tokens (1- containing-token))))))

      (when (and (not (or (wisi-tok-virtual containing-tok)
		     (wisi-tok-virtual contained-tok)))
		 contained-region)
	  ;; nil when empty production, may not contain any caches
	  (save-excursion
	    (goto-char (cdr contained-region))
	    (let ((cache (wisi-backward-cache))
		  (mark (copy-marker (car containing-region) t)))
	      (while cache

		;; skip blocks that are already marked
		(while (and (>= (point) (car contained-region))
			    (markerp (wisi-cache-containing cache)))
		  (goto-char (wisi-cache-containing cache))
		  (setq cache (wisi-get-cache (point))))

		(if (or (and (= (car containing-region) (car contained-region))
			     (<= (point) (car contained-region)))
			(< (point) (car contained-region)))
		    ;; done
		    (setq cache nil)

		  ;; else set mark, loop
		  (setf (wisi-cache-containing cache) mark)
		  (setq cache (wisi-backward-cache)))
		))))
      )))

(defun wisi--match-token (cache tokens start)
  "Return t if CACHE has id from TOKENS and is at START or has containing equal to START.
point must be at cache token start.
TOKENS is a vector [number token_id token_id ...].
number is ignored."
  (let ((i 1)
	(done nil)
	(result nil)
	token)
    (when (or (= start (point))
	      (and (wisi-cache-containing cache)
		   (= start (wisi-cache-containing cache))))
      (while (and (not done)
		  (< i (length tokens)))
	(setq token (aref tokens i))
	(if (eq token (wisi-cache-token cache))
	    (setq result t
		  done t)
	  (setq i (1+ i)))
	))
    result))

(defun wisi-motion-action (token-numbers)
  "Set prev/next marks in all tokens given by TOKEN-NUMBERS.
TOKEN-NUMBERS is a vector with each element one of:

number: the token number; mark that token

vector [number token_id]:
vector [number token_id token_id ...]:
   mark all tokens in number nonterminal matching token_id with nil prev/next."
  (when (eq wisi--parse-action 'navigate)
    (save-excursion
      (let (prev-keyword-mark
	    prev-cache
	    token
	    start
	    cache
	    mark
	    (i 0))
	(while (< i (length token-numbers))
	  (let ((token-number (aref token-numbers i))
		region)
	    (setq i (1+ i))
	    (cond
	     ((numberp token-number)
	      (setq token (aref wisi-tokens (1- token-number)))
	      (setq region (wisi-tok-region token))
	      (when region
		(unless start (setq start (car region)))
		(setq cache (wisi-get-cache (car region)))
		(unless cache (error "no cache on token %d; add to statement-action" token-number))
		(setq mark (copy-marker (car region) t))

		(if prev-keyword-mark
		    (progn
		      (setf (wisi-cache-prev cache) prev-keyword-mark)
		      (setf (wisi-cache-next prev-cache) mark)
		      (setq prev-keyword-mark mark)
		      (setq prev-cache cache))

		  ;; else first token; save as prev
		  (setq prev-keyword-mark mark)
		  (setq prev-cache cache))
		))

	     ((vectorp token-number)
	      ;; token-number may contain 1 or more token_ids
	      ;; the corresponding region may be empty
	      ;; there may not have been a prev keyword
	      (setq region (wisi-tok-region (aref wisi-tokens (1- (aref token-number 0)))))
	      (when region ;; not an empty token
		;; We must search for all targets at the same time, to
		;; get the motion order right.
		(unless start (setq start (car region)))
		(goto-char (car region))
		(setq cache (wisi-get-cache (point)))
		(unless cache (error "no cache at %d; add to statement-action" (car region)))
		(while (< (point) (cdr region))
		  (when (wisi--match-token cache token-number start)
		    (setq mark (copy-marker (point) t))

		    (if prev-keyword-mark
			;; Don't include this token if prev/next
			;; already set by a lower level statement,
			;; such as a nested if/then/elsif/end if.
			(when (and (null (wisi-cache-prev cache))
				   (null (wisi-cache-next prev-cache)))
			  (setf (wisi-cache-prev cache) prev-keyword-mark)
			  (setf (wisi-cache-next prev-cache) mark)
			  (setq prev-keyword-mark mark)
			  (setq prev-cache cache))

		      ;; else first token; save as prev
		      (setq prev-keyword-mark mark)
		      (setq prev-cache cache)))

		  (setq cache (wisi-forward-cache))
		  )))

	     (t
	      (error "unexpected token-number %s" token-number))
	     )

	    ))
	))))

(defun wisi--face-put-cache (region class)
  "Put a ’wisi-face’ cache with class CLASS on REGION."
  (when (> wisi-debug 1)
    (message "face: put cache %s:%s" region class))
  (with-silent-modifications
    (put-text-property
     (car region)
     (1+ (car region))
     'wisi-face
     (wisi-cache-create
      :last (- (cdr region) (car region))
      :class class)
     )))

(defun wisi-face-mark-action (pairs)
  "PAIRS is a vector of TOKEN CLASS pairs; mark TOKEN (token number)
as having face CLASS (prefix or suffix).
Intended as a grammar action."
  (when (eq wisi--parse-action 'face)
    (let ((i 0))
      (while (< i (length pairs))
	(let ((region (wisi-tok-region (aref wisi-tokens (1- (aref pairs i)))))
	      (class (aref pairs (setq i (1+ i)))))
	  (when region
	    ;; region can be null on an optional or virtual token
	    (let ((cache (get-text-property (car region) 'wisi-face)))
	      (if cache
		  ;; previously marked; extend this cache, delete any others
		  (progn
		    (with-silent-modifications
		      (remove-text-properties (+ (car region) (wisi-cache-last cache)) (cdr region) '(wisi-face nil)))
		    (setf (wisi-cache-class cache) class)
		    (setf (wisi-cache-last cache) (- (cdr region) (car region))))

		;; else not previously marked
		(wisi--face-put-cache region class)))
	    ))
	))))

(defun wisi-face-remove-action (tokens)
  "Remove face caches and faces in TOKENS.
Intended as a grammar action.

TOKENS is a vector of token numbers."
  (when (eq wisi--parse-action 'face)
    (let ((i 0))
      (while (< i (length tokens))
	(let* ((number (1- (aref tokens i)))
	       (region (wisi-tok-region (aref wisi-tokens number)))
	       face-cache)

	  (setq i (1+ i))

	  (when region
	    (let ((pos (car region)))
	      (while (< pos (cdr region))
		(when (setq face-cache (get-text-property pos 'wisi-face))
		  (when (> wisi-debug 1)
		    (message "face: remove face %s" (cons pos (+ pos (wisi-cache-last face-cache)))))
		  (with-silent-modifications
		    (remove-text-properties
		     pos (+ pos (wisi-cache-last face-cache))
		     (list
		      'wisi-face nil
		      'font-lock-face nil
		      'fontified t))))
		(setq pos (next-single-property-change
			   (+ pos (or (and face-cache
					   (wisi-cache-last face-cache))
				      0))
			   'wisi-face nil (cdr region)))
		)))
	  )))))

(defun wisi--face-action-1 (face region)
  "Apply FACE to REGION."
  (when region
    (when (> wisi-debug 1)
      (message "face: add face %s:%s" region face))
    (with-silent-modifications
      (add-text-properties
       (car region) (cdr region)
       (list
	'font-lock-face face
	'fontified t)))
    ))

(defun wisi-face-apply-action (triples)
  "Set face information in `wisi-face' text properties of tokens.
Intended as a grammar non-terminal action.

TRIPLES is a vector of the form [TOKEN-NUMBER PREFIX-FACE SUFFIX-FACE ...]

In the first ’wisi-face’ cache in each token region, apply
PREFIX-FACE to class PREFIX, SUFFIX-FACE to class SUFFIX, or
SUFFIX-FACE to all of the token region if there is no ’wisi-face’
cache."
  (when (eq wisi--parse-action 'face)
    (let (number prefix-face suffix-face (i 0))
      (while (< i (length triples))
	(setq number (aref triples i))
	(setq prefix-face (aref triples (setq i (1+ i))))
	(setq suffix-face (aref triples (setq i (1+ i))))
	(cond
	 ((integerp number)
	  (let* ((token-region (wisi-tok-region (aref wisi-tokens (1- number))))
		 (pos (car token-region))
		 (j 0)
		 (some-cache nil)
		 cache)
	    (when token-region
	      ;; region can be null for an optional or virtual token
	      (while (< j 2)
		(setq cache (get-text-property pos 'wisi-face))
		(cond
		 ((and (not some-cache)
		       (null cache))
		  ;; cache is null when applying a face to a token
		  ;; directly, without first calling
		  ;; wisi-face-mark-action. Or when there is a
		  ;; previously applied face in a lower level token,
		  ;; such as a numeric literal.
		  (wisi--face-action-1 suffix-face token-region))

		 ((and cache
		       (eq 'prefix (wisi-cache-class cache)))
		  (setq some-cache t)
		  (wisi--face-action-1 prefix-face (wisi-cache-region cache pos)))

		 ((and cache
		       (eq 'suffix (wisi-cache-class cache)))
		  (setq some-cache t)
		  (wisi--face-action-1 suffix-face (wisi-cache-region cache pos)))

		 (t
		  ;; don’t apply a face
		  nil)
		 )

		(setq j (1+ j))
		(if suffix-face
		    (setq pos (next-single-property-change (+ 2 pos) 'wisi-face nil (cdr token-region)))
		  (setq j 2))
		))))

	 (t
	  ;; catch conversion errors from previous grammar syntax
	  (error "wisi-face-apply-action with non-integer token number"))
	 )
	(setq i (1+ i))
	))))

(defun wisi-face-apply-list-action (triples)
  "Similar to ’wisi-face-apply-action’, but applies faces to all
tokens with a `wisi-face' cache in the wisi-tokens[token-number]
region, and does not apply a face if there are no such caches."
  (when (eq wisi--parse-action 'face)
    (let (number token-region face-region prefix-face suffix-face cache (i 0) pos)
      (while (< i (length triples))
	(setq number (aref triples i))
	(setq prefix-face (aref triples (setq i (1+ i))))
	(setq suffix-face (aref triples (setq i (1+ i))))
	(cond
	 ((integerp number)
	  (setq token-region (wisi-tok-region (aref wisi-tokens (1- number))))
	  (when token-region
	    ;; region can be null for an optional token
	    (setq pos (car token-region))
	    (while (and pos
			(< pos (cdr token-region)))
	      (setq cache (get-text-property pos 'wisi-face))
	      (setq face-region (wisi-cache-region cache pos))
	      (cond
	       ((or (null (wisi-cache-class cache))
		    (eq 'prefix (wisi-cache-class cache)))
		(wisi--face-action-1 prefix-face face-region))
	       ((eq 'suffix (wisi-cache-class cache))
		(wisi--face-action-1 suffix-face face-region))

	       (t
		(error "wisi-face-apply-list-action: face cache class is not prefix or suffix")))

	      (setq pos (next-single-property-change (1+ pos) 'wisi-face nil (cdr token-region)))
	      )))
	 (t
	  ;; catch conversion errors from previous grammar syntax
	  (error "wisi-face-apply-list-action with non-integer token number"))
	 )
	(setq i (1+ i))
	))))

;;;; indent action

(defvar wisi-token-index nil
  "Index of current token in `wisi-tokens'.
Let-bound in `wisi-indent-action', for grammar actions.")

(defvar wisi-indent-comment nil
  "Non-nil if computing indent for comment.
Let-bound in `wisi-indent-action', for grammar actions.")

(defun wisi-indent-zero-p (indent)
  (cond
   ((integerp indent)
    (= indent 0))

   (t ;; 'anchor
    (integerp (nth 2 indent)))
   ))

(defun wisi--apply-int (i delta)
  "Add DELTA (an integer) to the indent at index I."
  (let ((indent (aref (wisi-ind-indent wisi--indent) i))) ;; reference if list

    (cond
     ((integerp indent)
      (aset (wisi-ind-indent wisi--indent) i (+ delta indent)))

     ((listp indent)
      (cond
       ((eq 'anchor (car indent))
	(when (integerp (nth 2 indent))
	  (setf (nth 2 indent) (+ delta (nth 2 indent)))
	  ;; else anchored; not affected by this delta
	  ))

       ((eq 'anchored (car indent))
	;; not affected by this delta
	)))

     (t
      (error "wisi--apply-int: invalid form in wisi-ind-indent: %s" indent))
     )))

(defun wisi--apply-anchored (delta i)
  "Apply DELTA (an anchored indent) to indent I."
  ;; delta is from wisi-anchored; ('anchored 1 delta no-accumulate)
  (let ((indent (aref (wisi-ind-indent wisi--indent) i))
	(accumulate (not (nth 3 delta))))

    (cond
     ((integerp indent)
      (when (or accumulate
		(= indent 0))
	(let ((temp (seq-take delta 3)))
    	  (setf (nth 2 temp) (+ indent (nth 2 temp)))
	  (aset (wisi-ind-indent wisi--indent) i temp))))

     ((and (listp indent)
	   (eq 'anchor (car indent))
	   (integerp (nth 2 indent)))
      (when (or accumulate
		(= (nth 2 indent) 0))
	(let ((temp (seq-take delta 3)))
	  (setf (nth 2 temp) (+ (nth 2 indent) (nth 2 temp)))
	  (setf (nth 2 indent) temp))))
     )))

(defun wisi--indent-token-1 (line end delta)
  "Apply indent DELTA to all lines from LINE (a line number) thru END (a buffer position)."
  (let ((i (1- line));; index to wisi-ind-line-begin, wisi-ind-indent
	(paren-first (when (and (listp delta)
				(eq 'hanging (car delta)))
		       (nth 2 delta))))

    (while (<= (aref (wisi-ind-line-begin wisi--indent) i) end)
      (unless
	  (and ;; no check for called from wisi--indent-comment;
	       ;; comments within tokens are indented by
	       ;; wisi--indent-token
	       wisi-indent-comment-col-0
	       (= 11 (syntax-class (syntax-after (aref (wisi-ind-line-begin wisi--indent) i)))))
	(cond
	 ((integerp delta)
	  (wisi--apply-int i delta))

	 ((listp delta)
	  (cond
	   ((eq 'anchored (car delta))
	    (wisi--apply-anchored delta i))

	   ((eq 'hanging (car delta))
	    ;; from wisi-hanging; delta is ('hanging first-line nest delta1 delta2 no-accumulate)
	    ;; delta1, delta2 may be anchored
	    (when (or (not (nth 5 delta))
		      (wisi-indent-zero-p (aref (wisi-ind-indent wisi--indent) i)))
	      (if (= i (1- (nth 1 delta)))
		  ;; apply delta1
		  (let ((delta1 (nth 3 delta)))
		    (cond
		     ((integerp delta1)
		      (wisi--apply-int i delta1))

		     (t ;; anchored
		      (wisi--apply-anchored delta1 i))
		     ))

		;; don't apply hanging indent in nested parens.
		;; test/ada_mode-parens.adb
		;; No_Conditional_Set : constant Ada.Strings.Maps.Character_Set :=
		;;   Ada.Strings.Maps."or"
		;;     (Ada.Strings.Maps.To_Set (' '),
		(when (= paren-first
			 (nth 0 (save-excursion (syntax-ppss (aref (wisi-ind-line-begin wisi--indent) i)))))
		  (let ((delta2 (nth 4 delta)))
		    (cond
		     ((integerp delta2)
		      (wisi--apply-int i delta2))

		     (t ;; anchored
		      (wisi--apply-anchored delta2 i))
		     )))
		)))

	   (t
	    (error "wisi--indent-token-1: invalid delta: %s" delta))
	   )) ;; listp delta

	 (t
	  (error "wisi--indent-token-1: invalid delta: %s" delta))
	 ))
      (setq i (1+ i))
      )))

(defun wisi--indent-token (tok token-delta)
  "Add TOKEN-DELTA to all indents in TOK region,"
  (let ((line (if (wisi-tok-nonterminal tok)
		  (wisi-tok-first tok)
		(when (wisi-tok-first tok) (wisi-tok-line tok))))
	(end (cdr (wisi-tok-region tok))))
    (when (and line end token-delta)
      (wisi--indent-token-1 line end token-delta))))

(defun wisi--indent-comment (tok comment-delta)
  "Add COMMENT-DELTA to all indents in comment region following TOK."
  (let ((line (wisi-tok-comment-line tok))
	(end (wisi-tok-comment-end tok)))
    (when (and line end comment-delta)
      (wisi--indent-token-1 line end comment-delta))))

(defun wisi-anchored-1 (tok offset &optional no-accumulate)
  "Return offset of TOK relative to current indentation + OFFSET.
For use in grammar indent actions."
  (when (wisi-tok-region tok)
    ;; region can be nil when token is inserted by error recovery
    (let ((pos (car (wisi-tok-region tok)))
	  delta)

      (goto-char pos)
      (setq delta (+ offset (- (current-column) (current-indentation))))
      (wisi-anchored-2
       (wisi-tok-line tok) ;; anchor-line
       (if wisi-indent-comment
	   (wisi-tok-comment-end (aref wisi-tokens wisi-token-index))
	 (cdr (wisi-tok-region (aref wisi-tokens wisi-token-index))));; end
       delta
       no-accumulate)
      )))

(defun wisi--max-anchor (begin-line end)
  (let ((i (1- begin-line))
	(result 0))
    (while (<= (aref (wisi-ind-line-begin wisi--indent) i) end)
      (let ((indent (aref (wisi-ind-indent wisi--indent) i)))
	(when (listp indent)
	  (cond
	   ((eq 'anchor (car indent))
	    (setq result (max result (car (nth 1 indent))))
	    (when (listp (nth 2 indent))
	      (setq result (max result (nth 1 (nth 2 indent))))
	      ))
	   (t ;; anchored
	    (setq result (max result (nth 1 indent))))
	   )))
      (setq i (1+ i)))
    result
    ))

(defun wisi-anchored-2 (anchor-line end delta no-accumulate)
  "Set ANCHOR-LINE as anchor, increment anchors thru END, return anchored delta."
  ;; Typically, we use anchored to indent relative to a token buried in a line:
  ;;
  ;; test/ada_mode-parens.adb
  ;; Local_2 : Integer := (1 + 2 +
  ;;                         3);
  ;; line starting with '3' is anchored to '('
  ;;
  ;; If the anchor is a nonterminal, and the first token in the anchor
  ;; is also first on a line, we don't need anchored to compute the
  ;; delta:
  ;;
  ;; test/ada_mode-parens.adb
  ;; Local_5 : Integer :=
  ;;   (1 + 2 +
  ;;      3);
  ;; delta for line starting with '3' can just be '3'.
  ;;
  ;; However, in some places we need anchored to prevent later
  ;; deltas from accumulating:
  ;;
  ;; test/ada_mode-parens.adb
  ;; No_Conditional_Set : constant Ada.Strings.Maps.Character_Set :=
  ;;   Ada.Strings.Maps."or"
  ;;     (Ada.Strings.Maps.To_Set (' '),
  ;;
  ;; here the function call actual parameter part is indented first
  ;; by 'name' and by 'expression'; we use anchored to keep the
  ;; 'name' indent and ignore the later addition.
  ;;
  ;; So we apply anchored whether the anchor token is first or not.

  (let* ((i (1- anchor-line))
	 (indent (aref (wisi-ind-indent wisi--indent) i)) ;; reference if list
	 (anchor-id (1+ (wisi--max-anchor anchor-line end))))

    ;; Set anchor
    (cond
     ((integerp indent)
      (aset (wisi-ind-indent wisi--indent) i (list 'anchor (list anchor-id) indent)))

     ((and (listp indent)
	   (eq 'anchor (car indent)))
      (push anchor-id (nth 1 indent)))

     ((and (listp indent)
	   (eq 'anchored (car indent)))
      (aset (wisi-ind-indent wisi--indent) i (list 'anchor (list anchor-id) (copy-sequence indent))))

     (t
      (error "wisi-anchored-delta: invalid form in indent: %s" indent)))

    (list 'anchored anchor-id delta no-accumulate)
    ))

(defun wisi-anchored (token-number offset &optional no-accumulate)
  "Return offset of token TOKEN-NUMBER in `wisi-tokens'.relative to current indentation + OFFSET.
For use in grammar indent actions."
  (wisi-anchored-1 (aref wisi-tokens (1- token-number)) offset no-accumulate))

(defun wisi-anchored* (token-number offset)
  "If TOKEN-NUMBER token in `wisi-tokens' is first on a line,
call ’wisi-anchored OFFSET’. Otherwise return 0.
For use in grammar indent actions."
  (if (wisi-tok-first (aref wisi-tokens (1- token-number)))
      (wisi-anchored token-number offset)
    0))

(defun wisi-anchored*- (token-number offset)
  "If existing indent is zero, and TOKEN-NUMBER token in `wisi-tokens' is first on a line,
call ’wisi-anchored OFFSET’. Otherwise return 0.
For use in grammar indent actions."
  (if (wisi-tok-first (aref wisi-tokens (1- token-number)))
      (wisi-anchored token-number offset t)
    0))

(defun wisi--paren-in-anchor-line (anchor-tok offset)
  "If there is an opening paren containing ANCHOR-TOK in the same line as ANCHOR-TOK,
call ’wisi-anchored’ with OFFSET plus the delta from the line
begin to the paren position. Otherwise return OFFSET."
  (let* ((tok-syntax (syntax-ppss (car (wisi-tok-region anchor-tok))))
	 (paren-pos (nth 1 tok-syntax))
	 (anchor-line (wisi-tok-line anchor-tok)))

    (when (and paren-pos ;; in paren
	      (< paren-pos (aref (wisi-ind-line-begin wisi--indent) (1- anchor-line))))
      ;; paren not in anchor line
      (setq paren-pos nil))

    (if paren-pos
	(progn
	  (goto-char paren-pos)
	  (+ 1 (- (current-column) (current-indentation)) offset))
      offset)
    ))

(defun wisi-anchored% (token-number offset &optional no-accumulate)
  "Anchor the current token at OFFSET from either the first token on the line
containing TOKEN-NUMBER in `wisi-tokens', or an enclosing paren on that line.
Return the delta.
For use in grammar indent actions."
  (let* ((indent-tok (aref wisi-tokens wisi-token-index))
	 ;; tok is a nonterminal; this function makes no sense for terminals
	 (anchor-tok (aref wisi-tokens (1- token-number))))

    (when (not (or (wisi-tok-virtual indent-tok)
		   (wisi-tok-virtual anchor-tok)))
      (wisi-anchored-2
       (wisi-tok-line anchor-tok)

       (if wisi-indent-comment
	   (wisi-tok-comment-end indent-tok)
	 (cdr (wisi-tok-region indent-tok))) ;; end

       (wisi--paren-in-anchor-line anchor-tok offset)
       no-accumulate))
    ))

(defun wisi-anchored%- (token-number offset)
  "If existing indent is zero, anchor the current token at OFFSET
from the first token on the line containing TOKEN-NUMBER in `wisi-tokens'.
Return the delta.
For use in grammar indent actions."
  (wisi-anchored% token-number offset t))

(defun wisi-hanging-1 (delta1 delta2 option no-accumulate)
  "If OPTION is nil, implement `wisi-hanging'; otherwise `wisi-hanging%'."
  (if wisi-indent-comment
      delta1

    (let* ((tok (aref wisi-tokens wisi-token-index))
	   (tok-syntax (syntax-ppss (car (wisi-tok-region tok)))))
      ;; tok is a nonterminal; this function makes no sense for terminals
      ;; syntax-ppss moves point to start of tok

      (cond
       ((functionp wisi-indent-hanging-function)
	(let ((indent-hanging (funcall wisi-indent-hanging-function tok delta1 delta2 option)))
	  (list 'hanging
		(wisi-tok-line tok) ;; first line of token
		(nth 0 tok-syntax) ;; paren nest level at tok
		(nth 0 indent-hanging)
		(if (or (not option)
			(= 2 (length indent-hanging)))
		    (nth 1 indent-hanging)
		  (nth 0 indent-hanging)) ;; simplest way to implement no-accumulate
		no-accumulate)))

      (t
       (list 'hanging
	     (wisi-tok-line tok) ;; first line of token
	     (nth 0 tok-syntax) ;; paren nest level at tok
	     delta1
	     (if (or (not option)
		     (= (wisi-tok-line tok) (wisi-tok-first tok))) ;; first token in tok is first on line
		 delta2
	       delta1)
	     no-accumulate))
       ))
    ))

(defun wisi-hanging (delta1 delta2)
  "Use DETLA1 for first line, DELTA2 for following lines.
For use in grammar indent actions."
  (wisi-hanging-1 delta1 delta2 nil nil))

(defun wisi-hanging% (delta1 delta2)
  "If first token is first in line, use DETLA1 for first line, DELTA2 for following lines.
Otherwise use DELTA1 for all lines.
For use in grammar indent actions."
  (wisi-hanging-1 delta1 delta2 t nil))

(defun wisi-hanging%- (delta1 delta2)
  "If existing indent is non-zero, do nothing.
Else if first token is first in line, use DETLA1 for first line,
DELTA2 for following lines.  Otherwise use DELTA1 for all lines.
For use in grammar indent actions."
  (wisi-hanging-1 delta1 delta2 t t))

(defun wisi--indent-compute-delta (delta tok)
  "Return evaluation of DELTA."
  (cond
   ((integerp delta)
    delta)

   ((symbolp delta)
    (symbol-value delta))

   ((vectorp delta)
    ;; [token comment]
    ;; if wisi-indent-comment, we are indenting the comments of the
    ;; previous token; they should align with the 'token' delta.
    (wisi--indent-compute-delta (aref delta 0) tok))

   (t ;; form
    (save-excursion
      (goto-char (car (wisi-tok-region tok)))
      (eval delta)))
   ))

(defun wisi-indent-action (deltas)
  "Accumulate `wisi--indents' from DELTAS.
DELTAS is a vector; each element can be:
- an integer
- a symbol
- a lisp form
- a vector.

The first three are evaluated to give an integer delta. A vector must
have two elements, giving the code and following comment
deltas. Otherwise the comment delta is the following delta in
DELTAS."
  (when (eq wisi--parse-action 'indent)
    (dotimes (wisi-token-index (length wisi-tokens))
      (let* ((tok (aref wisi-tokens wisi-token-index))
	     (token-delta (aref deltas wisi-token-index))
	     (comment-delta
	      (cond
	       ((vectorp token-delta)
		(aref token-delta 1))

	       ((< wisi-token-index (1- (length wisi-tokens)))
		(aref deltas (1+ wisi-token-index)))
	       )))
	(when (wisi-tok-region tok)
	  ;; region is null when optional nonterminal is empty
	  (let ((wisi-indent-comment nil))
	    (setq token-delta
		  (when (and token-delta
			     (wisi-tok-first tok))
		    (wisi--indent-compute-delta token-delta tok)))

	    (when (and token-delta
		       (or (consp token-delta)
			   (not (= 0 token-delta))))
	      (wisi--indent-token tok token-delta))

	    (setq wisi-indent-comment t)
	    (setq comment-delta
		  (when (and comment-delta
			     (wisi-tok-comment-line tok))
		    (wisi--indent-compute-delta comment-delta tok)))

	    (when (and comment-delta
		       (or (consp comment-delta)
			   (not (= 0 comment-delta))))
	      (wisi--indent-comment tok comment-delta))
	    )
	  )))))

(defun wisi-indent-action* (n deltas)
  "If any of the first N tokens in `wisi-tokens' is first on a line,
call `wisi-indent-action' with DETLAS.  Otherwise do nothing."
  (when (eq wisi--parse-action 'indent)
    (let ((done nil)
	  (i 0)
	  tok)
      (while (and (not done)
		  (< i n))
	(setq tok (aref wisi-tokens i))
	(setq i (1+ i))
	(when (and (wisi-tok-region tok)
		   (wisi-tok-first tok))
	  (setq done t)
	  (wisi-indent-action deltas))
	))))

(defun wisi--indent-leading-comments ()
  "Set `wisi-ind-indent to 0 for comment lines before first token in buffer.
Leave point at first token (or eob)."
  (save-excursion
    (goto-char (point-min))
    (forward-comment (point-max))
    (let ((end (point))
	  (i 0)
	  (max-i (length (wisi-ind-line-begin wisi--indent))))
      (while (< (aref (wisi-ind-line-begin wisi--indent) i) end)
	(when (< i max-i)
	  (aset (wisi-ind-indent wisi--indent) i 0))
	(setq i (1+ i)))
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
  "Search at point or forward for a token that has a cache with CLASS.
Return cache, or nil if at end of buffer.
If LIMIT (a buffer position) is reached, throw an error."
  (let ((cache (or (wisi-get-cache (point))
		   (wisi-forward-cache))))
    (while (not (eq class (wisi-cache-class cache)))
      (setq cache (wisi-forward-cache))
      (when (>= (point) limit)
	(error "cache with class %s not found" class)))
    cache))

(defun wisi-forward-find-token (token limit &optional noerror)
  "Search forward for TOKEN.
If point is at a matching token, return that token.  TOKEN may be
a list; stop on any member of the list.  Return `wisi-tok'
struct, or if LIMIT (a buffer position) is reached, then if
NOERROR is nil, throw an error, if non-nil, return nil."
  (let ((token-list (cond
		     ((listp token) token)
		     (t (list token))))
	(tok (wisi-forward-token))
	(done nil))
    (while (not (or done
		    (memq (wisi-tok-token tok) token-list)))
      (setq tok (wisi-forward-token))
      (when (or (>= (point) limit)
		(eobp))
	(goto-char limit)
	(if noerror
	    (setq done t)
	  (error "token %s not found" token))))
    tok))

(defun wisi-forward-find-cache-token (ids limit)
  "Search forward for a cache with token in IDS (a list of token ids).
Return cache, or nil if at LIMIT or end of buffer."
  (let ((cache (wisi-forward-cache)))
    (while (and (< (point) limit)
		(not (eobp))
		(not (memq (wisi-cache-token cache) ids)))
      (setq cache (wisi-forward-cache)))
    cache))

(defun wisi-forward-find-nonterm (nonterm limit)
  "Search forward for a token that has a cache with NONTERM.
NONTERM may be a list; stop on any cache that has a member of the list.
Return cache, or nil if at end of buffer.
If LIMIT (a buffer position) is reached, throw an error."
  (let ((nonterm-list (cond
		       ((listp nonterm) nonterm)
		       (t (list nonterm))))
	(cache (wisi-forward-cache)))
    (while (not (memq (wisi-cache-nonterm cache) nonterm-list))
      (setq cache (wisi-forward-cache))
      (when (>= (point) limit)
	(error "cache with nonterm %s not found" nonterm)))
    cache))

(defun wisi-goto-cache-next (cache)
  (goto-char (wisi-cache-next cache))
  (wisi-get-cache (point))
  )

(defun wisi-forward-statement-keyword ()
  "If not at a cached token, move forward to next
cache. Otherwise move to cache-next, or cache-end, or next cache
if both nil.  Return cache found."
  (unless (eobp)
    (wisi-validate-cache (point-max) t 'navigate) ;; ensure there is a next cache to move to
    (let ((cache (wisi-get-cache (point))))
      (if (and cache
	       (not (eq (wisi-cache-class cache) 'statement-end)))
	  (let ((next (or (wisi-cache-next cache)
			  (wisi-cache-end cache))))
	    (if next
		(goto-char next)
	      (wisi-forward-token)
	      (wisi-forward-cache)))
	(wisi-forward-cache))
      )
    (wisi-get-cache (point))
    ))

(defun wisi-backward-statement-keyword ()
  "If not at a cached token, move backward to prev
cache. Otherwise move to cache-prev, or prev cache if nil."
  (wisi-validate-cache (point) t 'navigate)
  (let ((cache (wisi-get-cache (point)))
	prev)
    (when cache
      (setq prev (wisi-cache-prev cache))
      (unless prev
	(unless (eq 'statement-start (wisi-cache-class cache))
	  (setq prev (wisi-cache-containing cache)))))
    (if prev
	(goto-char prev)
      (wisi-backward-cache))
  ))

(defun wisi-forward-sexp (&optional arg)
  "For `forward-sexp-function'."
  (interactive "^p")
  (or arg (setq arg 1))
  (cond
   ((and (> arg 0) (= 4 (syntax-class (syntax-after (point)))))  ;; on open paren
    (let ((forward-sexp-function nil))
      (forward-sexp arg)))

   ((and (< arg 0) (= 5 (syntax-class (syntax-after (1- (point)))))) ;; after close paren
    (let ((forward-sexp-function nil))
      (forward-sexp arg)))

   ((and (> arg 0) (= 7 (syntax-class (syntax-after (point)))))  ;; on (open) string quote
    (let ((forward-sexp-function nil))
      (forward-sexp arg)))

   ((and (< arg 0) (= 7 (syntax-class (syntax-after (1- (point)))))) ;; after (close) string quote
    (let ((forward-sexp-function nil))
      (forward-sexp arg)))

   (t
    (dotimes (_i (abs arg))
      (if (> arg 0)
	  (wisi-forward-statement-keyword)
	(wisi-backward-statement-keyword))))
   ))

(defun wisi-goto-containing (cache &optional error)
  "Move point to containing token for CACHE, return cache at that point.
If ERROR, throw error when CACHE has no container; else return nil."
  (cond
   ((markerp (wisi-cache-containing cache))
    (goto-char (wisi-cache-containing cache))
    (wisi-get-cache (point)))
   (t
    (when error
      (error "already at outermost containing token")))
   ))

(defun wisi-goto-containing-paren (cache)
  "Move point to just after the open-paren containing CACHE.
Return cache for paren, or nil if no containing paren."
  (while (and cache
	      (not (eq (wisi-cache-class cache) 'open-paren)))
    (setq cache (wisi-goto-containing cache)))
  (when cache
    (forward-char 1))
  cache)

(defun wisi-goto-start (cache)
  "Move point to containing ancestor of CACHE that has class statement-start.
Return start cache."
  ;; cache nil at bob, or on cache in partially parsed statement
  (while (and cache
	      (not (eq (wisi-cache-class cache) 'statement-start)))
    (setq cache (wisi-goto-containing cache)))
  cache)

(defun wisi-goto-end-1 (cache)
  (goto-char (wisi-cache-end cache)))

(defun wisi-goto-statement-start ()
  "Move point to token at start of statement point is in or after.
Return start cache."
  (interactive)
  (wisi-validate-cache (point) t 'navigate)
  (wisi-goto-start (or (wisi-get-cache (point))
		       (wisi-backward-cache))))

(defun wisi-goto-statement-end ()
  "Move point to token at end of statement point is in or before."
  (interactive)
  (wisi-validate-cache (point) t 'navigate)
  (let ((cache (or (wisi-get-cache (point))
		   (wisi-forward-cache))))
    (when (wisi-cache-end cache)
      ;; nil when cache is statement-end
      (wisi-goto-end-1 cache))
    ))

(defun wisi-next-statement-cache (cache)
  "Move point to CACHE-next, return cache; error if nil."
  (when (not (markerp (wisi-cache-next cache)))
    (error "no next statement cache"))
  (goto-char (wisi-cache-next cache))
  (wisi-get-cache (point)))

(defun wisi-prev-statement-cache (cache)
  "Move point to CACHE-prev, return cache; error if nil."
  (when (not (markerp (wisi-cache-prev cache)))
    (error "no prev statement cache"))
  (goto-char (wisi-cache-prev cache))
  (wisi-get-cache (point)))

;;;; indentation

(defun wisi-comment-indent ()
  "For `comment-indent-function'. Indent single line comment to
the comment on the previous line."
  ;; Called from `comment-indent', either to insert a new comment, or
  ;; to indent the first line of an existing one.  In either case, the
  ;; comment may be after code on the same line.  For an existing
  ;; comment, point is at the start of the starting delimiter.
  (or
   (save-excursion
     ;; Check for a preceding comment line; fail if comment follows code.
     (when (forward-comment -1)
       ;; For the case:
       ;;
       ;; code;-- comment
       ;;
       ;; point is on '--', and 'forward-comment' does not move point,
       ;; returns nil.
       (when (looking-at comment-start)
         (current-column))))

   (save-excursion
     (back-to-indentation)
     (if (looking-at comment-start)
         ;; An existing comment, no code preceding comment, and
         ;; no comment on preceding line. Return nil, so
         ;; `comment-indent' will call `indent-according-to-mode'
         nil

       ;; A comment after code on the same line.
       comment-column))
   ))

(defun wisi-indent-statement ()
  "Indent region given by `wisi-goto-start', `wisi-cache-end'."
  (wisi-validate-cache (point) t 'navigate)

  (save-excursion
    (let ((cache (or (wisi-get-cache (point))
		     (wisi-backward-cache))))
      (when cache
	;; can be nil if in header comment
	(let ((start (progn (wisi-goto-start cache) (point)))
	      (end (if (wisi-cache-end cache)
			 ;; nil when cache is statement-end
			 (wisi-cache-end cache)
		       (point))))
	  (indent-region start end)
	  ))
      )))

(defvar-local wisi-indent-calculate-functions nil
  "Functions to compute indentation special cases.
Called with point at current indentation of a line; return
indentation column, or nil if function does not know how to
indent that line. Run after parser indentation, so other lines
are indented correctly.")

(defvar-local wisi-post-indent-fail-hook
  "Function to reindent portion of buffer.
Called from `wisi-indent-region' when a parse succeeds after
failing; assumes user was editing code that is now syntactically
correct. Must leave point at indentation of current line.")

(defvar-local wisi-indent-failed nil
  "Non-nil when wisi-indent-region fails due to parse failing; cleared when indent succeeds.")

(defvar-local wisi-indent-region-fallback 'wisi-indent-region-fallback-default
  "Function to compute indent for lines in region when wisi parse fails.
Called with BEGIN END.")

(defun wisi-indent-region-fallback-default (begin end)
  ;; no indent info at point. Assume user is
  ;; editing; indent to previous lines, fix it
  ;; after parse succeeds
  (goto-char begin)
  (forward-line -1);; safe at bob
  (back-to-indentation)
  (let ((col (current-column)))
    (while (and (not (eobp))
		(< (point) end))
      (forward-line 1)
      (indent-line-to col))))

(defun wisi--set-line-begin ()
  "Set line-begin field of `wisi--indent'."
  (save-excursion
    (goto-char (point-min))

    (dotimes (i (length (wisi-ind-line-begin wisi--indent)))
      (aset (wisi-ind-line-begin wisi--indent) i (point))
      (forward-line 1))))

(defconst wisi-indent-max-anchor-depth 20)

(defun wisi-indent-region (begin end)
  "For `indent-region-function', using the wisi indentation engine."
  (let ((wisi--parse-action 'indent)
	(parse-required nil))

    (wisi--check-change)

    ;; Always indent the line containing begin.
    (setq begin (save-excursion (goto-char begin) (line-beginning-position)))

    (let* ((end-mark (copy-marker end))
	   (begin-line (count-lines (point-min) begin))
	   (i (max begin-line 1))
	   pos
	   (line-count (+ 1 begin-line (count-lines begin (point-max))))

	   ;; FIXME: don’t need wisi--indent if lexer /= elisp; dispatch to lexer init function?
	   (wisi--indent
	    (make-wisi-ind
	     :line-begin (make-vector line-count 0)
	     :indent (make-vector line-count 0)
	     :last-line nil))
	   (anchor-indent (make-vector wisi-indent-max-anchor-depth 0)))

      (wisi--set-line-begin)

      (while (and (< i line-count)
		  (<= (setq pos (aref (wisi-ind-line-begin wisi--indent) i)) end))
	(unless (get-text-property (1- pos) 'wisi-indent)
	  (setq parse-required t))
	(setq i (1+ i)))

      ;; A parse either succeeds and sets the indent cache on all
      ;; lines in the buffer, or fails and leaves valid caches
      ;; untouched.
      (when (and parse-required
		 (wisi-parse-try))

	(setq wisi--last-parse-action wisi--parse-action)
	(wisi-set-parse-try nil)
	(wisi--indent-leading-comments)
	(wisi--run-parse)

	(when (not wisi-parse-failed)
	  (setq parse-required nil)
	  (move-marker (wisi-cache-max 'indent) (point-max))

	  ;; cache indent action results
	  (dotimes (i (length (wisi-ind-indent wisi--indent)))
	    (let ((indent (aref (wisi-ind-indent wisi--indent) i)))

	      (cond
	       ((integerp indent))

	       ((listp indent)
		(let ((anchor-ids (nth 1 indent))
		      (indent2 (nth 2 indent)))
		  (cond
		   ((eq 'anchor (car indent))
		    (cond
		     ((integerp indent2)
		      (dotimes (i (length anchor-ids))
			(aset anchor-indent (nth i anchor-ids) indent2))
		      (setq indent indent2))

		     ((listp indent2) ;; 'anchored
		      (setq indent (+ (aref anchor-indent (nth 1 indent2)) (nth 2 indent2)))

		      (dotimes (i (length anchor-ids))
			(aset anchor-indent (nth i anchor-ids) indent)))

		     (t
		      (error "wisi-indent-region: invalid form in wisi-ind-indent %s" indent))
		     ));; 'anchor

		   ((eq 'anchored (car indent))
		    (setq indent (+ (aref anchor-indent (nth 1 indent)) indent2)))

		   (t
		    (error "wisi-indent-region: invalid form in wisi-ind-indent %s" indent))
		   )));; listp indent

	       (t
		(error "wisi-indent-region: invalid form in wisi-ind-indent %s" indent))
	       );; cond indent

	      (when (> i 0)
		(setq pos (aref (wisi-ind-line-begin wisi--indent) i))
		(with-silent-modifications
		  (put-text-property (1- pos) pos 'wisi-indent indent)))
	      )) ;; dotimes lines

	  )) ;; parse succeeded

      (if parse-required
	  (progn
	    ;; primary indent failed
	    (setq wisi-indent-failed t)
	    (when (functionp wisi-indent-region-fallback)
	      (funcall wisi-indent-region-fallback begin end)))

	;; Apply cached indents. Inserting or deleting spaces causes
	;; wisi-ind-line-begin to be wrong, so we can't use it in
	;; the loop.
	(save-excursion
	  (goto-char (aref (wisi-ind-line-begin wisi--indent) begin-line))
	  (let ((wisi-indenting t))
	    (while (and (not (eobp))
			(<= (point) end-mark)) ;; end-mark can be at the start of an empty line
	      (indent-line-to (if (bobp) 0 (get-text-property (1- (point)) 'wisi-indent)))
	      (forward-line 1)))

	  ;; run wisi-indent-calculate-functions
	  (when wisi-indent-calculate-functions
	    (goto-char begin)
	    (while (and (not (eobp))
			(< (point) end-mark))
	      (back-to-indentation)
	      (let ((indent
		     (run-hook-with-args-until-success 'wisi-indent-calculate-functions)))
		(when indent
		  (indent-line-to indent)))

	      (forward-line 1)))

	  (when wisi-indent-failed
	    ;; previous parse failed
	    (setq wisi-indent-failed nil)
	    (goto-char end)
	    (run-hooks 'wisi-post-indent-fail-hook))
	  ))
      )))

(defun wisi-indent-line ()
  "For `indent-line-function'."
  (let ((savep (copy-marker (point)))
	(to-indent nil))
    (back-to-indentation)
    (when (>= (point) savep)
      (setq to-indent t))

    (wisi-indent-region (line-beginning-position) (line-end-position))

    (goto-char savep)
    (when to-indent (back-to-indentation))
    ))

(defun wisi-repair-error-1 (data)
  "Repair error reported in DATA (a ’wisi-error’)"
  (let ((wisi--parse-action 'navigate) ;; tell wisi-forward-token not to compute indent stuff.
	tok-2)
      (goto-char (wisi--error-pos data))
      (dolist (tok-1 (wisi--error-popped data))
	(setq tok-2 (wisi-backward-token))
	(if (eq (wisi-tok-token tok-1) (wisi-tok-token tok-2))
	    (delete-region (car (wisi-tok-region tok-2)) (cdr (wisi-tok-region tok-2)))
	  (error "mismatched tokens: parser %s, buffer %s" (wisi-tok-token tok-1) (wisi-tok-token tok-2))))

      (dolist (tok-1 (wisi--error-deleted data))
	(setq tok-2 (wisi-forward-token))
	(if (eq (wisi-tok-token tok-1) (wisi-tok-token tok-2))
	    (delete-region (car (wisi-tok-region tok-2)) (cdr (wisi-tok-region tok-2)))
	  (error "mismatched tokens: parser %s, buffer %s" (wisi-tok-token tok-1) (wisi-tok-token tok-2))))

      (dolist (id (wisi--error-inserted data))
	(insert (cdr (assoc id (wisi-lex-id-alist wisi--lexer))))
	(insert " "))
      ))

(defun wisi-repair-error ()
  "Repair the current error."
  (interactive)
  (if (= 1 (length (wisi-parser-errors wisi--parser)))
      (progn
	(wisi-goto-error)
	(wisi-repair-error-1 (car (wisi-parser-errors wisi--parser))))
    (if (buffer-live-p wisi-error-buffer)
	(let ((err
	       (with-current-buffer wisi-error-buffer
		 ;; FIXME: ensure at beginning of error message line.
		 (get-text-property (point) 'wisi-error-data))))
	  (wisi-repair-error-1 err))
      (error "no current error found")
      )))

(defun wisi-repair-errors (&optional beg end)
  "Repair errors reported by last parse.
If non-nil, only repair errors in BEG END region."
  (interactive)
  (dolist (data (wisi-parser-errors wisi--parser))
    (when (or (null beg)
	      (and (wisi--error-pos data)
		   (<= beg (wisi--error-pos data))
		   (<= (wisi--error-pos data) end)))
      (wisi-repair-error-1 data))))

;;;; debugging

(defun wisi-debug-keys ()
  "Add debug key definitions to `global-map'."
  (interactive)
  (define-key global-map "\M-h" 'wisi-show-containing-or-previous-cache)
  (define-key global-map "\M-i" 'wisi-show-indent)
  (define-key global-map "\M-j" 'wisi-show-cache)
  )

(defun wisi-parse-buffer (&optional parse-action)
  (interactive)
  (when (< emacs-major-version 25) (syntax-propertize (point-max)))
  (unless parse-action (setq parse-action 'indent))
  (wisi-set-parse-try t parse-action)
  (move-marker (wisi-cache-max parse-action) (point-max));; force delete caches
  (wisi-invalidate-cache parse-action (point-min))

  (cl-ecase parse-action
    (face
     (with-silent-modifications
       (remove-text-properties
	(point-min) (point-max)
	(list
	 'font-lock-face nil
	 'fontified nil)))
     (wisi-validate-cache (point-max) t parse-action)
     (font-lock-ensure))

    (navigate
     (wisi-validate-cache (point-max) t parse-action))

    (indent
     (wisi-indent-region (point-min) (point-max)))
    ))

(defun wisi-time (func count &optional report-wait-time)
  "call FUNC COUNT times, show total time"
  (interactive "afunction \nncount ")

  (let ((start-time (float-time))
	(start-gcs gcs-done)
	(cum-wait-time 0.0)
        (i 0)
        diff-time
	diff-gcs)
    (while (not (eq (1+ count) (setq i (1+ i))))
      (save-excursion
        (funcall func))
      (when report-wait-time
	(setq cum-wait-time (+ cum-wait-time (wisi-process--parser-total-wait-time wisi--parser)))))
    (setq diff-time (- (float-time) start-time))
    (setq diff-gcs (- gcs-done start-gcs))
    (if report-wait-time
	(progn
	  (message "Total %f seconds, %d gcs; per iteration %f seconds %d gcs %d responses %f wait"
		   diff-time
		   diff-gcs
		   (/ diff-time count)
		   (/ (float diff-gcs) count)
		   (wisi-process--parser-response-count wisi--parser)
		   (/ cum-wait-time count)))

      (message "Total %f seconds, %d gcs; per iteration %f seconds %d gcs"
	       diff-time
	       diff-gcs
	       (/ diff-time count)
	       (/ (float diff-gcs) count))
      ))
  nil)

(defun wisi-time-indent-middle-line-cold-cache (count &optional report-wait-time)
  (goto-char (point-min))
  (forward-line (1- (/ (count-lines (point-min) (point-max)) 2)))
  (let ((cum-wait-time 0.0))
    (wisi-time
     (lambda ()
       (wisi-set-parse-try t 'indent)
       (move-marker (wisi-cache-max 'indent) (point-max));; force delete caches
       (wisi-invalidate-cache 'indent (point-min))
       (wisi-indent-line)
       (when (wisi-process--parser-p wisi--parser)
	 (setq cum-wait-time (+ cum-wait-time (wisi-process--parser-total-wait-time wisi--parser)))))
     count
     report-wait-time)
    ))

(defun wisi-time-indent-middle-line-warm-cache (count)
  (wisi-set-parse-try t 'indent)
  (move-marker (wisi-cache-max 'indent) (point-max));; force delete caches
  (wisi-invalidate-cache 'indent (point-min))
  (goto-char (point-min))
  (forward-line (/ (count-lines (point-min) (point-max)) 2))
  (wisi-indent-line)
  (wisi-time #'wisi-indent-line count))

(defun wisi-lex-buffer (&optional parse-action)
  ;; for timing the lexer; set indent so we get the slowest time
  (interactive)
  (when (< emacs-major-version 25) (syntax-propertize (point-max)))

  (let* ((wisi--parse-action (or parse-action 'indent))
	 (line-count (1+ (count-lines (point-min) (point-max))))
	 (wisi--indent
	  (make-wisi-ind
	   :line-begin (make-vector line-count 0)
	   :indent (make-vector line-count 0)
	   :last-line nil)))

    (wisi--set-line-begin)

    (goto-char (point-min))
    (while (forward-comment 1))
    (while (not (eq wisi-eoi-term (wisi-tok-token (wisi-forward-token)))))
    ))

(defun wisi-show-indent ()
  "Show indent cache for current line."
  (interactive)
  (message "%s" (get-text-property (1- (line-beginning-position)) 'wisi-indent)))

(defun wisi-show-cache ()
  "Show navigation and face caches at point."
  (interactive)
  (message "%s:%s:%s:%s"
	   (wisi-get-cache (point))
	   (get-text-property (point) 'wisi-face)
	   (get-text-property (point) 'face)
	   (get-text-property (point) 'font-lock-face)
	   ))

(defun wisi-show-token ()
  "Move forward across one keyword, show token."
  (interactive)
  (let* ((wisi--parse-action nil)
	 (token (wisi-forward-token)))
    (message "%s" token)))

(defun wisi-show-containing-or-previous-cache ()
  (interactive)
  (let ((cache (wisi-get-cache (point))))
    (if cache
	(message "containing %s" (wisi-goto-containing cache t))
      (message "previous %s" (wisi-backward-cache)))
    ))

(defun wisi-show-cache-max (action)
  (push-mark)
  (goto-char (wisi-cache-max action)))

;;;;; setup

(cl-defun wisi-setup (&key indent-calculate post-indent-fail parser lexer)
  "Set up a buffer for parsing files with wisi."
  (when wisi--parser
    (wisi-kill-parser))

  (setq wisi--parser parser)
  (setq wisi--lexer lexer)

  (setq wisi--cache-max
	(list
	 (cons 'face (copy-marker (point-min)))
	 (cons 'navigate (copy-marker (point-min)))
	 (cons 'indent (copy-marker (point-min)))))

  (setq wisi--parse-try
	(list
	 (cons 'face t)
	 (cons 'navigate t)
	 (cons 'indent t)))

  ;; file local variables may have added opentoken, gnatprep
  (setq wisi-indent-calculate-functions (append wisi-indent-calculate-functions indent-calculate))
  (set (make-local-variable 'indent-line-function) #'wisi-indent-line)
  (set (make-local-variable 'indent-region-function) #'wisi-indent-region)
  (set (make-local-variable 'forward-sexp-function) #'wisi-forward-sexp)

  (setq wisi-post-indent-fail-hook post-indent-fail)
  (setq wisi-indent-failed nil)

  (add-hook 'before-change-functions #'wisi-before-change 'append t)
  (add-hook 'after-change-functions #'wisi-after-change nil t)
  (setq wisi--change-end (copy-marker (point-min) t))

  ;; See comments above on syntax-propertize.
  (when (< emacs-major-version 25) (syntax-propertize (point-max)))

  (add-hook 'hack-local-variables-hook 'wisi-post-local-vars nil t)
  )

(defun wisi-post-local-vars ()
  "See wisi-setup."
  (setq hack-local-variables-hook (delq 'wisi-post-local-vars hack-local-variables-hook))

  (unless wisi-disable-face
    (jit-lock-register #'wisi-fontify-region)))


(provide 'wisi)
;;; wisi.el ends here
