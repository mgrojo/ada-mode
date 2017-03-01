;;; wisi.el --- Utilities for implementing an indentation/navigation engine using a generalized LALR parser -*- lexical-binding:t -*-
;;
;; Copyright (C) 2012 - 2017  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
;; Maintainer: Stephen Leake <stephen_leake@member.fsf.org>
;; Keywords: parser
;;  indentation
;;  navigation
;; Version: 1.1.4
;; package-requires: ((cl-lib "0.4") (emacs "24.2"))
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
;; 'wisi' was originally short for "wisent indentation engine", but
;; now is just a name.
;;
;; The approach to indenting a given token is to parse the buffer,
;; computing a delta indent at each parse action.
;;
;; The parser actions may also cache face and navigation information
;; as text properties of tokens in statements.
;;
;; The three reasons to run the parser (indent, face, navigate) occur
;; at different times (user indent, font-lock, user navigate), so only
;; the relevant parser actions are run.
;;
;; Since we have a cache (the text properties), we need to consider
;; when to invalidate it.  Ideally, we invalidate only when a change to
;; the buffer would change the result of a parse that crosses that
;; change, or starts after that change.  Changes in whitespace
;; (indentation and newlines) do not affect an Ada parse.  Other
;; languages are sensitive to newlines (Bash for example) or
;; indentation (Python).  Adding comments does not change a parse,
;; unless code is commented out.  For now we invalidate the cache after
;; the edit point if the change involves anything other than
;; whitespace.
;;
;;; Handling parse errors:
;;
;; When a parse fails, the cache information before the failure point
;; is only partly correct, and there is no cache information after the
;; failure point.
;;
;; However, in the case where a parse previously succeeded, and the
;; current parse fails due to editing, we keep the preceding cache
;; information by setting wisi-cache-max to the edit point in
;; wisi-before change; the parser does not apply actions before that
;; point.
;;
;; This allows navigation in the text preceding the edit point, and
;; saves some time.
;;
;;;; grammar compiler and parser
;;
;; Since we are using a generalized LALR(1) parser, we cannot use any
;; of the wisent grammar functions.  We use OpenToken wisi-generate
;; to compile BNF to Elisp source (similar to
;; semantic-grammar-create-package), and wisi-compile-grammar to
;; compile that to the parser table.
;;
;; Semantic provides a complex lexer, more complicated than we need
;; for indentation.  So we use the elisp lexer, which consists of
;; `forward-comment', `skip-syntax-forward', and `scan-sexp'.  We wrap
;; that in functions that return tokens in the form wisi-parse
;; expects.
;;
;;;; lexer
;;
;; The lexer is `wisi-forward-token'. It relies on syntax properties,
;; so (in Emacs < 25) syntax-propertize must be called on the text to
;; be lexed before wisi-forward-token is called. In general, it is
;; hard to determine an appropriate end-point for syntax-propertize,
;; other than point-max. So we call (syntax-propertize point-max) in
;; wisi-setup, and also call syntax-propertize in wisi-after-change.
;;
;;;;;

;;; Code:

(require 'cl-lib)
(require 'wisi-parse)

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

;;;; lexer

(cl-defstruct wisi-lex
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

(defconst wisi-eoi-term '$EOI
  "End Of Input token.")

(defvar-local wisi--lexer nil
  "A `wisi-lex' struct defining the lexer for the current buffer.")

(cl-defstruct wisi-ind
  ;; data used while running parser for indent.
  region ;; region being indented.
  indent ;; array of indentation for lines in region
  first-line ;; line number of first line in region (for computing index into indent)
  line-begin ;; array of beginning-of-line positions in buffer
  last-line ;; index into line-begin of last line where first token found
  )

(defvar wisi--indent
  ;; not buffer-local; only let-bound in wisi-indent-region
  "A `wisi-ind' struct holding current indent information.")

;; struct wisi-tok defined in wisi-parse.el

(defvar wisi--parse-action nil
  ;; not buffer-local; only let-bound in wisi-indent-region, FIXME: when is it set to 'face, 'navigate?
  "Reason current parse is begin run; one of
{indent, face, navigate}.")

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
  "Move point forward across one token, skipping leading whitespace and comments.
Return the corresponding token as a `wisi-tok'.
If at end of buffer, returns `wisi-eoi-term'."
  (forward-comment (point-max))
  ;; skips leading whitespace, comment, trailing whitespace.

  (let ((start (point))
	;; (info "(elisp)Syntax Table Internals" "*info elisp syntax*")
	(syntax (syntax-class (syntax-after (point))))
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

     (t ;; assuming word or symbol syntax; includes numbers
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
     );; cond

    (unless token-id
      (signal 'wisi-parse-error
	      (wisi-error-msg "unrecognized token '%s'" (buffer-substring-no-properties start (point)))))

    (when (and (not (eobp))
	       (eq wisi--parse-action 'indent))
      (if (wisi-ind-last-line wisi--indent)
	  (progn
	    (setq line (wisi-ind-last-line wisi--indent));; also index into wisi-ind-line-begin
	    (if (>= start (aref (wisi-ind-line-begin wisi--indent) line))
		;; first token on next non-blank line
		(progn
		  ;; skip blank lines
		  (while (and (< (1+ line) (length (wisi-ind-line-begin wisi--indent)))
			      (>= start (aref (wisi-ind-line-begin wisi--indent) (1+ line))))
		    (setq line (1+ line)))
		  (setq line (1+ line))
		  (setf (wisi-ind-last-line wisi--indent) line))
	      ;; other token on line
	      (setq line nil))
	    )
	;; First token on first non-comment line
	(setq line (line-number-at-pos start))
	(setf (wisi-ind-last-line wisi--indent) line)
	))

    (make-wisi-tok
     :token token-id
     :region (cons start (point))
     :line line)
    ))

(defun wisi-backward-token ()
  "Move point backward across one token, skipping whitespace and comments.
Does _not_ handle numbers with wisi-number-p; just sees
lower-level syntax.  Return (nil start . end) - same structure as
wisi-forward-token, but does not look up symbol."
  (forward-comment (- (point)))
  ;; skips leading whitespace, comment, trailing whitespace.

  ;; (info "(elisp)Syntax Table Internals" "*info elisp syntax*")
  (let ((end (point))
	(syntax (syntax-class (syntax-after (1- (point))))))
    (cond
     ((bobp) nil)

     ((eq syntax 1)
      ;; punctuation. Find the longest matching string in wisi-punctuation-table
      (backward-char 1)
      (let ((next-point (point))
	    temp-text done)
	(while (not done)
	  (setq temp-text (buffer-substring-no-properties (point) end))
	  (when (car (rassoc temp-text (wisi-lex-punctuation-table wisi--lexer)))
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
      (backward-char 1))

     ((eq syntax 7)
      ;; a string quote. we assume we are after the end quote, not the start quote
      (let ((forward-sexp-function nil))
	(forward-sexp -1)))

     (t ;; assuming word or symbol syntax
      (if (zerop (skip-syntax-backward "."))
	  (skip-syntax-backward "w_'")))
     )
    (cons nil (cons (point) end))
    ))

;;;; token info cache
;;
;; the cache stores the results of parsing as text properties on
;; keywords, for use by the indention and motion engines.

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

  class
  ;; arbitrary lisp symbol, used for indentation and navigation.
  ;; some classes are defined by wisi:
  ;;
  ;; 'block-middle - a block keyword (ie: if then else end), not at the start of a statement
  ;;
  ;; 'block-start - a block keyword at the start of a statement
  ;;
  ;; 'statement-start - the start of a statement
  ;;
  ;; 'open-paren
  ;;
  ;; others are language-specific

  containing
  ;; Marker at the containing keyword for this token.
  ;; A containing keyword is an indent point; the start of a
  ;; statement, or 'begin', 'then' or 'else' for a block of
  ;; statements, etc.
  ;; nil only for first token in buffer

  prev ;; marker at previous motion token in statement; nil if none
  next ;; marker at next motion token in statement; nil if none
  end  ;; marker at token at end of current statement
  )

(defvar-local wisi-parse-table nil)

(defvar-local wisi-parse-failed nil
  "Non-nil when a recent parse has failed - cleared when parse succeeds.")

(defvar-local wisi-parse-try nil
  "Non-nil when parse is needed - cleared when parse succeeds.")

(defvar-local wisi-end-caches nil
  "List of buffer positions of caches in current statement that need wisi-cache-end set.")

(defun wisi-delete-cache (after)
  (with-silent-modifications
    (remove-text-properties after (point-max) '(wisi-cache nil))
    ;; We don't remove 'font-lock-face; that's annoying to the user,
    ;; since they won't be restored until a parse for some other
    ;; reason, and they are likely to be right anyway.
    ))

(defun wisi-invalidate-cache(&optional after)
  "Invalidate parsing caches for the current buffer from AFTER to end of buffer."
  (interactive)
  (setq after
	(if (not after)
	    (point-min)
	  (save-excursion
	    (goto-char after)
	    (line-beginning-position))))
  (when (> wisi-debug 0) (message "wisi-invalidate %s:%d" (current-buffer) after))
  (move-marker wisi-cache-max after)
  (setq wisi-parse-try t)
  (wisi-delete-cache after)
  )

;; wisi--change-* keep track of buffer modifications.
;; If wisi--change-end comes before wisi--change-beg, it means there were
;; no modifications.
(defvar-local wisi--change-beg most-positive-fixnum
  "First position where a change may have taken place.")

(defvar-local wisi--change-end nil
  "Marker pointing to the last position where a change may have taken place.")

;; To see the effect of wisi-before-change, you need:
;; (global-font-lock-mode 0)
;; (setq jit-lock-functions nil)
;;
;; otherwise jit-lock runs and overrides it

(defun wisi-before-change (begin end)
  "For `before-change-functions'."
  ;; begin . end is range of text being deleted
  (setq wisi--change-beg (min wisi--change-beg begin))
  (when (> end wisi--change-end)
    (move-marker wisi--change-end end)))

(defun wisi--after-change (begin end)
  "Update wisi text properties for changes in region BEG END."
  ;; (syntax-ppss-flush-cache begin) is in before-change-functions

  ;; see comments above on "lexer" re syntax-propertize
  (when (< emacs-major-version 25) (syntax-propertize end))

  ;; Remove caches on inserted text, which could have caches from
  ;; before the failed parse (or another buffer), and are in any case
  ;; invalid. No point in removing 'fontified; that's handled by
  ;; jit-lock.

  (with-silent-modifications
    (remove-text-properties begin end '(wisi-cache nil font-lock-face nil)))

  ;; Also remove grammar face from word(s) containing change region;
  ;; might be changing to/from a keyword. See
  ;; test/ada_mode-interactive_common.adb Obj_1
  (save-excursion
    (let (need-invalidate begin-state end-state word-end)
      (when (> end begin)
	(setq begin-state (syntax-ppss begin))
	(setq end-state (syntax-ppss end))
	;; syntax-ppss has moved point to "end".

	;; extend fontification over new text,
	(skip-syntax-forward "w_")
	(setq word-end (point))
	(goto-char begin)
	(skip-syntax-backward "w_")
	(with-silent-modifications
	  (remove-text-properties (point) word-end '(font-lock-face nil fontified nil))))

      (if (<= wisi-cache-max begin)
	  ;; Change is in unvalidated region
	  (when wisi-parse-failed
	    ;; The parse was failing, probably due to bad syntax; this
	    ;; change may have fixed it, so try reparse.
	    (setq wisi-parse-try t))

	;; Change is in validated region
	;; (info "(elisp)Parser State")
	(cond
	 ((= end begin)
	  (setq need-invalidate nil))

	 ((and
	   (nth 3 begin-state); in string
	   (nth 3 end-state))
	  ;; no easy way to tell if there is intervening non-string
          ;; FIXME: Of course there is, just test if
          ;;    (= (nth 8 begin-state) (nth 8 end-state))
	  (setq need-invalidate nil))

	 ((and
	   (nth 4 begin-state)
	   (nth 4 end-state)); in comment
	  ;; no easy way to detect intervening non-comment
          ;; FIXME: Of course there is, just test if
          ;;    (= (nth 8 begin-state) (nth 8 end-state))
	  (setq need-invalidate nil)
	  ;; no caches to remove
	  )

	 ;; Adding whitespace generally does not require parse, but in
	 ;; the middle of word it does; check that there was
	 ;; whitespace on at least one side of the inserted text.
	 ;;
	 ;; We are not in a comment (checked above), so treat
	 ;; comment end as whitespace in case it is newline
	 ((and
	   (or
	    (memq (car (syntax-after (1- begin))) '(0 12)); whitespace, comment end
	    (memq (car (syntax-after end)) '(0 12)))
	   (progn
	    (goto-char begin)
	    (skip-syntax-forward " >" end)
	    (eq (point) end)))
	  (setq need-invalidate nil))

	 (t
	  (setq need-invalidate
		(progn
		  (goto-char begin)
		  ;; note that because of the checks above, this never
		  ;; triggers a parse, so it's fast
		  (wisi-goto-statement-start)
		  (point))))
	 )

	(when need-invalidate
	    (wisi-invalidate-cache need-invalidate))
	))
    ))

(defun wisi-get-cache (pos)
  "Return `wisi-cache' struct from the `wisi-cache' text property at POS.
If accessing cache at a marker for a token as set by `wisi-cache-tokens', POS must be (1- mark)."
  (get-text-property pos 'wisi-cache))

(defvar-local wisi-parse-error-msg nil)

(defun wisi-goto-error ()
  "Move point to position in last error message (if any)."
  (when (and wisi-parse-error-msg
	     (string-match ":\\([0-9]+\\):\\([0-9]+\\):" wisi-parse-error-msg))
    (let ((line (string-to-number (match-string 1 wisi-parse-error-msg)))
	  (col (string-to-number (match-string 2 wisi-parse-error-msg))))
      (push-mark)
      (goto-char (point-min))
      (forward-line (1- line))
      (forward-char col))))

(defun wisi-show-parse-error ()
  "Show last wisi-parse error."
  (interactive)
  (cond
   (wisi-parse-failed
    (wisi-goto-error)
    (message wisi-parse-error-msg))

   (wisi-parse-try
    (message "need parse"))

   (t
    (message "parse succeeded"))
   ))

(defvar-local wisi-class-list nil
  "list of valid token classes; checked in wisi-statement-action.")

(defun wisi-run-parse ()
  "Run the parser."
  (let ((msg (when (> wisi-debug 0)
	       (format "wisi: parsing %s:%d ..."
		       (buffer-name) (line-number-at-pos (point))))))
    (when (> wisi-debug 0)
      (message msg))

    (setq wisi-parse-error-msg nil)

    (condition-case-unless-debug err
	(save-excursion
	  (wisi-parse wisi-parse-table #'wisi-forward-token)
	  (setq wisi-parse-failed nil)
	  (cl-ecase wisi--parse-action
	    ((face navigate)
	     (move-marker wisi-cache-max (point)))
	    (indent nil)
	    ))
      (wisi-parse-error
       (cl-ecase wisi--parse-action
	 ((face navigate)
	  ;; delete caches past wisi-cache-max added by failed parse
	  (wisi-delete-cache wisi-cache-max))
	 (indent nil))
       (setq wisi-parse-failed t)
       (setq wisi-parse-error-msg (cdr err)))
      )

    (if wisi-parse-error-msg
	;; error
	(when (> wisi-debug 0)
	  (message "%s error" msg)
	  (wisi-goto-error)
	  (error wisi-parse-error-msg)))

      ;; no error
      (when (> wisi-debug 0)
	(message "%s done" msg))
      ))

(defun wisi-validate-cache (pos &optional error-on-fail)
  "Ensure cached data for `wisi--parse-action' is valid at least up to POS in current buffer."
  (when (< wisi--change-beg wisi--change-end)
    ;; There have been buffer changes since last parse. so make sure
    ;; we update the existing parsing data first.
    (let ((beg wisi--change-beg)
          (end wisi--change-end))
      (setq wisi--change-beg most-positive-fixnum)
      (move-marker wisi--change-end (point-min))
      (wisi--after-change beg end)))

  ;; Now we can rely on wisi-cache-max.

  ;; If wisi-cache-max = pos, then there is no cache at pos; need parse
  (when (and wisi-parse-try
	     (<= wisi-cache-max pos))

    ;; Don't keep retrying failed parse until text changes again.
    (setq wisi-parse-try nil)
    (setq wisi-end-caches nil)

    (wisi-run-parse)

    (when (and error-on-fail (not (>= wisi-cache-max pos)))
      (error "parse failed"))
    ))

(defun wisi-fontify-region (_begin end)
  "For `jit-lock-functions'."
  (when (< (point-max) wisi-size-threshold)
    (let ((wisi--parse-action 'face))
      (wisi-validate-cache end))))

(defun wisi-get-containing-cache (cache)
  "Return cache from (wisi-cache-containing CACHE)."
  (let ((containing (wisi-cache-containing cache)))
    (and containing
	 (wisi-get-cache (1- containing)))))

(defun wisi-cache-region (cache)
  "Return region designated by cache.
Point must be at cache."
  (cons (point) (+ (point) (wisi-cache-last cache))))

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

(defvar wisi-tokens nil)
(defvar $nterm nil)
;; keep byte-compiler happy; `wisi-tokens' and `$nterm' are bound in
;; action created by wisi-semantic-action, and in module parser.
;; FIXME: $nterm should have wisi- prefix

(defun wisi-statement-action (pairs)
  "Cache navigation information in text properties of tokens.
Intended as a grammar non-terminal action.

PAIRS is a vector of the form [TOKEN-NUMBER CLASS TOKEN-NUMBER
CLASS ...] where TOKEN-NUMBER is the (1 indexed) token number in
the production, CLASS is the wisi class of that token. Use in a
grammar action as:
  (wisi-statement-action [1 \\='statement-start 7 \\='statement-end])"
  (when (eq wisi--parse-action 'navigate)
    (save-excursion
      (let ((first-item t)
	    first-keyword-mark
	    (override-start nil)
	    (i 0))
	(while (< i (length pairs))
	  (let* ((number (1- (aref pairs i)))
		 (region (cdr (aref wisi-tokens number)));; wisi-tokens is let-bound in wisi-parse-reduce
		 (token (car (aref wisi-tokens number)))
		 (class (aref pairs (setq i (1+ i))))
		 (mark
		  ;; Marker one char into token, so indent-line-to
		  ;; inserts space before the mark, not after
		  (when region (copy-marker (1+ (car region)))))
		 cache)

	    (setq i (1+ i))

	    (unless (memq class wisi-class-list)
	      (error "%s not in wisi-class-list" class))

	    (if region
		(progn
		  (if (setq cache (wisi-get-cache (car region)))
		      ;; We are processing a previously set non-terminal; ie generic_formal_part in
		      ;;
		      ;; generic_package_declaration : generic_formal_part package_specification SEMICOLON
		      ;;    (wisi-statement-action 1 'block-start 2 'block-middle 3 'statement-end)
		      ;;
		      ;; or simple_statement in
		      ;;
		      ;; statement : label_opt simple_statement
		      ;;
		      ;; override nonterm, class, containing
		      ;; set end only if not set yet (due to failed parse)
		      (progn
			(cl-case (wisi-cache-class cache)
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
			(setf (wisi-cache-nonterm cache) $nterm)
			(setf (wisi-cache-containing cache) first-keyword-mark)
			(unless (wisi-cache-end cache)
			  (if wisi-end-caches
			      (push (car region) wisi-end-caches)
			    (setq wisi-end-caches (list (car region)))
			    ))
			)

		    ;; else create new cache
		    (with-silent-modifications
		      (put-text-property
		       (car region)
		       (1+ (car region))
		       'wisi-cache
		       (wisi-cache-create
			:nonterm    $nterm
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
			      (memq class '(block-start statement-start)))
		      (setq override-start nil)
		      (setq first-keyword-mark mark)))

		  (when (eq class 'statement-end)
		    (wisi--set-end (1- first-keyword-mark) (copy-marker (1+ (car region)))))
		  )

	      ;; region is nil when a production is empty; if the first
	      ;; token is a start, override the class on the next token.
	      (when (and first-item
			 (memq class '(block-middle block-start statement-start)))
		(setq override-start class)))
	    ))
	))))

(defun wisi--match-class-token (cache class-tokens)
  "Return t if CACHE matches CLASS-TOKENS.
CLASS-TOKENS is a vector [number class token_id class token_id ...].
number is ignored."
  (let ((i 1)
	(done nil)
	(result nil)
	class token)
    (while (and (not done)
		(< i (length class-tokens)))
      (setq class (aref class-tokens i))
      (setq token (aref class-tokens (setq i (1+ i))))
      (setq i (1+ i))
      (when (and (eq class (wisi-cache-class cache))
		 (eq token (wisi-cache-token cache)))
	(setq result t
	      done t))
      )
    result))

(defun wisi-motion-action (token-numbers)
  "Set prev/next marks in all tokens given by TOKEN-NUMBERS.
TOKEN-NUMBERS is a vector with each element one of:

number: the token number; mark that token

vector [number class token_id]:
vector [number class token_id class token_id ...]:
   mark all tokens in number nonterminal matching (class token_id) with nil prev/next."
  (when (eq wisi--parse-action 'navigate)
    (save-excursion
      (let (prev-keyword-mark
	    prev-cache
	    cache
	    mark
	    (i 0))
	(while (< i (length token-numbers))
	  (let ((token-number (aref token-numbers i))
		region)
	    (setq i (1+ i))
	    (cond
	     ((numberp token-number)
	      (setq region (cdr (aref wisi-tokens (1- token-number))))
	      (when region
		(setq cache (wisi-get-cache (car region)))
		(setq mark (copy-marker (1+ (car region))))

		(when (and prev-keyword-mark
			   cache
			   (null (wisi-cache-prev cache)))
		  (setf (wisi-cache-prev cache) prev-keyword-mark)
		  (setf (wisi-cache-next prev-cache) mark))

		(setq prev-keyword-mark mark)
		(setq prev-cache cache)
		))

	     ((vectorp token-number)
	      ;; token-number may contain 0, 1, or more 'class token_id' pairs
	      ;; the corresponding region may be empty
	      ;; there must have been a prev keyword
	      (setq region (cdr (aref wisi-tokens (1- (aref token-number 0)))))
	      (when region ;; not an empty token
		;; We must search for all targets at the same time, to
		;; get the motion order right.
		(goto-char (car region))
		(setq cache (or (wisi-get-cache (point))
				(wisi-forward-cache)))
		(while (< (point) (cdr region))
		  (when (wisi--match-class-token cache token-number)
		    (when (null (wisi-cache-prev cache))
		      (setf (wisi-cache-prev cache) prev-keyword-mark))
		    (when (null (wisi-cache-next cache))
		      (setq mark (copy-marker (1+ (point))))
		      (setf (wisi-cache-next prev-cache) mark)
		      (setq prev-keyword-mark mark)
		      (setq prev-cache cache)))

		  (setq cache (wisi-forward-cache))
		  )))

	     (t
	      (error "unexpected token-number %s" token-number))
	     )

	    ))
	))))

(defun wisi-extend-action (first last)
  "Extend text of cache at token FIRST to cover all tokens thru LAST."
  (when (eq wisi--parse-action 'face)
    (let* ((first-region (cdr (aref wisi-tokens (1- first))));; wisi-tokens is let-bound in wisi-parse-reduce
	   (last-region (cdr (aref wisi-tokens (1- last))))
	   cache)

      (when first-region
	(setq cache (wisi-get-cache (car first-region)))
	(setf (wisi-cache-last cache) (- (cdr last-region) (car first-region)))
	)
      )))

(defun wisi--face-action-1 (face region &optional override-no-error)
  "Apply FACE to REGION.
If OVERRIDE-NO-ERROR is non-nil, don't report an error for overriding an existing face."
  (when region
    ;; We allow overriding a face property, because we don't want to
    ;; delete them in wisi-invalidate (see comments there). On the
    ;; other hand, it can be an error, so keep this debug
    ;; code. However, to validly report errors, note that
    ;; font-lock-face properties must be removed first, or the buffer
    ;; must be fresh (never parsed), and wisi-debug must be > 1.
    ;;
    ;; Grammar sets override-no-error when a higher-level production might
    ;; override a face in a lower-level production.
    (when (> wisi-debug 1)
      (let ((cur-face (get-text-property (car region) 'font-lock-face)))
	(when cur-face
	  (unless override-no-error
	    (message "%s:%d overriding face %s with %s on '%s'"
		     (buffer-file-name)
		     (line-number-at-pos (car region))
		     face
		     cur-face
		     (buffer-substring-no-properties (car region) (cdr region))))

	  )))
    (with-silent-modifications
      (add-text-properties
       (car region) (cdr region)
       (list
	'font-lock-face face
	'fontified t)))
    ))

(defun wisi-face-action (pairs &optional no-override)
  "Cache face information in text properties of tokens.
Intended as a grammar non-terminal action.

PAIRS is a vector of the form [token-number face token-number face ...]
token-number may be an integer, or a vector [integer token_id token_id ...]

For an integer token-number, apply face to the first cached token
in the range covered by wisi-tokens[token-number]. If there are
no cached tokens, apply face to entire wisi-tokens[token-number]
region.

For a vector token-number, apply face to the first cached token
in the range matching one of token_id covered by
wisi-tokens[token-number].

If NO-OVERRIDE is non-nil, don't override existing face."
  (when (eq wisi--parse-action 'face)
    (let (number region face (tokens nil) cache (i 0) (j 1))
      (while (< i (length pairs))
	(setq number (aref pairs i))
	(setq face (aref pairs (setq i (1+ i))))
	(cond
	 ((integerp number)
	  (setq region (cdr (aref wisi-tokens (1- number))));; wisi-tokens is let-bound in wisi-parse-reduce
	  (when region
	    (save-excursion
	      (goto-char (car region))
	      (setq cache (or (wisi-get-cache (point))
			      (wisi-forward-cache)))
	      (if (< (point) (cdr region))
		  (when cache
		    (wisi--face-action-1 face (wisi-cache-region cache) no-override))

		;; no caches in region; just apply face to region
		(wisi--face-action-1 face region no-override))
	      )))

	 ((vectorp number)
	  (setq region (cdr (aref wisi-tokens (1- (aref number 0)))))
	  (when region
	    (while (< j (length number))
	      (setq tokens (cons (aref number j) tokens))
	      (setq j (1+ j)))
	    (save-excursion
	      (goto-char (car region))
	      (setq cache (wisi-forward-find-token tokens (cdr region) t))
	      ;; might be looking for IDENTIFIER in name, but only have "*".
	      (when cache
		(wisi--face-action-1 face (wisi-cache-region cache) no-override))
	      )))
	 )
	(setq i (1+ i))

	))))

(defun wisi-face-list-action (pairs &optional no-override)
  "Cache face information in text properties of tokens.
Intended as a grammar non-terminal action.

PAIRS is a vector of the form [token-number face token-number face ...]
token-number is an integer. Apply face to all cached tokens
in the range covered by wisi-tokens[token-number].

If NO-OVERRIDE is non-nil, don't override existing face."
  (when (eq wisi--parse-action 'face)
    (let (number region face cache (i 0))
      (while (< i (length pairs))
	(setq number (aref pairs i))
	(setq face (aref pairs (setq i (1+ i))))
	(setq region (cdr (aref wisi-tokens (1- number))));; wisi-tokens is let-bound in wisi-parse-reduce
	(when region
	  (save-excursion
	    (goto-char (car region))
	    (setq cache (or (wisi-get-cache (point))
			    (wisi-forward-cache)))
	    (while (<= (point) (cdr region))
	      (when cache
		(wisi--face-action-1 face (wisi-cache-region cache) no-override))
	      (setq cache (wisi-forward-cache))
	      )))

	(setq i (1+ i))

	))))

(defun wisi--indent-token (tok delta)
  "Add DELTA to all indents in TOK region."
  (let ((end (cdr (wisi-tok-region tok)))
	(i (1- (wisi-tok-line tok)));; index to wisi-ind-line-begin
	(j (- (wisi-tok-line tok) (wisi-ind-first-line wisi--indent)))
	(max-j (length (wisi-ind-indent wisi--indent)))
	)
    (while (< (aref (wisi-ind-line-begin wisi--indent) i) end)
      (when (and (<= 0 j)
		 (< j max-j))
	(aset (wisi-ind-indent wisi--indent) j (+ delta (aref (wisi-ind-indent wisi--indent) j))))
      (setq i (1+ i))
      (setq j (1+ j))
      )))

(defun wisi-token-column (token-number)
  "Return column of start of token TOKEN-NUMBER in `wisi-tokens'.
For use in grammar indent actions."
  (goto-char (car (wisi-tok-region (aref wisi-tokens (1- token-number)))))
  (current-column))

(defun wisi-indent-comments-action (token-number delta)
  "Indent comment lines before token TOKEN-NUMBER by DELTA."
  (let* ((tok (aref wisi-tokens (1- token-number)))
	 (line (wisi-tok-line tok))
	 (i (when line (1- line)))
	 (j (when line (- line (wisi-ind-first-line wisi--indent))))
	 (max-j (length (wisi-ind-indent wisi--indent)))
	 start)
    (when line
      (save-excursion
	(goto-char (car (wisi-tok-region tok)))
	(forward-comment (- (point)))
	(when (> (point) (line-beginning-position)) (forward-line 1))
	(setq start (point)))

      (while (< start (aref (wisi-ind-line-begin wisi--indent) i))

	(when (and (>= j 0)
		   (< j max-j))
	  (aset (wisi-ind-indent wisi--indent) j (+ delta (aref (wisi-ind-indent wisi--indent) j))))
	(setq i (1- i))
	(setq j (1- j)))
      )))

(defun wisi-indent-action (deltas)
  "Accumulate `wisi--indents' from DELTAS."
    (dotimes (i (length wisi-tokens))
      (let ((tok (aref wisi-tokens i))
	    (delta (aref deltas i)))
	(cond
	 ((symbolp delta)
	  (setq delta (symbol-value delta)))
	 ((listp delta)
	  (save-excursion
	    (goto-char (car (wisi-tok-region tok)))
	    (setq delta (eval delta))))
	 (t
	  ;; assume delta is an integer
	  nil))

	(when (wisi-tok-line tok)
	  (wisi--indent-token tok delta))
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
If point is at a matching token, return that token.
TOKEN may be a list; stop on any cache that has a member of the list.
Return cache, or nil if at end of buffer.
If LIMIT (a buffer position) is reached, then if NOERROR is nil, throw an
error, if non-nil, return nil."
  (let ((token-list (cond
		     ((listp token) token)
		     (t (list token))))
	(cache (wisi-get-cache (point)))
	(done nil))
    (while (not (or done
		    (and cache
			 (memq (wisi-cache-token cache) token-list))))
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
  (goto-char (1- (wisi-cache-next cache)))
  (wisi-get-cache (point))
  )

(defun wisi-forward-statement-keyword ()
  "If not at a cached token, move forward to next
cache. Otherwise move to cache-next, or next cache if nil.
Return cache found."
  (wisi-validate-cache (point-max) t) ;; ensure there is a next cache to move to
  (let ((cache (wisi-get-cache (point))))
    (if cache
	(let ((next (wisi-cache-next cache)))
	  (if next
	      (goto-char (1- next))
	    (wisi-forward-token)
	    (wisi-forward-cache)))
      (wisi-forward-cache))
    )
  (wisi-get-cache (point))
  )

(defun wisi-backward-statement-keyword ()
  "If not at a cached token, move backward to prev
cache. Otherwise move to cache-prev, or prev cache if nil."
  (wisi-validate-cache (point) t)
  (let ((cache (wisi-get-cache (point))))
    (if cache
	(let ((prev (wisi-cache-prev cache)))
	  (if prev
	      (goto-char (1- prev))
	    (wisi-backward-cache)))
      (wisi-backward-cache))
  ))

(defun wisi-goto-containing (cache &optional error)
  "Move point to containing token for CACHE, return cache at that point.
If ERROR, throw error when CACHE has no container; else return nil."
  (cond
   ((markerp (wisi-cache-containing cache))
    (goto-char (1- (wisi-cache-containing cache)))
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
  "Move point to containing ancestor of CACHE that has class block-start or statement-start.
Return start cache."
  (when
    ;; cache nil at bob, or on cache in partially parsed statement
    (while (and cache
		(not (memq (wisi-cache-class cache) '(block-start statement-start))))
      (setq cache (wisi-goto-containing cache)))
    )
  cache)

(defun wisi-goto-end-1 (cache)
  (goto-char (1- (wisi-cache-end cache))))

(defun wisi-goto-statement-start ()
  "Move point to token at start of statement point is in or after.
Return start cache."
  (interactive)
  (wisi-validate-cache (point) t)
  (let ((cache (wisi-get-cache (point))))
    (unless cache
      (setq cache (wisi-backward-cache)))
    (wisi-goto-start cache)))

(defun wisi-goto-statement-end ()
  "Move point to token at end of statement point is in or before."
  (interactive)
  (wisi-validate-cache (point) t)
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
  (goto-char (1- (wisi-cache-next cache)))
  (wisi-get-cache (point)))

(defun wisi-prev-statement-cache (cache)
  "Move point to CACHE-next, return cache; error if nil."
  (when (not (markerp (wisi-cache-prev cache)))
    (error "no prev statement cache"))
  (goto-char (1- (wisi-cache-prev cache)))
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

(defun wisi-indent-current (offset)
  "Return indentation OFFSET relative to indentation of current line."
  (+ (current-indentation) offset)
  )

(defun wisi-indent-paren (offset)
  "Return indentation OFFSET relative to preceding open paren."
  (save-excursion
    (goto-char (nth 1 (syntax-ppss)))
    (+ (current-column) offset)))

(defun wisi-indent-start (offset cache)
  "Return indentation of OFFSET relative to containing ancestor
of CACHE with class statement-start or block-start."
  (wisi-goto-start cache)
  (+ (current-indentation) offset))

(defun wisi-indent-statement ()
  "Indent region given by `wisi-goto-start' on cache at or before point, then wisi-cache-end."
  (wisi-validate-cache (point) t)

  (save-excursion
    (let ((cache (or (wisi-get-cache (point))
		     (wisi-backward-cache))))
      (when cache
	;; can be nil if in header comment
	(let ((start (progn (wisi-goto-start cache) (point)))
	      (end (progn
		     (when (wisi-cache-end cache)
		       ;; nil when cache is statement-end
		       (goto-char (1- (wisi-cache-end cache))))
		     (point))))
	  (indent-region start end)
	  ))
      )))

(defvar-local wisi-indent-calculate-functions nil
  "Functions to compute indentation special cases, including
comments.  Called with point at current indentation of a line;
return indentation column, or nil if function does not know how
to indent that line. Run after parser indentation, so other lines
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
    (while (< (point) end)
      (forward-line 1)
      (indent-line-to col))))

(defun wisi--set-line-begin ()
  "Set line-begin field of `wisi--indent'."
  (goto-char (point-min))

  (dotimes (i (length (wisi-ind-line-begin wisi--indent)))
    (aset (wisi-ind-line-begin wisi--indent) i (copy-marker (point)))
    (forward-line 1)))

(defun wisi-indent-region (begin end)
  "For `indent-region-function', using the wisi indentation engine."
  (let* ((wisi--parse-action 'indent)
	 (wisi-cache-max (copy-marker (point-min)))
	 (wisi--indent
	  (make-wisi-ind
	   :region (cons begin end)
	   :first-line (line-number-at-pos begin)
	   :line-begin (make-vector (1+ (count-lines (point-min) (point-max))) 0)
	   :indent (make-vector (count-lines begin end) 0)
	   :last-line nil))
	 indent)

    (wisi--set-line-begin)
    (wisi-run-parse)

    (if wisi-parse-failed
	(progn
	  (setq wisi-indent-failed t)
	  (setq indent (funcall wisi-indent-region-fallback begin end)))

      ;; parse succeeded
      ;;
      ;; apply indent action results
      (goto-char begin)

      (dotimes (j (length (wisi-ind-indent wisi--indent)))
	(let ((i (+ -1 j (wisi-ind-first-line wisi--indent))))
	  (goto-char (aref (wisi-ind-line-begin wisi--indent) i))
	  (indent-line-to (aref (wisi-ind-indent wisi--indent) j))))

      (when wisi-indent-failed
	;; previous parse failed
	(setq wisi-indent-failed nil)
	(run-hooks 'wisi-post-indent-fail-hook))

      ;; run wisi-indent-calculate-functions
      (goto-char begin)
      (setq end (min end (point-max))) ;; may have been marker moved by indent
      (while (< (point) end)
	(back-to-indentation)
	(setq indent
	      (run-hook-with-args-until-success 'wisi-indent-calculate-functions))
	(when indent
	  (indent-line-to indent))

	(forward-line 1))
      )))

(defun wisi-indent-line ()
  "For `indent-line-function'."
  (let ((savep (copy-marker (point)))
	(restore t))
    (back-to-indentation)
    (when (> (point) savep)
      (setq restore nil))

    (wisi-indent-region (line-beginning-position) (line-beginning-position 2))

    (goto-char savep)

    (when restore
      ;; point was inside line text; leave it there
      (back-to-indentation))
    ))

;;;; debug
(defun wisi-parse-buffer (&optional parse-action)
  (interactive)
  (when (< emacs-major-version 25) (syntax-propertize (point-max)))
  (if parse-action
      (let ((wisi--parse-action parse-action))
	(wisi-invalidate-cache)
	(wisi-validate-cache (point-max)) t)

    (wisi-indent-region (point-min) (point-max))
    ))

(defun wisi-lex-buffer ()
  ;; for timing the lexer; set indent so we get the slowest time
  (interactive)
  (when (< emacs-major-version 25) (syntax-propertize (point-max)))

  (let* ((wisi--parse-action 'indent)
	 (line-count (count-lines (point-min) (point-max)))
	 (wisi--indent
	  (make-wisi-ind
	   :region (cons (point-min) (point-max))
	   :first-line (line-number-at-pos (point-min))
	   :line-begin (make-vector (1+ line-count) 0)
	   :indent (make-vector line-count 0)
	   :last-line nil)))

    (wisi--set-line-begin (point-min))

    (goto-char (point-min))
    (while (not (eq wisi-eoi-term (wisi-tok-token (wisi-forward-token)))))
    ))

(defun wisi-show-cache ()
  "Show cache at point."
  (interactive)
  (message "%s" (wisi-get-cache (point))))

(defun wisi-show-token ()
  "Move forward across one keyword, show token_id."
  (interactive)
  (let ((token (wisi-forward-token)))
    (message "%s" (car token))))

(defun wisi-show-containing-or-previous-cache ()
  (interactive)
  (let ((cache (wisi-get-cache (point))))
    (if cache
	(message "containing %s" (wisi-goto-containing cache t))
      (message "previous %s" (wisi-backward-cache)))
    ))

(defun wisi-show-cache-max ()
  (interactive)
  (push-mark)
  (goto-char wisi-cache-max))

;;;;; setup

(defun wisi-setup (indent-calculate post-indent-fail class-list keyword-table token-table parse-table)
  "Set up a buffer for parsing files with wisi."
  (setq wisi-class-list class-list)

  (let ((numbers (cadr (symbol-value (intern-soft "number" token-table)))))
    (setq wisi--lexer
	  (make-wisi-lex
	   :keyword-table keyword-table
	   :punctuation-table (symbol-value (intern-soft "punctuation" token-table))
	   :punctuation-table-max-length 0
	   :string-double-term (car (symbol-value (intern-soft "string-double" token-table)))
	   :string-quote-escape-doubled nil
	   :string-quote-escape nil
	   :string-single-term (car (symbol-value (intern-soft "string-single" token-table)))
	   :symbol-term (car (symbol-value (intern-soft "symbol" token-table)))
	   :number-term (car numbers)
	   :number-p (cdr numbers)
	   )))

  (let (fail)
    (dolist (item (wisi-lex-punctuation-table wisi--lexer))
      (when item ;; default matcher can be nil

	;; check that all chars used in punctuation tokens have punctuation syntax
	(mapc (lambda (char)
		(when (not (= ?. (char-syntax char)))
		  (setq fail t)
		  (message "in %s, %c does not have punctuation syntax"
			   (car item) char)))
	      (cdr item))

	(when (< (wisi-lex-punctuation-table-max-length wisi--lexer) (length (cdr item)))
	  (setf (wisi-lex-punctuation-table-max-length wisi--lexer) (length (cdr item)))))
      )
    (when fail
      (error "aborting due to punctuation errors")))

  (setq wisi-parse-table parse-table)

  ;; file local variables may have added opentoken, gnatprep
  (setq wisi-indent-calculate-functions (append wisi-indent-calculate-functions indent-calculate))
  (set (make-local-variable 'indent-line-function) 'wisi-indent-line)
  (set (make-local-variable 'indent-region-function) 'wisi-indent-region)

  (setq wisi-post-indent-fail-hook post-indent-fail)
  (setq wisi-indent-failed nil)

  (add-hook 'before-change-functions #'wisi-before-change 'append t)
  (setq wisi--change-end (copy-marker (point-min) t))

  (when (functionp 'jit-lock-register);; FIXME: in emacs 24?
      (jit-lock-register 'wisi-fontify-region))

  ;; See comments on "lexer" above re syntax-propertize.
  (when (< emacs-major-version 25) (syntax-propertize (point-max)))

  (wisi-invalidate-cache)
  )

(provide 'wisi)
;;; wisi.el ends here
