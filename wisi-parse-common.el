;;; wisi-parse-common.el --- declarations used by wisi-parse.el, wisi-ada-parse.el, and wisi.el
;;
;; Copyright (C) 2014, 2015, 2017, 2018  Free Software Foundation, Inc.
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

;;; Code:

;; WORKAROUND: for some reason, this condition doesn't work in batch mode!
;; (when (and (= emacs-major-version 24)
;; 	   (= emacs-minor-version 2))
  (require 'wisi-compat-24.2)
;;)

(cl-defstruct (wisi--lexer-error)
  pos ;; position (integer) in buffer where error was detected.
  message  ;; string error message
  inserted ;; char inserted after pos.
  )

(cl-defstruct (wisi--parse-error-repair)
  pos ;; position (integer) in buffer where insert/delete is done.
  inserted ;; list of token IDs that were inserted before pos
  deleted  ;; list of token IDs that were deleted after pos
  )

(cl-defstruct (wisi--parse-error)
  ;; Includes information derived from compiler error recovery to edit
  ;; text to fix one error. Used by ’wisi-repair-error’ to edit buffer.
  pos      ;; position (integer or marker) in buffer where error was detected.
  message  ;; string error message
  repair   ;; list of wisi--parse-error-repair.
  )

(cl-defstruct wisi-parser
  ;; Separate lists for lexer and parse errors, because lexer errors
  ;; must be repaired first, before parse errors can be repaired. And
  ;; they have different structures.
  lexer-errors
  ;; list of wisi--lexer-errors from last parse.  Can be more than one if
  ;; lexer supports error recovery.
  parse-errors
  ;; List of wisi--parse-errors from last parse. Can be more than one if
  ;; parser supports error recovery.
)

(cl-defgeneric wisi-parse-format-language-options ((parser wisi-parser))
  "Return a string to be sent to the parser, containing settings
for the language-specific parser options."
  ;; not needed for the elisp parser, which can see the options directly.
  "")

(cl-defgeneric wisi-parse-current ((parser wisi-parser))
  "Parse current buffer.")

(cl-defgeneric wisi-parse-kill ((parser wisi-parser))
  "Kill any external process associated with parser.")

(cl-defgeneric wisi-parse-find-token ((parser wisi-parser) token-symbol)
  "Find token with TOKEN-SYMBOL on current parser stack, return token struct.
For use in grammar actions.")

(cl-defgeneric wisi-parse-stack-peek ((parser wisi-parser) n)
  "Return the Nth token on the parse stack.
For use in grammar actions.")

(defvar wisi-debug 0
  "wisi debug mode:
0 : normal - ignore parse errors, for indenting new code
1 : report parse errors (for running tests)
2 : show parse states, position point at parse errors
3 : also show top 10 items of parser stack.")

;; The following parameters are easily changeable for debugging.
(defvar wisi-action-disable nil
  "If non-nil, disable all elisp actions during parsing.
Allows timing parse separate from actions.")

(defvar-local wisi-panic-enable nil
  "If non-nil, enable panic mode error recovery.")

(defvar-local wisi-mckenzie-disable nil
  "If non-nil, disable McKenzie error recovery. Otherwise, use parser default.")

(defcustom wisi-mckenzie-cost-limit nil
  "If integer, sets McKenzie recover algorithm limit.
Higher value has more recover power, but takes longer.
If nil, uses value from grammar file."
  :type 'integer
  :group 'wisi
  :safe 'integerp)
(make-variable-buffer-local 'wisi-mckenzie-cost-limit)

(defcustom wisi-mckenzie-check-limit nil
  "If integer, sets McKenzie recover algorithm limit.
Higher value has more recover power, but may fail if there are
two errors close together.  If nil, uses value from grammar
file."
  :type 'integer
  :group 'wisi
  :safe 'integerp)
(make-variable-buffer-local 'wisi-mckenzie-check-limit)

(defcustom wisi-mckenzie-enqueue-limit nil
  "If integer, sets McKenzie recover algorithm limit.
Higher value has more recover power, but will be slower to fail.
If nil, uses value from grammar file."
  :type 'integer
  :group 'wisi
  :safe 'integerp)
(make-variable-buffer-local 'wisi-mckenzie-enqueue-limit)

(defvar wisi-parse-max-parallel 15
  "Maximum number of parallel parsers for acceptable performance.
If a file needs more than this, it's probably an indication that
the grammar is excessively redundant.")

(defvar wisi-parse-max-stack-size 500
  "Maximum parse stack size")
;; end of easily changeable parameters

(defvar wisi--parse-action nil
  ;; not buffer-local; only let-bound in wisi-indent-region, wisi-validate-cache
  "Reason current parse is begin run; one of
{indent, face, navigate}.")

(defconst wisi-eoi-term 'Wisi_EOI
  ;; must match FastToken wisi-output_elisp.adb EOI_Name, which must
  ;; be part of a valid Ada identifer.
  "End Of Input token.")

(defun wisi-error-msg (message &rest args)
  (let ((line (line-number-at-pos))
	(col (- (point) (line-beginning-position))))
    (format
     "%s:%d:%d: %s"
       (buffer-name) ;; buffer-file-name is sometimes nil here!?
       line col
       (apply 'format message args))))

(defvar wisi-parse-error nil)
(put 'wisi-parse-error
     'error-conditions
     '(error wisi-parse-error))
(put 'wisi-parse-error
     'error-message
     "wisi parse error")

(cl-defstruct wisi-tok
  token  ;; symbol from a token table ;; IMPROVEME: rename to ’id’?
  region ;; cons giving buffer region containing token text

  nonterminal ;; t if a nonterminal

  line ;; Line number at start of token. Nil for empty nonterminals

  first
  ;; For terminals, t if token is the first token on a line.
  ;;
  ;; For nonterminals, line number of first contained line that needs
  ;; indenting; it is a comment, or begins with a contained token.
  ;;
  ;; Otherwise nil.

  ;; The following are non-nil if token (terminal or non-terminal) is
  ;; followed by blank or comment lines
  comment-line ;; first blank or comment line following token
  comment-end ;; position at end of blank or comment lines
  )

(defun wisi-token-text (token)
  "Return buffer text from token range."
  (let ((region (wisi-tok-region token)))
    (and region
       (buffer-substring-no-properties (car region) (cdr region)))))

(defun wisi-and-regions (left right)
  "Return region enclosing both LEFT and RIGHT."
  (if left
      (if right
	  (cons (min (car left) (car right))
		(max (cdr left) (cdr right)))
	left)
    right))

;;;; debugging
(defun wisi-tok-debug-image (tok)
  "Return id and region from TOK, as string."
  (cond
   ((wisi-tok-region tok)
    (format "(%s %d . %d)"
	    (wisi-tok-token tok)
	    (car (wisi-tok-region tok))
	    (cdr (wisi-tok-region tok))))
   (t
    (format "(%s)" (wisi-tok-token tok)))
   ))

(provide 'wisi-parse-common)
