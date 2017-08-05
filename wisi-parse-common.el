;;; wisi-parse-common.el --- declarations used by wisi-parse.el, wisi-ada-parse.el, and wisi.el
;;
;; Copyright (C) 2014, 2015, 2017  Free Software Foundation, Inc.
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

(cl-defstruct wisi-parser
  error-msgs
  ;; List of error messages from last parse. Can be more than one if
  ;; parser supports error recovery.
)

(cl-defgeneric wisi-parse-current ((parser wisi-parser))
  "Parse current buffer.")

(cl-defgeneric wisi-parse-kill ((parser wisi-parser))
  "Kill any external process associated with parser.")

(cl-defgeneric wisi-parse-find-token ((parser wisi-parser) token-symbol)
  "Find token with TOKEN-SYMBOL on current parser stack, return token struct.
For use in grammar actions.")

(cl-defgeneric wisi-parse-prev-token ((parser wisi-parser) n)
  "Return the Nth token on the stack before the token currently being reduced.
For use in grammar actions.")

(defvar wisi-debug 0
  "wisi debug mode:
0 : normal - ignore parse errors, for indenting new code
1 : report parse errors (for running tests)
2 : show parse states, position point at parse errors, debug-on-error works in parser
3 : also show top 10 items of parser stack.")

;; The following parameters are easily changeable for debugging.
(defvar-local wisi-panic-enable nil
  "If non-nil, enable panic mode error recovery.")

(defvar-local wisi-mckenzie-enable nil
  "If non-nil, enable McKenzie error recovery.")

(defvar-local wisi-mckenzie-enqueue-limit nil
  "If non-nil, McKenzie recover algorithm limit; higher has more recover power, but takes longer.
If nil, uses value from grammar file.")

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
  token  ;; symbol from a token table ;; FIXME: rename to ’id’?
  region ;; cons giving buffer region containing token text

  nonterminal ;; t if a nonterminal

  ;; The following are set if parsing for indent.

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
