;;; Utilities for implementing an indentation engine using the wisent LALR parser
;;
;; Copyright (C) 2012  Free Software Foundation, Inc.
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
;;; code style
;;
;; 'wisi' is short for "wisent indentation engine".
;;
;; not using lexical-binding because we support Emacs 23
;;
;;;;

;;; lexer
;;

(defvar wisi-keyword-table nil)

(defun wisi-forward-token ()
  "Move point forward across one token, skipping whitespace and comments.
Return the corresponding wisent token: '(symbol text (start end)),
where:
`symbol' is a token symbol as defined in the tokens section of the
grammar definition file,
`text' is the token text from the buffer,
`(start end)' are the character positions in the buffer of the start
and end of the token text."
  ;; FIXME: handle parens
  (forward-comment (point-max))
  ;; skips leading whitespace, comment, trailing whitespace.

  (let ((start (point))
	token-text
	end)
    (setq token-text
	  (buffer-substring-no-properties
	  (point)
	  (progn (if (zerop (skip-syntax-forward "."))
		     (skip-syntax-forward "w_'"))
		 (setq end (point)))))

    (list (or (symbol-value (intern-soft token-text wisi-keyword-table))
	      'IDENTIFIER)
	  token-text
	  (list start end))
  ))

(defun wisi-backward-token ()
  "Move point backward across one token, skipping whitespace and comments."
  ;; FIXME: handle parens
  (forward-comment (- (point-max)))
  ;; skips leading whitespace, comment, trailing whitespace.

  (if (zerop (skip-syntax-backward "."))
      (skip-syntax-backward "w_'"))
  )

;;; token info cache

(defstruct
  (wisi-cache
   (:constructor wisi-cache-make)
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
  (setq wisi-cache-max 0))

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

;;; parse actions

(defun wisi-cache-keywords (&rest keywords)
  "Cache information used by wisi in text properties of keywords.
Intended as a wisent grammar non-terminal action.  KEYWORDS is a
list of lists '(symbol class start-func (start end)) for keywords
that should receive cached information."
  (save-excursion
    (let (first-keyword-mark)
      (while keywords
	(let* ((keyword (pop keywords))
	       (symbol (pop keyword))
	       (class (pop keyword))
	       (region (pop keyword))
	       (mark
		(progn (goto-char (caar region))
		       (point-marker))))

	  (put-text-property
	   (caar keyword-region)
	   (cadar keyword-region)
	   'wisi-cache
	   (wisi-cache-create
	    :symbol symbol
	    :class class
	    :start (or first-keyword-mark start-func)
	    ))

	  (unless first-keyword-mark (setq first-keyword-mark mark))
	))
      )))

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

;;; motion

(defun wisi-prev-keyword ()
  "Move point the keyword preceding point, skipping over non-keyword tokens.
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
   ((mark-p (wisi-cache-start cache))
    (goto-char (wisi-cache-start cache))
    (wisi-goto-statement-start (wisi-get-cache (point)) keyword))

   ((and (not keyword)
	 (function-p (wisi-cache-start cache)))
    (funcall (wisi-cache-start cache)))

   (t nil)))

;;; indentation

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
  "Indent current line using the ada-wisi indentation engine."
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
      (if savep
	  ;; point was inside line text; leave it there
          (save-excursion (indent-line-to indent))
	;; point was before line text; move to start of text
        (indent-line-to indent)))
  ))

;;; setup

(defun wisi-setup (indent-calculate keyword-table parse-table)
  "Set up a buffer for parsing files with wisi."

  (set (make-local-variable 'wisi-indent-calculate-function) indent-calculate)
  (set (make-local-variable 'wisi-keyword-table) keyword-table)
  (set (make-local-variable 'wisi-parse-table) parse-table)
  (set (make-local-variable 'indent-line-function) 'wisi-indent-line)

  ;; FIXME: do we want this?
  ;; (semantic-make-local-hook 'wisent-discarding-token-functions)
  ;; (add-hook 'wisent-discarding-token-functions
  ;; 	    'wisent-collect-unmatched-syntax nil t))

  ;; FIXME: (set (make-local-variable 'forward-sexp-function) 'wisi-forward-sexp-command)
  ;; This is nice for user navigation; do we need it for wisi?

  )

(provide 'wisi)

;;; end of file
