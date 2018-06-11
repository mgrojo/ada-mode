;; wisi-grammar-mode --- Major mode for editing Wisi grammar files  -*- lexical-binding:t -*-

;; Copyright (C) 2017, 2018  Free Software Foundation, Inc.

;; Author: Stephen Leake <stephen_leake@stephe-leake.org>
;; Maintainer: Stephen Leake <stephen_leake@stephe-leake.org>

;; This file is part of GNU Emacs.

;; wisi-grammar-mode is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; wisi-grammar-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:

(require 'cl-lib)
(require 'xref)
(require 'wisi)
(require 'wisi_grammar_1-process)
(require 'wisi-process-parse)

(defgroup wisi-grammar nil
  "Major mode for editing Wisi grammar files in Emacs."
  :group 'languages)

(defcustom wisi-grammar-process-parse-exec "wisi_grammar_mode_parse.exe"
  "Name of executable to use for external process wisi-grammar parser,"
  :type 'string
  :group 'wisi-grammar)

(defvar wisi-grammar-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; see wisi-grammar-syntax-propertize for double semicolon as comment
    (modify-syntax-entry ?\n ">   " table)
    table))

(defvar wisi-grammar-mode-map
  (let ((map (make-sparse-keymap)))
    ;; C-c <letter> are reserved for users

    ;; comment-dwim is in global map on M-;
    (define-key map "\C-c\C-f" 'wisi-show-parse-error)
    (define-key map [S-return] 'wisi-grammar-new-line)
    map
  )  "Local keymap used for WISI-GRAMMAR mode.")

(defvar-local wisi-grammar-action-mode nil
  "Emacs major mode used for actions, inferred from ’%output_language’ declaration.")

(defun wisi-grammar-new-line ()
  "If in comment, insert new comment line.
If in nonterminal, insert new production right hand side.
Otherwise insert a plain new line."
  (interactive)
  (if (nth 4 (syntax-ppss))
      ;; in comment
      (comment-indent-new-line)

    (let ((pos (point))
	  (cache (save-excursion (wisi-goto-statement-start))))
	(if (and cache
		 (eq (wisi-cache-nonterm cache) 'nonterminal)
		 (wisi-cache-end cache)
		 (> (wisi-cache-end cache) pos))
	    (progn
	      ;; in nonterminal
	      (insert "\n| ")
	      (indent-according-to-mode))

	  (newline-and-indent)
	  ))
    ))

(defun wisi-grammar-which-function ()
  "For `which-func-functions'."
  (wisi-validate-cache (point) nil 'navigate)
  ;; no message on parse fail, since this could be called from which-func-mode
  (when (> (wisi-cache-max 'navigate) (point))
    (save-excursion
      (let ((cache (wisi-backward-cache)))
	;; Find terminal or nonterminal containing point (if any)
	(while
	    (and
	     cache
	     (not (bobp))
	     (not
	      (or
	       (and (eq (wisi-cache-class cache) 'statement-start)
		    (eq (wisi-cache-nonterm cache) 'nonterminal))
	       (and (eq (wisi-cache-class cache) 'name)
		    (eq (wisi-cache-nonterm cache) 'declaration)))))
	  (setq cache (wisi-backward-cache)))

	(when cache
	  ;; We don’t define an elisp lexer, so we can’t use wisi-forward-token
	  (buffer-substring-no-properties (point) (progn (skip-syntax-forward "w_") (point))); name

	  )))))

(defun wisi-grammar-add-log-current-function ()
  "For `add-log-current-defun-function'; return name of current non-terminal or declaration."
  ;; add-log-current-defun is typically called with point at the start
  ;; of an ediff change section, which is before the start of the
  ;; declaration of a new item. So go to the end of the current line
  ;; first
  (save-excursion
    (end-of-line 1)
    (wisi-grammar-which-function)))

(defun wisi-grammar-syntax-propertize (start end)
  "Assign `syntax-table' properties in accessible part of buffer."
  ;; (info "(elisp)Syntax Properties")
  ;;
  ;; called from `syntax-propertize', inside save-excursion with-silent-modifications
  ;; syntax-propertize-extend-region-functions is set to
  ;; syntax-propertize-wholelines by default.
  (let ((inhibit-read-only t)
	(inhibit-point-motion-hooks t))
    (goto-char start)
    (save-match-data
      (while (re-search-forward ";;" end t); comment start
	(put-text-property
	 (match-beginning 0) (match-end 0) 'syntax-table '(11 . nil)))
  )))

(defun wisi-grammar-set-action-mode ()
  (save-excursion
    (goto-char (point-min))
    (if (search-forward-regexp "%output_language +\\([A-Za-z_]+\\)$")
	(cond
	 ((or
	   (string-equal (match-string 1) "Ada_Emacs")
	   (string-equal (match-string 1) "elisp"))
	  (setq wisi-grammar-action-mode 'emacs-lisp-mode))

	 ((string-equal (match-string 1) "Ada")
	  (setq wisi-grammar-action-mode 'ada-mode))

	 (t
	  (error "unrecognized output language %s" (match-string 1)))
	 )
      (error "output_language declaration not found"))))

;;; xref integration
(defun wisi-grammar--xref-backend ()
  (cl-case major-mode
    (wisi-grammar 'wisi-grammar)
    (emacs-lisp-mode 'elisp)
    (t nil)))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql wisi-grammar)))
  (thing-at-point 'symbol))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql wisi-grammar)))
  ;; Complete on declaration names (ie terminals and other things) and
  ;; nonterminal names.
  (save-excursion
    (let (result
	  cache)
      (wisi-validate-cache (point-max) nil 'navigate)
      (goto-char (point-min))
      (while (setq cache (wisi-forward-cache))
	(when (or (and (eq 'declaration (wisi-cache-nonterm cache))
		       (eq 'name (wisi-cache-class cache)))
		  (eq 'nonterminal (wisi-cache-nonterm cache)))
	  ;; We can’t store location data in a string text property -
	  ;; it does not survive completion. So we include the line
	  ;; number in the identifier string.
	  (setq result (cons (format "%s<%d>" (wisi-cache-text cache) (line-number-at-pos)) result))))
      result)))

(cl-defmethod xref-backend-definitions ((_backend (eql wisi-grammar)) identifier)
  ;; The line number is incuded in the identifier wrapped in <>
  (string-match "\\([^<]*\\)\\(?:<\\([0-9]+\\)>\\)?" identifier)
  (let ((ident (match-string 1 identifier))
	(line-str (match-string 2 identifier))
	line)
    (if line-str
	(setq line (string-to-number line-str))

      ;; identifier is from identifier-at-point; search for definition
      (save-excursion
	(let (cache)
	  (wisi-validate-cache (point-max) nil 'navigate)
	  (goto-char (point-min))
	  (while (and (null line)
		      (setq cache (wisi-forward-cache)))
	    (when (or (and (eq 'declaration (wisi-cache-nonterm cache))
			   (eq 'name (wisi-cache-class cache)))
		      (eq 'nonterminal (wisi-cache-nonterm cache)))
	      (when (string-equal ident (wisi-cache-text cache))
		(setq line (line-number-at-pos)))
	      ))
	  )))
    (when line
      (list (xref-make ident (xref-make-file-location (buffer-file-name) line  0))))
    ))

;;; debug
(defun wisi-grammar-set-exec (exec-file)
  "Set EXEC-FILE for current and future wisi-grammar parsers."
  (interactive "f")
  (setq wisi-grammar-process-parse-exec exec-file)
  (wisi-process-parse-set-exec "wisi-grammar" exec-file))

;;;;
;;;###autoload
(define-derived-mode wisi-grammar-mode prog-mode "Wisi"
  "A major mode for Wisi grammar files."
  (set (make-local-variable 'syntax-propertize-function) 'wisi-grammar-syntax-propertize)
  (syntax-ppss-flush-cache (point-min));; reparse with new function
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)

  (set (make-local-variable 'comment-start) ";;")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "---*[ \t]*")
  (set (make-local-variable 'comment-multi-line) nil)
  (set (make-local-variable 'require-final-newline) t)
  (set (make-local-variable 'add-log-current-defun-function)
       'wisi-grammar-add-log-current-function)
  (setq wisi-size-threshold most-positive-fixnum);; grammar is simple enough for very large files

  (wisi-grammar-set-action-mode)

  (add-hook 'xref-backend-functions #'wisi-grammar--xref-backend nil t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
  (add-hook 'before-save-hook 'copyright-update nil t)

  (wisi-setup
   :indent-calculate nil
   :post-indent-fail nil
   :parser (wisi-process-parse-get
	    (make-wisi-process--parser
	     :label "wisi-grammar"
	     :exec-file wisi-grammar-process-parse-exec
	     :face-table wisi_grammar_1-process-face-table
	     :token-table wisi_grammar_1-process-token-table
	     ))
   :lexer nil)

  ;; Our wisi parser does not fontify comments and strings, so tell
  ;; font-lock to do that.
  (setq font-lock-defaults
	'(nil ;; keywords
	  nil ;; keywords-only
	  ))
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wy\\'" . wisi-grammar-mode))

(put 'wisi-grammar-mode 'custom-mode-group 'wisi-grammar)

(provide 'wisi-grammar-mode)

(when (locate-library "mmm-mode")
  (require 'wisi-grammar-mmm))

;;; end of file
