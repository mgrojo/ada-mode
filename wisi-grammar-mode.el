;; wisi-grammar-mode --- Major mode for editing Wisi grammar files  -*- lexical-binding:t -*-

;; Copyright (C) 2017  Free Software Foundation, Inc.

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
(require 'wisi)
(require 'wisi_grammar-process)
(require 'wisi-process-parse)

(defgroup wisi-grammar nil
  "Major mode for editing Wisi grammar files in Emacs."
  :group 'languages)

(defcustom wisi-grammar-process-parse-exec "wisi_grammar_mode_parse"
  "Name of executable to use for external process wisi-grammar parser,"
  :type 'string
  :group 'wisi-grammar)

(defvar wisi-grammar-mode-map
  (let ((map (make-sparse-keymap)))
    ;; C-c <letter> are reserved for users

    ;; comment-dwim is in global map on M-;
    (define-key map "\C-c\C-f" 'wisi-show-parse-error)
    map
  )  "Local keymap used for WISI-GRAMMAR mode.")

(defun wisi-grammar-which-function ()
  "For `which-func-functions'."
  (wisi-validate-cache (point) nil 'navigate)
  ;; no message on parse fail, since this could be called from which-func-mode
  (when (> (wisi-cache-max 'navigate) (point))
    (let ((cache (wisi-backward-cache))
	  done)
      (while (and cache (not done))
	;; find nonterminal containing point (if any)
	(cond
	 ((eq (wisi-cache-class cache) 'name)
	  (setq done t))
	 )
	(wisi-backward-cache))

      (when done
	;; We don’t define an elisp lexer, so we can’t use wisi-forward-token
	(buffer-substring (point) (progn (skip-syntax-forward "w_") (point))); name

	))))

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

;;;;
;;;###autoload
(define-derived-mode wisi-grammar-mode prog-mode "Wisi"
  "A major mode for Wisi grammar files."
  (set (make-local-variable 'syntax-propertize-function) 'wisi-grammar-syntax-propertize)
  (set (make-local-variable 'comment-start) ";;")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "---*[ \t]*")
  (set (make-local-variable 'comment-multi-line) nil)
  (set (make-local-variable 'require-final-newline) t)
  (set (make-local-variable 'add-log-current-defun-function)
       'wisi-grammar-add-log-current-function)

  (wisi-setup
   :indent-calculate nil
   :post-indent-fail nil
   :parser (wisi-process-parse-get
	    (make-wisi-process--parser
	     :label "wisi-grammar"
	     :exec-file wisi-grammar-process-parse-exec
	     :face-table wisi_grammar-process-face-table
	     :token-table wisi_grammar-process-token-table
	     ))
   :lexer nil)
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wy\\'" . wisi-grammar-mode))

(put 'wisi-grammar-mode 'custom-mode-group 'wisi-grammar)

(provide 'wisi-grammar-mode)
;;; end of file
