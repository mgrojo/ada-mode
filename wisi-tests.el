;;; wisi-tests.el --- Common utils for wisi tests -*- lexical-binding:t -*-
;;
;; Copyright (C) 2012 - 2020  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@stephe-leake.org>
;; Maintainer: Stephen Leake <stephen_leake@stephe-leake.org>
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

(require 'cl-lib)
(require 'wisi)

(defvar wisi-test-parser 'process
  "Set to ’process to test external process parser.")

(defvar test-syntax-table
  (let ((table (make-syntax-table)))
    ;; make-syntax-table sets all alphanumeric to w, etc; so we only
    ;; have to add test-specific things.

    ;; operator symbols
    (modify-syntax-entry ?&  "." table)
    (modify-syntax-entry ?*  "." table)
    (modify-syntax-entry ?+  "." table)
    (modify-syntax-entry ?-  "." table)
    (modify-syntax-entry ?/  "." table)
    (modify-syntax-entry ?<  "." table)
    (modify-syntax-entry ?=  "." table)
    (modify-syntax-entry ?>  "." table)
    (modify-syntax-entry ?|  "." table)

    ;; \f and \n end a comment - see test-syntax-propertize for comment start
    (modify-syntax-entry ?\f  ">   " table)
    (modify-syntax-entry ?\n  ">   " table)
    table
    ))

(defun test-syntax-propertize (start end)
  "Assign `syntax-table' properties in accessible part of buffer."
  ;; (info "(elisp)Syntax Properties")
  (let ((modified (buffer-modified-p))
	(buffer-undo-list t)
	(inhibit-read-only t)
	(inhibit-point-motion-hooks t)
	(inhibit-modification-hooks t))
    (goto-char start)
    (while (re-search-forward
	     "\\(--\\)"; 1: comment start
	    end t)
      ;; The help for syntax-propertize-extend-region-functions
      ;; implies that 'start end' will always include whole lines, in
      ;; which case we don't need
      ;; syntax-propertize-extend-region-functions
      (cond
       ((match-beginning 1)
	(put-text-property
	 (match-beginning 1) (match-end 1) 'syntax-table '(11 . nil)))
       ))
    (unless modified
      (restore-buffer-modified-p nil))))

(defun wisi-tests-setup (grammar-name)
  ;; grammar-elisp file must be on load-path
  ;; use Ada style comments in source
  (set-syntax-table test-syntax-table)
  (set (make-local-variable 'syntax-propertize-function) 'test-syntax-propertize)
  (syntax-ppss-flush-cache (point-min));; force re-evaluate with hook.

  (cl-ecase wisi-test-parser
    (process
     (require 'wisi-process-parse)
     (require (intern (concat grammar-name "-process"))) ;; generated by wisi-generate
     (require (intern grammar-name)) ;; declares parser cl-defstruct
     (add-to-list 'exec-path default-directory)
     (wisi-setup
      :indent-calculate nil
      :post-indent-fail nil
      :parser
      (wisi-process-parse-get
       (funcall
	(intern-soft (concat "make-" grammar-name "-wisi-parser"))
	:label grammar-name
	:exec-file (concat grammar-name "_wisi_parse.exe")
	:face-table (symbol-value (intern-soft (concat grammar-name "-process-face-table")))
	:token-table (symbol-value (intern-soft (concat grammar-name "-process-token-table")))
	)))
     )
    )

  ;; Not clear why this is not being done automatically
  (syntax-propertize (point-max))
  )

;;; Initialization

;; Default includes mtn, among others, which is broken in Emacs 22.2
(setq vc-handled-backends '(CVS))

(setq eval-expression-debug-on-error nil)

;; ’package-initialize’ is not here; it must be run as part of one of the
;; -l or --eval command line options

(provide 'wisi-tests)
;; end of file
