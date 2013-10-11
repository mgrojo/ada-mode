;;; gpr-mode --- major-mode for editing GNAT project files

;; Copyright (C) 2007, 2008, 2012, 2013  Stephen Leake
;; Copyright (C) 2004  Rolf Ebert

;; Author: Stephen Leake <stephen_leake@member.fsf.org>
;; Maintainer: Stephen Leake <stephen_leake@member.fsf.org>
;; Keywords: languages ada gpr gprbuild

;; This file is part of GNU Emacs.

;; gpr-mode is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; gpr-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;;
;;; History:
;;
;; The first gpr-mode was written by Rolf Ebert
;; <rolf.ebert_nosp...@gmx.net> in 2004.
;;
;; Stephen Leake <stephen_leake@member.fsf.org> rewrote it in 2013 to
;; use the wisi indentation engine.
;;
;;;;; Code:

;; we reuse several ada-mode functions
(require 'ada-mode)

(defun gpr-align ()
  "If region is active, apply 'align'. If not, attempt to align
current construct."
  (interactive)
  (if (use-region-p)
      (progn
        (align (region-beginning) (region-end))
        (deactivate-mark))

    (align-current)
    ))

(defvar gpr-mode-map
  (let ((map (make-sparse-keymap)))
    ;; C-c <letter> are reserved for users

    ;; global-map has C-x ` 'next-error
    (define-key map [return]   'ada-indent-newline-indent)
    (define-key map "\C-c`"    'ada-show-secondary-error)
    (define-key map "\C-c\C-a" 'gpr-align)
    (define-key map "\C-c\C-c" 'compile)
    ;; FIXME (later): implement?
    ;; (define-key map "\C-c\C-n" 'ada-next-statement-keyword)
    ;; (define-key map "\C-c\C-p" 'ada-prev-statement-keyword)
    (define-key map "\C-c\C-S-p" 'gpr-set-as-project)
    (define-key map "\C-c\C-t" 'ada-case-read-all-exceptions)
    (define-key map "\C-c\C-w" 'ada-case-adjust-at-point)
    (define-key map "\C-c\C-y" 'ada-case-create-exception)
    (define-key map "\C-c\C-\M-y" (lambda () (ada-case-create-exception nil nil t)))
    map
  )  "Local keymap used for GPR mode.")

(defvar gpr-mode-menu (make-sparse-keymap "gpr"))
(easy-menu-define gpr-mode-menu gpr-mode-map "Menu keymap for gpr mode"
  '("gpr"
    ("Help"
     ["gpr Mode"              (info "gpr-mode") t]
     ["GNAT Reference Manual" (info "gnat_rm") t]
     ["GNAT User Guide"       (info "gnat_ugn") t]
     ["Key bindings"          describe-bindings t]
     )

    ["Customize"     (customize-group 'ada)];; we reuse the Ada indentation options
    ["------"        nil nil]
    ["Set as current project"     gpr-set-as-project        t]
    ["Next compilation error"     next-error                t]
    ["Show secondary error"       ada-show-secondary-error  t]
    ["Toggle show parser errors"  wisi-toggle-show-parser-errors t]
    ("Edit"
     ["Indent Line"                 indent-for-tab-command  t]
     ["Indent Lines in Selection"   indent-region           t]
     ["Indent Lines in File"        (indent-region (point-min) (point-max))  t]
     ["Align"                       gpr-align               t]
     ["Comment Selection"           comment-region               t]
     ["Uncomment Selection"         (lambda () (comment-region t)) t]
     ["Fill Comment Paragraph"      fill-paragraph               t]
     ["Fill Comment Paragraph Justify"
      ada-fill-comment-paragraph-justify                         t]
     ["Fill Comment Paragraph Postfix"
      ada-fill-comment-paragraph-postfix                         t]
     )
    ))

(defvar gpr-font-lock-keywords
  (progn
    (list
     ;;
     ;; keyword plus name.
     (list (concat
	    "\\<\\("
	    "package\\|"
	    "project\\|"
	    "for"
	    "\\)\\>[ \t]*"
	    "\\(\\sw+\\(\\.\\sw*\\)*\\)?")
	   '(1 font-lock-keyword-face) '(2 font-lock-function-name-face nil t))
     ;;
     ;; Main keywords
     (list (concat "\\<"
		   (regexp-opt
		    '("abstract" "aggregate" "case" "configuration" "external" "is" "library" "null" "others"
		      "renames" "standard" "type" "use" "when" "with") t)
		   "\\>")
	   '(1 font-lock-keyword-face))
     ;;
     ;; Anything following end and not already fontified is a body name.
     '("\\<\\(end\\)\\>\\([ \t]+\\)?\\(\\(\\sw\\|[_.]\\)+\\)?"
       (1 font-lock-keyword-face) (3 font-lock-function-name-face nil t))
     ;;
     ))
  "Expressions to highlight in gpr mode.")

(defun gpr-ff-special-with ()
  (ada-require-project-file)
  (let ((project-name (match-string 1)))
    (file-name-nondirectory
     (or
      (ff-get-file-name
       (ada-prj-get 'prj_dir)
       (match-string 1)
       '("" ".gpr"))
      (error "project '%s' not found; set project file?" project-name)))
    ))

(defun gpr-set-ff-special-constructs ()
  "Add gpr-specific pairs to `ff-special-constructs'."
  (set (make-local-variable 'ff-special-constructs) nil)
  (mapc (lambda (pair) (add-to-list 'ff-special-constructs pair))
	;; Each car is a regexp; if it matches at point, the cdr is
	;; invoked.  Each cdr should return the absolute file name to
	;; go to.
	(list
	 ;; A "with" clause; allow "foo_bar.gpr"
	 (cons "^with[ \t]+\"\\(\\(?:\\(?:\\sw\\|\\s.\\)\\|\\s_\\)+\\)\";"
	       'gpr-ff-special-with)
	 )))

(defvar gpr-which-function nil
  ;; supplied by the indentation engine
  "Function called with no parameters; it should return the name
of the package or project point is in or just after, or nil.")

(defun gpr-which-function ()
  "See `gpr-which-function' variable."
  (when gpr-which-function
    (funcall gpr-which-function)))

(defun gpr-add-log-current-function ()
  "For `add-log-current-defun-function'. Returns enclosing package or project name."
  ;; add-log-current-defun is typically called with point at the start
  ;; of an ediff change section, which is before the start of the
  ;; declaration of a new item. So go to the end of the current line
  ;; first
  (save-excursion
    (end-of-line 1)
    (gpr-which-function)))

(defun gpr-set-as-project (&optional file)
  "Set FILE (default current buffer file) as Emacs project file."
  (interactive)
  (ada-parse-prj-file (or file (buffer-file-name)))
  (ada-select-prj-file (or file (buffer-file-name))))

;;;;
(defun gpr-mode ()
  "The major mode for editing GNAT project files."

  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'gpr-mode)
  (setq mode-name "GNAT Project")
  (use-local-map gpr-mode-map)
  (set-syntax-table ada-mode-syntax-table)
  (set (make-local-variable 'syntax-begin-function) nil)
  (set 'case-fold-search t); gpr is case insensitive; the syntax parsing requires this setting
  (set (make-local-variable 'comment-start) "--")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "---*[ \t]*")
  (set (make-local-variable 'comment-multi-line) nil)

  (set (make-local-variable 'require-final-newline) t)

  (set (make-local-variable 'font-lock-defaults)
       '(gpr-font-lock-keywords
	 nil t
	 ((?\_ . "w"))))

  (gpr-set-ff-special-constructs)
  (setq ff-search-directories 'ada-project-search-path)

  (set (make-local-variable 'add-log-current-defun-function)
       'gpr-add-log-current-function)

  ;; used by autofill to break a comment line and continue it on
  ;; another line. The reason we need this one is that the default
  ;; behavior does not work correctly with the definition of
  ;; paragraph-start above when the comment is right after a
  ;; multi-line subprogram declaration (the comments are aligned under
  ;; the latest parameter, not under the declaration start).
  ;; FIXME: need test - should be in gpr-wisi?
  (set (make-local-variable 'comment-line-break-function)
       (lambda (&optional soft) (let ((fill-prefix nil))
				  (indent-new-comment-line soft))))

  (run-hooks 'gpr-mode-hook)

  ;; FIXME (later): need this? use ada? duplicate? factor out?
  ;; (if gpr-auto-case
  ;;     (gpr-activate-keys-for-case))
  )

;; user needs to add this somewhere:
(add-to-list 'auto-mode-alist '("\\.gpr\\'" . gpr-mode))  ; GNAT project files

(provide 'gpr-mode)

(unless (featurep 'gpr-indent-engine)
  (require 'gpr-wisi))

;;; end of file
