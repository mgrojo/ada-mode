;;; wisi-prj.el --- project definition files -*- lexical-binding:t -*-
;;
;; Copyright (C) 2019  Free Software Foundation, Inc.
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

(cl-defstruct wisi-prj
  (case-exception-files nil)
  ;; List of casing exception files; from `casing' project variable.
  ;;
  ;; New exceptions may be added interactively via
  ;; `wisi-case-create-exception'.  If an exception is defined in
  ;; multiple files, the first occurence is used.
  ;;
  ;; The file format is one word per line, which gives the casing to be
  ;; used for that word in Ada source code.  If the line starts with
  ;; the character *, then the exception will be used for partial
  ;; words that either start at the beginning of a word or after a _
  ;; character, and end either at the end of the word or at a _
  ;; character.  Characters after the first word are ignored, and not
  ;; preserved when the list is written back to the file."

  (case-full-exceptions '())
  ;; Alist of full words that have special casing, built from
  ;; case-exception-files. Indexed by properly cased word; value is t.

  (case-partial-exceptions '())
  ;; Alist of partial words that have special casing, built from
  ;; project casing files list partial word exceptions. Indexed by
  ;; properly cased word; value is t.

  )

(defconst wisi-prj-default (make-wisi-prj))

(defvar wisi-prj-file-extensions '("adp" "prj")
  "List of wisi project file extensions.")

(defvar wisi-prj-parse-hook nil
  "Hook run at start of `wis-parse-prj-file'.")

;;;; auto-casing

(defvar-local wisi-auto-case nil
  "Buffer-local value indicating whether to change case while typing.
t means always; not-upper-case means only change case if typed
word is not all upper-case.  Casing of language keywords is done
according to `wisi-case-keyword', identifiers according to
wisi-case-identifier."
  ;; This is not a defcustom, because each language should declare a
  ;; defcustom for this.
  )

(defvar-local wisi-case-keyword 'lower-case
  "Indicates how to adjust the case of Ada keywords.
One of lower-case, upper-case."
  ;; This is not a defcustom, because each language should declare a
  ;; defcustom for this.
  )

(defvar-local wisi-case-identifier 'mixed-case
  "Buffer-local value indicating how to case language keywords.
Value is one of:

- mixed-case-relaxed : Mixed_Case, unless already UPPER_CASE
- mixed-case-strict  : Mixed_Case
- lower-case         : lower_case
- upper-case         : UPPER_CASE")

(defun wisi-case-show-files (project)
  "Show PROJECT casing files list."
  (if (wisi-prj-case-exception-files project)
      (progn
	(pop-to-buffer (get-buffer-create "*casing files*"))
	(erase-buffer)
	(dolist (file (wisi-prj-case-exception-files project))
	  (insert (format "%s\n" file))))
    (message "no casing files")
    ))

(defun wisi--case-save-exceptions (full-exceptions partial-exceptions file-name)
  "Save FULL-EXCEPTIONS, PARTIAL-EXCEPTIONS to the file FILE-NAME."
  (with-temp-file (expand-file-name file-name)
    (mapc (lambda (x) (insert (car x) "\n"))
	  (sort (copy-sequence full-exceptions)
		(lambda(a b) (string< (car a) (car b)))))
    (mapc (lambda (x) (insert "*" (car x) "\n"))
	  (sort (copy-sequence partial-exceptions)
		(lambda(a b) (string< (car a) (car b)))))
    ))

(defun wisi--case-read-exceptions (file-name)
  "Read the content of the casing exception file FILE-NAME.
Return (cons full-exceptions partial-exceptions)."
  (setq file-name (expand-file-name (substitute-in-file-name file-name)))
  (if (file-readable-p file-name)
      (let (full-exceptions partial-exceptions word)
	(with-temp-buffer
	  (insert-file-contents file-name)
	  (while (not (eobp))

	    (setq word (buffer-substring-no-properties
			(point) (save-excursion (skip-syntax-forward "w_") (point))))

	    (if (char-equal (string-to-char word) ?*)
		;; partial word exception
		(progn
		  (setq word (substring word 1))
		  (unless (assoc-string word partial-exceptions t)
		    (push (cons word t) partial-exceptions)))

	      ;; full word exception
	      (unless (assoc-string word full-exceptions t)
		(push (cons word t) full-exceptions)))

	    (forward-line 1))
	  )
	(cons full-exceptions partial-exceptions))

    ;; else file not readable; might be a new project with no
    ;; exceptions yet, so just return empty pair
    (message "'%s' is not a readable file." file-name)
    '(nil . nil)
    ))

(defun wisi--case-merge-exceptions (result new)
  "Merge NEW exeptions into RESULT.
An item in both lists has the RESULT value."
  (dolist (item new)
    (unless (assoc-string (car item) result t)
      (push item result)))
  result)

(defun wisi--case-merge-all-exceptions (exceptions project)
  "Merge EXCEPTIONS into PROJECT case-full-exceptions, case-partial-exceptions."
  (setf (wisi-prj-case-full-exceptions project)
	(wisi--case-merge-exceptions (wisi-prj-case-full-exceptions project)
				     (car exceptions)))
  (setf (wisi-prj-case-partial-exceptions project)
	(wisi--case-merge-exceptions (wisi-prj-case-partial-exceptions project)
				     (cdr exceptions))))

(defun wisi--case-read-all-exceptions (project)
  "Read case exceptions from all files in PROJECT casing files."
  (setf (wisi-prj-case-full-exceptions project) '())
  (setf (wisi-prj-case-partial-exceptions project) '())

  (dolist (file (wisi-prj-case-exception-files project))
    (wisi--case-merge-all-exceptions (wisi--case-read-exceptions file) project)))

(defun wisi--case-add-exception (word exceptions)
  "Add case exception WORD to EXCEPTIONS, replacing current entry, if any."
  (if (assoc-string word exceptions t)
      (setcar (assoc-string word exceptions t) word)
    (push (cons word t) exceptions))
  exceptions)

(defun wisi-case-create-exception (project &optional partial)
  "Define a word as an auto-casing exception in PROJECT.
The word is the active region, or the symbol at point.  If
PARTIAL is non-nil, create a partial word exception.  User is
prompted to choose a file from the project case-exception-files
if it is a list."
  (let ((file-name
	 (cond
	   ((< 1 (length (wisi-prj-case-exception-files project)))
	    (completing-read "case exception file: " (wisi-prj-case-exception-files project)
			     nil ;; predicate
			     t   ;; require-match
			     nil ;; initial-input
			     nil ;; hist
			     (car (wisi-prj-case-exception-files project)) ;; default
			     ))

	   ((= 1 (length (wisi-prj-case-exception-files project)))
	    (car (wisi-prj-case-exception-files project)))

	   (t
	    (error "No exception file specified; set `casing' in project file."))
	   ))
	word)

    (if (use-region-p)
	(progn
	  (setq word (buffer-substring-no-properties (region-beginning) (region-end)))
	  (deactivate-mark))
      (save-excursion
	(let ((syntax (if partial "w" "w_")))
	  (skip-syntax-backward syntax)
	  (setq word
		(buffer-substring-no-properties
		 (point)
		 (progn (skip-syntax-forward syntax) (point))
		 )))))

    (let* ((exceptions (wisi--case-read-exceptions file-name))
	   (file-full-exceptions (car exceptions))
	   (file-partial-exceptions (cdr exceptions)))

      (cond
       ((null partial)
	(setf (wisi-prj-case-full-exceptions project)
	      (wisi--case-add-exception word (wisi-prj-case-full-exceptions project)))
	(setq file-full-exceptions (wisi--case-add-exception word file-full-exceptions)))

       (t
	(setq (wisi-prj-case-partial-exceptions project)
	      (wisi--case-add-exception word (wisi-prj-case-partial-exceptions project)))
	(setq file-partial-exceptions (wisi--case-add-exception word file-partial-exceptions)))

       )
      (wisi--case-save-exceptions file-full-exceptions file-partial-exceptions file-name)
      (message "created %s case exception '%s' in file '%s'"
	       (if partial "partial" "full")
	       word
	       file-name)
      )))

(defun wisi-case-create-partial-exception ()
  "Define active region or word at point as a partial word exception.
User is prompted to choose a file from the project
case-exception-files if it is a list."
  (interactive)
  (wisi-case-create-exception t))

(provide 'wisi-prj)
;; end wisi-prj.el
