;; Ada mode functionality specific to the GNAT compiler
;;
;; GNAT is provided by AdaCore; see http://libre.adacore.com/
;;
;;; Copyright (C) 2012  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
;; Maintainer: Stephen Leake <stephen_leake@member.fsf.org>
;; Keywords: languages ada
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
;;; Usage:
;;
;; Emacs should enter Ada mode automatically when you load an Ada
;; file, based on the file extension.
;;
;; By default, ada-mode is configured to load this file, so nothing
;; special needs to done to use it.

(defun ada-indent-gnatprep ()
  "If point is on a gnatprep keyword, return indentation column
for it. Otherwise return nil.
Intended to be added to `smie-indent-functions'."
  ;; gnatprep keywords are:
  ;;
  ;; #if identifier [then]
  ;; #elsif identifier [then]
  ;; #else
  ;; #end if;
  ;;
  ;; they are all indented at column 0.
  (when (equal (char-after) ?\#) 0))

(defun ada-gnat-syntax-propertize (start end)
  (goto-char start)
  (while (re-search-forward
	  "^[ \t]*\\(#\\(?:if\\|else\\|elsif\\|end\\)\\)"; 1: gnatprep keywords.
	  end t)
    (cond
     ((match-beginning 1)
      (put-text-property
       (match-beginning 1) (match-end 1) 'syntax-table '(11 . ?\n)))
     )
    ))

(defun ada-compile-mouse-goto-error ()
  "Mouse interface for `ada-compile-goto-error'."
  (interactive)
  (mouse-set-point last-input-event)
  (ada-compile-goto-error (point))
  )

(defun ada-compile-goto-error (pos)
  ;; FIXME (later): do this in compilation-parse-error instead
  "Replace `compile-goto-error' from compile.el.
If POS is on a file and line location, go to this position.  It adds
to compile.el the capacity to go to a reference in an error message.
For instance, on these lines:
  foo.adb:61:11:  [...] in call to size declared at foo.ads:11
  foo.adb:61:11:  [...] in call to local declared at line 20
the 4 file locations can be clicked on and jumped to."
  (interactive "d")
  (goto-char pos)

  (skip-chars-backward "-a-zA-Z0-9_:./\\")
  (cond
   ;;  special case: looking at a filename:line not at the beginning of a line
   ;;  or a simple line reference "at line ..."
   ((and (not (bolp))
	 (or (looking-at ada-compile-goto-error-file-linenr-re)
	     (and
	      (save-excursion
		(beginning-of-line)
		(looking-at ada-compile-goto-error-file-linenr-re))
	      (save-excursion
		(if (looking-at "\\([0-9]+\\)") (backward-word 1))
		(looking-at "line \\([0-9]+\\)"))))
	     )
    (let ((line (if (match-beginning 2) (match-string 2) (match-string 1)))
	  (file (if (match-beginning 2) (match-string 1)
		  (save-excursion (beginning-of-line)
				  (looking-at ada-compile-goto-error-file-linenr-re)
				  (match-string 1))))
	  (error-pos (point-marker))
	  source)

      ;; set source marker
      (save-excursion
	(compilation-find-file (point-marker) (match-string 1) "./")
	(set-buffer file)

	(when (stringp line)
	  (goto-char (point-min))
	  (forward-line (1- (string-to-number line))))

	(setq source (point-marker)))

      (compilation-goto-locus error-pos source nil)

      ))

   ;; otherwise, default behavior
   (t
    (compile-goto-error))
   )
  (recenter))

  ;; (add-hook 'compilation-mode-hook
  ;; 	    (lambda()
  ;; 	      ;; FIXME (later): This has global impact!  -stef
  ;; 	      ;; add the extra error points in compilation-error-regexp
  ;; 	      ;; Mouse-2 is bound to compile-goto-error
  ;; 	      (define-key compilation-minor-mode-map [mouse-2]
  ;; 		'ada-compile-mouse-goto-error)
  ;; 	      (define-key compilation-minor-mode-map "\C-c\C-c"
  ;; 		'ada-compile-goto-error)
  ;; 	      (define-key compilation-minor-mode-map "\C-m"
  ;; 		'ada-compile-goto-error)))

(defun ada-gnat-setup ()
  (font-lock-add-keywords nil
   ;; gnatprep preprocessor line
   (list (list "^[ \t]*\\(#.*\n\\)"  '(1 font-lock-type-face t))))

  (add-hook 'ada-syntax-propertize-hook 'ada-gnat-syntax-propertize)

  (when (featurep 'ada-indent)
    ;; we don't use add-hook here, because we don't want the global value.
    (add-to-list 'smie-indent-functions 'ada-indent-gnatprep))
)

;; add at end, so it is after ada-indent-setup, and can modify smi-indent-functions
(add-hook 'ada-mode-hook 'ada-gnat-setup t)

(provide 'ada-gnat)
(provide 'ada-compiler)

;; end of file
