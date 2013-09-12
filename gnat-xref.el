;;; gnat-xref.el --- minor-mode for navigating sources using GNAT
;;; cross reference tool.
;;
;;; Copyright (C) 2013  Free Software Foundation, Inc.

;; Author: Stephen Leake <stephen_leake@member.fsf.org>
;; Maintainer: Stephen Leake <stephen_leake@member.fsf.org>
;; Keywords: languages ada

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Usage:
;;
;; M-x gnat-xref

(defgroup gnat-xref nil
  "Minor mode for navigating sources using GNAT cross reference tool `gnatinspect'."
  :group 'languages)

(defun gnat-xref-other (identifier file line col parent)
  "For `ada-xref-other-function', using gnatinspect."
  (unless (ada-prj-get 'gpr_file)
    (error "no gnat project file defined."))

  (with-current-buffer (ada-gnat-run-buffer)
    (let* ((project-file (file-name-nondirectory (ada-prj-get 'gpr_file)))
	   (arg (format "%s:%s:%d:%d" identifier file line col))
	   status
	   (result nil))

      (setq status (ada-gnat-run "gnatinspect" (list "-P" project-file "-c" (concat "refs " arg))))

      (cond
       ((= status 0); success
	(goto-char (point-min))
	(forward-line 2); skip ADA_PROJECT_PATH, echoed command

	;; FIXME: change to gnatinspect, handle parent

	;; gnat find returns two items; the starting point, and the 'other' point
	(while (not result)
	  (unless (looking-at (concat ada-gnat-file-line-col-regexp ":"))
	    ;; no results
	    (error "'%s' not found in cross-reference files; recompile?" identifier))
	  (if (looking-at (concat ada-gnat-file-line-col-regexp ": warning:"))
	      ;; error in *.gpr; ignore here.
	      (forward-line 1)
	    ;; else process line
	    (let ((found-file (match-string 1))
		  (found-line (string-to-number (match-string 2)))
		  (found-col  (string-to-number (match-string 3))))
	      (cond
	       (parent
		(skip-syntax-forward "^ ")
		(skip-syntax-forward " ")
		(if (looking-at (concat "derived from .* (" ada-gnat-file-line-col-regexp ")"))
		    ;; found other item
		    (setq result (list (match-string 1)
				       (string-to-number (match-string 2))
				       (1- (string-to-number (match-string 3)))))
		  (forward-line 1)))

	       (t
		(if (not
		     (and
		      (equal file found-file)
		      (= line found-line)
		      (= col found-col)))
		    ;; found other item
		    (setq result (list found-file found-line (1- found-col)))
		  (forward-line 1)))
	       )
	      (when (eobp)
		(pop-to-buffer (current-buffer))
		(error "gnat find did not return other item"))
	      ))))

       (t ; failure
	(pop-to-buffer (current-buffer))
	(error "gnat find failed"))
       ))
    result))

(defun ada-gnat-xref-all (identifier file line col)
  "For `ada-xref-all-function'."
  ;; we use `compilation-start' to run gnat, not `ada-gnat-run', so it
  ;; is asynchronous, and automatically runs the compilation error
  ;; filter.

  (let* ((cmd (format "gnat find -r %s:%s:%d:%d" identifier file line col)))

    (with-current-buffer (ada-gnat-run-buffer); for process-environment
      (let ((compilation-environment process-environment)
	    (compilation-error "reference"))
	(when (ada-prj-get 'gpr_file)
	  (setq cmd (concat cmd " -P" (file-name-nondirectory (ada-prj-get 'gpr_file)))))

	(compilation-start cmd)
    ))))


(defun gnat-xref-goto-declaration (other-window-frame &optional parent)
  "Move to the declaration or body of the identifier around point.
If at the declaration, go to the body, and vice versa.

OTHER-WINDOW-FRAME (default nil, set by interactive prefix)
controls window and frame choice:

nil     : show in current window
C-u     : show in other window
C-u C-u : show in other frame
"
  (interactive "P")
  (let ((target (ada-gnat-xref-other
		 (thing-at-point 'symbol)
		 (file-name-nondirectory (buffer-file-name))
		 (line-number-at-pos)
		 (car (bounds-of-thing-at-point 'symbol))
		 parent)))

    (ada-goto-source (nth 0 target)
		     (nth 1 target)
		     (nth 2 target)
		     other-window-frame)
    ))

(defun gnat-xref-show-references ()
  "Show declaration and all references of identifier at point."
  (interactive)
  (ada-gnat-xref-all
   (symbol-at-point)
   (file-name-nondirectory (buffer-file-name))
   (line-number-at-pos)
   (cl-case (char-after)
     (?\" (+ 2 (current-column))) ;; work around bug in gnat find
     (t (1+ (current-column)))))
  )

(defun gnat-xref-goto-declaration-parent ()
  "Move to the parent type declaration of the type identifier around point."
  (interactive)
  (gnat-xref-goto-declaration t))

(defvar-local gnat-xref-map
  (let ((map (make-sparse-keymap)))
    ;; C-c <letter> are reserved for users

    (define-key map "\C-c\C-d" 'gnat-xref-goto-declaration)
    (define-key map "\C-c\M-d" 'gnat-xref-goto-declaration-parent)
    (define-key map "\C-c\C-r" 'gnat-xref-all-references)
    map
  )  "Local keymap used for GNAT xref minor mode.")

(define-minor-mode gnat-xref
  "Minor mode for navigating sources using GNAT cross reference tool."
  :initial-value t
  :lighter       " gnat-xref"   ;; mode line
  )

;;; end of file
