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

;;; gnatprep utils

(defun ada-gnatprep-indent ()
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
  ;;
  ;; This function works on smie-indent-functions; it will probably
  ;; work in other indentation engines as well, so we call it
  ;; 'ada-gnatprep-indent
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

;;; command line tool interface

(defvar ada-gnat-gpr-file nil
  "Absolute filename of the current GNAT project file.")

(defun ada-gnat-require-project-file ()
  "If no GNAT project file has been set, find one, parse it, set it."
  (unless ada-gnat-gpr-file
    (ada-require-project-file)
    (unless ada-gnat-gpr-file
      (error "Ada project file '%s' did not define GNAT project file"))))

(defun ada-gnat-prj-parse-emacs-file (name value project)
  "Handle gnat-specific Emacs Ada project file settings.
Return new PROJECT if NAME recognized, nil otherwise.
See also `ada-gnat-parse-emacs-prj-file-final'."
  (cond
   ((string= name "ada_project_path")
    (let ((path-current (plist-get project 'ada_project_path))
	  (path-add (expand-file-name (substitute-in-file-name value)))
	  (sep (plist-get project 'path_sep)))
      (setq project
	   (plist-put project
		      'ada_project_path
		      (concat path-current sep path-add)))
      project))

   ((string= (match-string 1) "gpr_file")
    ;; The file is parsed in `ada-gnat-parse-emacs-prj-file-final', so
    ;; it can add to user-specified src_dir.
    (setq ada-gnat-gpr-file (expand-file-name (substitute-in-file-name value)))
    (setq project (plist-put project 'gpr_file ada-gnat-gpr-file))
    project)
   ))

(defun ada-gnat-parse-emacs-prj-final (project)
  "Final processing of gnat-specific Emacs Ada project file settings."
  (if ada-gnat-gpr-file
      (set 'project (ada-gnat-parse-gpr project))
      ))
  project)

(defun ada-gnat-parse-gpr (project)
  "Append to src_dir and obj_dir in PROJECT by parsing `ada-gnat-gpr-file'.
Return new value of PROJECT.
GPR_FILE must be full path to file, normalized.
src_dir, obj_dir will include compiler runtime."
  ;; this can take a long time; let the user know what's up
  (message "Parsing %s ..." ada-gnat-gpr-file)
  (with-current-buffer (ada-gnat-run-buffer)
    (let ((status (ada-gnat-run "list" "-v"))
	  src-dirs
	  obj-dirs)
      (goto-char (point-min))

      ;; gnat list -P -v returns 0 in nominal cases
      (when (/= 0 status)
	(pop-to-buffer (current-buffer))
	(error "gnat list -P -v returned status %d" status))

      ;; Source path
      (search-forward "Source Search Path:")
      (forward-line 1)
      (while (not (looking-at "^$")) ; terminate on blank line
	(back-to-indentation) ; skip whitespace forward
	(if (looking-at "<Current_Directory>")
	    (add-to-list 'src-dirs  default-directory)
	  (add-to-list 'src-dirs
		       (expand-file-name ; canonicalize path part
			(buffer-substring-no-properties (point) (point-at-eol)))))
	(forward-line 1))

      ;;  Object path

      (search-forward "Object Search Path:")
      (forward-line 1)
      (while (not (looking-at "^$"))
	(back-to-indentation)
	(if (looking-at "<Current_Directory>")
	    (add-to-list 'obj-dirs ".")
	  (add-to-list 'obj-dirs
		       (expand-file-name
			(buffer-substring-no-properties (point) (point-at-eol)))))
	(forward-line 1))

      (setq project (plist-put project 'src_dir (reverse src-dirs)))
      (setq project (plist-put project 'obj_dir (reverse obj-dirs)))
    ))
  (message "Parsing %s ... done" ada-gnat-gpr-file)
  project)

(defun ada-gnat-run-buffer ()
  "Return a buffer suitable for running gnat command line tools for the current project."
  (ada-gnat-require-project-file)
  (let* ((buffername (concat " *gnat-run-"
			     (file-name-nondirectory ada-gnat-gpr-file)
			     "*"))
	 (buffer (get-buffer buffername)))
    (if buffer
	buffer
      (setq buffer (get-buffer-create buffername))
      (with-current-buffer buffer
	(setq default-directory (file-name-directory ada-gnat-gpr-file))

	(let ((ada_project_path (plist-get (ada-gnat-current-project) 'project_path)))
	  (when ada_project_path
	    (add-to-list (make-buffer-local-variable 'process-environment)
			 (concat "ADA_PROJECT_PATH=" ada_project_path))))
	)
      buffer)))

(defun ada-gnat-run (command &rest switches-args)
  "Run the gnat command line tool, as \"gnat COMMAND -P<prj> SWITCHES-ARGS\".
Return process status.
Assumes current buffer is (ada-gnat-run-buffer)"
  (set 'buffer-read-only nil)
  (erase-buffer)

  (let ((project-file-switch
	 (concat "-P" (file-name-nondirectory ada-gnat-gpr-file))))

    (apply 'call-process "gnat" (append (list nil t nil command project-file-switch) switches-args))
    ))

;;; cross reference handling

(defun ada-gnat-xref (identifier parent)
  "Return '(file line column) for declaration or body for IDENTIFIER, which must be at point.
If PARENT is non-nil, return parent type declaration (assumes IDENTIFIER is a derived type)."
  (let ((arg (format "%s:%s:%d:%d" identifier (buffer-file-name) (line-number-at-pos) (current-column)))
	(switch (if parent "-d" nil)))
    (with-current-buffer (ada-gnat-run-buffer)
      (if switch
	  (ada-gnat-run "find" switch arg)
	(ada-gnat-run "find" arg))

      ;; FIXME: parse result
      )))

;;; compiler message handling

(defun ada-compile-mouse-goto-error ()
  "Mouse interface for `ada-compile-goto-error'."
  (interactive)
  (mouse-set-point last-input-event)
  (ada-compile-goto-error (point))
  )

(defun ada-compile-goto-error (pos)
  ;; FIXME (later): do this in compilation-parse-error instead
  "Replace `compile-goto-error' from compile.el.
If POS is on a file and line location, go to this position.  It
adds to compile.el the capacity to go to an extra reference in an
error message.  For instance, on these lines:

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

;;; setup

(defun ada-gnat-setup ()
  (set (make-variable-buffer-local 'ada-compiler) 'gnat)

  (font-lock-add-keywords nil
   ;; gnatprep preprocessor line
   (list (list "^[ \t]*\\(#.*\n\\)"  '(1 font-lock-type-face t))))

  (add-hook 'ada-syntax-propertize-hook 'ada-gnat-syntax-propertize)

  (when (featurep 'ada-smie)
    ;; we don't use add-hook here, because we don't want the global value.
    (add-to-list 'smie-indent-functions 'ada-gnatprep-indent))

  ;; don't need ada-prj-default-function

  (add-to-list 'ada-xref-function (cons 'gnat 'ada-gnat-xref))
  (add-to-list 'ada-prj-parser-alist (cons "gpr" 'ada-gnat-parse-gpr))
  (add-to-list 'ada-prj-parse-file-ext (cons 'gnat 'ada-gnat-prj-parse-emacs-file))
  (add-to-list 'ada-prj-parse-file-final (cons 'gnat 'ada-gnat-parse-emacs-prj-final))
)

;; add at end, so it is after ada-smie-setup, and can modify smi-indent-functions
(add-hook 'ada-mode-hook 'ada-gnat-setup t)

;; gnatmake -gnatD generates files with .dg extensions. But we don't
;; need to navigate between them.
;;
;; There is no common convention for a file extension for gnatprep files.

(provide 'ada-gnat)
(provide 'ada-compiler)

;; end of file
