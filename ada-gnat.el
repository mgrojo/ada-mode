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

(require 'ada-fix-error)

;;;; gnatprep utils

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

;;; project file handling

(defun ada-gnat-prj-parse-emacs-file (name value project)
  "Handle gnat-specific Emacs Ada project file settings.
Return new PROJECT if NAME recognized, nil otherwise.
See also `ada-gnat-parse-emacs-prj-file-final'."
  (let ((process-environment (plist-get project 'proc_env))); for substitute-in-file-name
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
      (setq project
	    (plist-put project
		       'gpr_file
		       (expand-file-name (substitute-in-file-name value))))
      project)
     )))

(defun ada-gnat-prj-parse-emacs-final (project)
  "Final processing of gnat-specific Emacs Ada project file settings."
  (if (ada-prj-get 'gpr_file project)
      (set 'project (ada-gnat-parse-gpr (ada-prj-get 'gpr_file project) project))

    ;; add the compiler libraries to src_dir, obj_dir
    (setq project (ada-gnat-get-paths project))
    )

  (kill-buffer (ada-gnat-run-buffer-name)); things may have changed, force re-create

  ;; FIXME: This is only needed when actually running the gnat
  ;; compiler; parsing a gnat project is a crude approximation to
  ;; that. Could set in an 'ada-compile' function, but there's no good
  ;; way to know when to clear it. Same for
  ;; compilation-error-regexp-alist. So we do this here, and assume other modes will set
  ;; these variables appropriately.
  ;;
  ;; One possible approach is per-project compilation buffers; then
  ;; these variables could be buffer-local.
  ;;
  ;; Or can we use compilation-[start|finish]-functions to set and remove this?
  (setq compilation-filter-hook nil)
  (add-hook 'compilation-filter-hook 'ada-gnat-compilation-filter)

  ;; ada-mode.el project file parser sets this to other compilers used
  ;; in the project, so we only add here.
  (add-to-list 'compilation-error-regexp-alist 'gnat)

  project)

(defun ada-gnat-get-paths (project)
  "Add project and/or compiler source, object paths to PROJECT src_dir, obj_dir."
  (with-current-buffer (ada-gnat-run-buffer)
    (let ((status (ada-gnat-run "list" "-v"))
	  src-dirs
	  obj-dirs)

      ;; gnat list -P -v returns 0 in nominal cases
      ;; gnat list -v return 4, but still lists compiler dirs
      (when (not (member status '(0 4)))
	(pop-to-buffer (current-buffer))
	(error "gnat list returned status %d" status))

      (goto-char (point-min))

      (condition-case nil
	  (progn
	    ;; Source path
	    (search-forward "Source Search Path:")
	    (forward-line 1)
	    (while (not (looking-at "^$")) ; terminate on blank line
	      (back-to-indentation) ; skip whitespace forward
	      (if (looking-at "<Current_Directory>")
		  (add-to-list 'src-dirs  (directory-file-name default-directory))
		(add-to-list 'src-dirs
			     (expand-file-name ; canonicalize path part
			      (directory-file-name
			       (buffer-substring-no-properties (point) (point-at-eol))))))
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
	    )
	('error
	 (pop-to-buffer (current-buffer))
	 ;; search-forward failed; it already output a message
	 ))

      (setq project (plist-put project 'src_dir (reverse src-dirs)))
      (setq project (plist-put project 'obj_dir (reverse obj-dirs)))
      ))
  project)

(defun ada-gnat-parse-gpr (gpr-file project)
  "Append to src_dir and obj_dir in PROJECT by parsing GPR-FILE.
Return new value of PROJECT.
GPR-FILE must be full path to file, normalized.
src_dir, obj_dir will include compiler runtime."
  ;; this can take a long time; let the user know what's up
  (message "Parsing %s ..." gpr-file)

  (if (ada-prj-get 'gpr_file project)
      ;; gpr-file defined in Emacs Ada mode project file
      (when (not (equal gpr-file (ada-prj-get 'gpr_file project)))
	(error "Ada project file %s defines a different GNAT project file than %s"
	       ada-prj-current-file
	       gpr-file))

    ;; gpr-file is top level Ada mode project file
    (setq project (plist-put project 'gpr_file gpr-file))
    )

  (setq project (ada-gnat-get-paths project))
  (message "Parsing %s ... done" gpr-file)
  project)

;;; command line tool interface

(defun ada-gnat-run-buffer-name ()
  (concat " *gnat-run-"
	  (or (ada-prj-get 'gpr_file)
	      ada-prj-current-file)
	  "*"))

(defun ada-gnat-run-buffer ()
  "Return a buffer suitable for running gnat command line tools for the current project."
  (ada-require-project-file)
  (let* ((buffername (ada-gnat-run-buffer-name))
	 (buffer (get-buffer buffername)))
    (if buffer
	buffer
      (setq buffer (get-buffer-create buffername))
      (with-current-buffer buffer
	(setq default-directory
	      (file-name-directory
	       (or (ada-prj-get 'gpr_file)
		   ada-prj-current-file)))

	(let ((ada_project_path (ada-prj-get 'ada_project_path)))
	  (when ada_project_path
	    (add-to-list (make-variable-buffer-local 'process-environment)
			 (concat "ADA_PROJECT_PATH=" ada_project_path))))
	)
      buffer)))

(defun ada-gnat-run (command &rest switches-args)
  "Run the gnat command line tool, as \"gnat COMMAND -P<prj> SWITCHES-ARGS\".
Return process status.
Assumes current buffer is (ada-gnat-run-buffer)"
  (set 'buffer-read-only nil)
  (erase-buffer)

  (let* ((project-file-switch
	  (when (ada-prj-get 'gpr_file)
	    (concat "-P" (file-name-nondirectory (ada-prj-get 'gpr_file)))))
	 (cmd (append (list command project-file-switch) switches-args)))

    (insert (format "ADA_PROJECT_PATH=%s\ngnat " (getenv "ADA_PROJECT_PATH"))); for debugging
    (setq cmd (delete-if 'null cmd))
    (mapc (lambda (str) (insert (concat str " "))) cmd);; show command for debugging
    (newline)
    (apply 'call-process "gnat" nil t nil cmd)
    ))

(defun ada-gnat-run-no-prj (command &rest switches-args)
  "Run the gnat command line tool, as \"gnat COMMAND SWITCHES-ARGS\".
Return process status.
Assumes current buffer is (ada-gnat-run-buffer)"
  (set 'buffer-read-only nil)
  (erase-buffer)

  (let ((cmd (append (list command) switches-args)))

    (setq cmd (delete-if 'null cmd))
    (mapc (lambda (str) (insert (concat str " "))) cmd);; show command for debugging
    (newline)
    (apply 'call-process "gnat" nil t nil cmd)
    ))

;;; uses of gnat tools

(defconst ada-gnat-file-line-col-regexp "\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)")

(defun ada-gnat-xref (identifier parent)
  "Return '(file line column) for declaration or body for IDENTIFIER, which must be at point.
If PARENT is non-nil, return parent type declaration (assumes IDENTIFIER is a derived type)."
  (let* ((start-file (file-name-nondirectory (buffer-file-name)))
	 (start-line (line-number-at-pos))
	 (start-col  (1+ (current-column)))
	 (arg (format "%s:%s:%d:%d" identifier start-file start-line start-col))
	 (switch (if parent "-d" nil))
	 status
	 (result nil))
    (with-current-buffer (ada-gnat-run-buffer)
      (if switch
	  (setq status (ada-gnat-run "find" switch arg))
	(setq status (ada-gnat-run "find" arg)))

      (cond
       ((= status 0); success
	(goto-char (point-min))
	(forward-line 2); skip ADA_PROJECT_PATH, 'gnat find'

	;; gnat find returns two items; the starting point, and the 'other' point
	(while (not result)
	  (unless (looking-at (concat ada-gnat-file-line-col-regexp ":"))
	    ;; no results
	    (error "'%s' not found in cross-reference files; recompile?" identifier))
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
		    (equal start-file found-file)
		    (= start-line found-line)
		    (= start-col found-col)))
		  ;; found other item
		  (setq result (list found-file found-line (1- found-col)))
		(forward-line 1)))
	     )
	    (when (eobp)
	      (pop-to-buffer (current-buffer))
	      (error "gnat find did not return other item"))
	    )))

       (t ; failure
	(pop-to-buffer (current-buffer))
	(error "gnat find failed"))
       ))
    result))

(defun ada-gnat-file-name-from-ada-name (ada-name)
  "For `ada-file-name-from-ada-name'."
  (let* (status
	 (result nil))

    (while (string-match "\\." ada-name)
      (setq ada-name (replace-match "-" t t ada-name)))
    (downcase ada-name)

    (with-current-buffer (ada-gnat-run-buffer)
      (setq status
	    (ada-gnat-run-no-prj
	     "krunch"
	     ada-name
	     ;; "0" means only krunch GNAT library names
	     "0"))

      (cond
       ((= status 0); success
	(goto-char (point-min))
	(forward-line 1); skip  cmd
	(setq result (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
	)

       (t ; failure
	(pop-to-buffer (current-buffer))
	(error "gnat find failed"))
       ))
    result))

(defconst ada-gnat-predefined-package-alist
  '(("a-textio" . "Ada.Text_IO")
    ("a-chahan" . "Ada.Characters.Handling")
    ("a-comlin" . "Ada.Command_Line")
    ("a-except" . "Ada.Exceptions")
    ("a-numeri" . "Ada.Numerics")
    ("a-string" . "Ada.Strings")
    ("g-socket" . "GNAT.Sockets")
    ("interfac" . "Interfaces")
    ("i-c"      . "Interfaces.C")
    ("i-cstrin" . "Interfaces.C.Strings")
    ("s-stoele" . "System.Storage_Elements")
    ("unchconv" . "Unchecked_Conversion") ; Ada 83 name
    )
  "Alist (filename . package name) of GNAT file names for predefined Ada packages.")

(defun ada-gnat-ada-name-from-file-name (file-name)
  "For `ada-ada-name-from-file-name'."
  (let* (status
	 (ada-name (file-name-sans-extension (file-name-nondirectory file-name)))
	(predefined (cdr (assoc ada-name ada-gnat-predefined-package-alist))))

    (if predefined
        predefined
      (while (string-match "-" ada-name)
	(setq ada-name (replace-match "." t t ada-name)))
      ada-name)))

;;; compiler message handling

(defun ada-gnat-compilation-filter ()
  "Filter to add text properties to secondary file references.
For `compilation-filter-hook'."
  (save-excursion
    (goto-char compilation-filter-start)
    ;; compilation-filter might insert partial lines, or it might insert multiple lines
    (when (bolp)
      (while (not (eobp))
	;; We don't want 'next-error' to always go to this
	;; reference, so we _don't_ set 'compilation-message text
	;; property. Instead, we set 'ada-secondary-error, so
	;; `ada-goto-secondary-error' will handle it. We also set
	;; fonts, so the user can see the reference.

	;; c:/foo/bar/lookahead_test.adb:379:14: found type access to "Standard.String" defined at line 379
	(cond
	 ((looking-at "^\\(\\(.:\\)?[^ :\n]+\\):.* \\(at line \\)\\([0-9]+\\)")
	  (compilation--put-prop 3 'font-lock-face compilation-info-face); "at line" instead of file
	  (compilation--put-prop 4 'font-lock-face compilation-line-face); line
	  (with-silent-modifications
	    (put-text-property
	     (match-beginning 3) (match-end 3)
	     'ada-secondary-error
	     (list
	      (buffer-substring-no-properties (match-beginning 1) (match-end 1)); actual file
	      (string-to-number (buffer-substring-no-properties (match-beginning 4) (match-end 4))); line
	      1)); column
	    ))

	 (t
	  ;; lookahead_test.ads:23:09: "Name" has been inherited from subprogram at aunit-simple_test_cases.ads:47
	  ;;
	  ;; skip the primary reference, look for "*.ad?:nn"
	  (skip-syntax-forward "^-")
	  (when (search-forward-regexp "\\s-\\(\\([^[:blank:]]+\\.[[:alpha:]]+\\):\\([0-9]+\\)\\)"
				       (line-end-position) t)

	    (compilation--put-prop 2 'font-lock-face compilation-info-face); file
	    (compilation--put-prop 3 'font-lock-face compilation-line-face); line
	    (with-silent-modifications
	      (put-text-property
	       (match-beginning 0) (match-end 0)
	       'ada-secondary-error
	       (list
		(buffer-substring-no-properties (match-beginning 2) (match-end 2)); file
		(string-to-number (buffer-substring-no-properties (match-beginning 3) (match-end 3))); line
		1)); column
	      )))
	 )
	(forward-line 1))
      )))

(defun ada-gnat-debug-filter ()
  ;; call ada-gnat-compilation-filter with `compilation-filter-start' bound
  (interactive)
  (beginning-of-line)
  (let ((compilation-filter-start (point)))
    (ada-gnat-compilation-filter)))

;;; auto fix compilation errors

(defconst ada-gnat-quoted-name-regexp
  "\"\\([a-zA-Z0-9_.']+\\)\""
  "regexp to extract the quoted names in error messages")

(defconst ada-gnat-quoted-punctuation-regexp
  "\"\\([,:;=()|]+\\)\""
  "regexp to extract quoted punctuation in error messages")

(defun ada-gnat-fix-error (msg source-buffer source-window)
  "For `ada-fix-error-alist'."

  ;; Move to start of error message text
  (set-buffer compilation-last-buffer)
  (skip-syntax-forward "^-")
  (forward-char 1)

  ;; recognize it, handle it
  (unwind-protect
      (cond
       ;; It is tempting to define an alist of (MATCH . ACTION), but
       ;; that is too hard to debug
       ;;
       ;; This list will get long, so let's impose some order.
       ;;
       ;; First expressions that start with a named regexp, alphabetical by variable name.
       ;;
       ;; Then expressions that start with a string, alphabetical by string.
       ;;
       ;; Then style errors.

       ((looking-at (concat ada-gnat-quoted-name-regexp " is not visible"))
	(let ((ident (match-string 1)))
	  ;; next line may contain a reference to where ident is
	  ;; defined; if present, it will have been marked by
	  ;; ada-gnat-compilation-filter
	  (next-line 1)
	  (let* ((pos (next-single-property-change (point) 'ada-secondary-error nil (line-end-position)))
		 (item (when pos (get-text-property pos 'ada-secondary-error)))
		 (unit-file (when item (nth 0 item))))
	    (unless unit-file
	      (error "unrecognized error message"))
	    (pop-to-buffer source-buffer)
	    ;; We either need to add a with_clause for a package, or
	    ;; prepend the package name here (or add a use clause, but I
	    ;; don't want to do that automatically).
	    ;;
	    ;; If we need to add a with_clause, unit-name may be only
	    ;; the prefix of the real package name, but in that case
	    ;; we'll be back after the next compile; no way to get the
	    ;; full package name (without the function/type name) now.
	    ;; Note that we can't use gnat find, because the code
	    ;; doesn't compile.
	    (let ((unit-name (ada-ada-name-from-file-name unit-file)))
	      (cond
	       ((looking-at (concat unit-name "\\."))
                  (ada-fix-add-with-clause unit-name))
	       (t
		(ada-fix-insert-unit-name unit-name)
		(insert ".")))))))

       ((looking-at (concat ada-gnat-quoted-name-regexp " not declared in " ada-gnat-quoted-name-regexp))
	(let ((child-name (match-string 1))
	      correct-spelling)
	  ;; First check for "possible misspelling" message on next line
	  (save-excursion
	    (next-line 1)
	    (if (looking-at (concat "possible misspelling of " ada-gnat-quoted-name-regexp))
		;; correctable misspelling
		(progn
		  (setq correct-spelling (match-string 1))
		  (pop-to-buffer source-buffer)
		  (search-forward child-name)
		  (replace-match correct-spelling)
		  );; progn
	      ;; else guess that "child" is a child package, and extend the with_clause
	      (pop-to-buffer source-buffer)
	      (ada-fix-extend-with-clause child-name)))))

       ((looking-at "expected an access type")
	(progn
	  (set-buffer source-buffer)
	  (backward-char 1)
	  (if (looking-at "\\.all")
	      (delete-char 4)
	    (ding))))

       ((looking-at (concat "missing " ada-gnat-quoted-punctuation-regexp))
	(let ((stuff (match-string-no-properties 1)))
	  (set-buffer source-buffer)
	  (insert stuff)))

       ((looking-at (concat "missing \"with \\([a-zA-Z0-9_.']+\\);\""))
	(let ((package-name (match-string-no-properties 1)))
	  (pop-to-buffer source-buffer)
	  (ada-fix-add-with-clause package-name)))

       (t
	(error "error not recognized"))
       );; end of 'cond'
    ;; restore compilation buffer point
    (set-buffer compilation-last-buffer)
    (compilation-next-error 0)
  ))

;;; setup

(defun ada-gnat-setup ()
  (set (make-variable-buffer-local 'ada-compiler) 'gnat)

  (set (make-variable-buffer-local 'ada-file-name-from-ada-name) 'ada-gnat-file-name-from-ada-name)
  (set (make-variable-buffer-local 'ada-ada-name-from-file-name) 'ada-gnat-ada-name-from-file-name)

  (font-lock-add-keywords nil
   ;; gnatprep preprocessor line
   (list (list "^[ \t]*\\(#.*\n\\)"  '(1 font-lock-type-face t))))

  (add-hook 'ada-syntax-propertize-hook 'ada-gnat-syntax-propertize)

  (when (featurep 'ada-smie)
    ;; we don't use add-hook here, because we don't want the global value.
    (add-to-list 'smie-indent-functions 'ada-gnatprep-indent))

  (when (featurep 'ada-smie)
    (set (make-variable-buffer-local 'ada-fix-context-clause) 'ada-smie-context-clause))
)

;; add at end, so it is after ada-smie-setup, and can modify smie-indent-functions
(add-hook 'ada-mode-hook 'ada-gnat-setup t)

(setq-default ada-compiler 'gnat)

;; don't need ada-prj-default-function
(add-to-list 'ada-xref-function        (cons 'gnat 'ada-gnat-xref))
(add-to-list 'ada-prj-parser-alist     (cons "gpr" 'ada-gnat-parse-gpr))
(add-to-list 'ada-prj-parse-file-ext   (cons 'gnat 'ada-gnat-prj-parse-emacs-file))
(add-to-list 'ada-prj-parse-file-final (cons 'gnat 'ada-gnat-prj-parse-emacs-final))
(add-to-list 'ada-fix-error-alist      (cons 'gnat 'ada-gnat-fix-error))

(add-to-list
 'compilation-error-regexp-alist-alist
 '(gnat
   ;; typical:
   ;;   cards_package.adb:45:32: expected private type "System.Address"
   ;;
   ;; with full path Source_Reference pragma :
   ;;   d:/maphds/version_x/1773/sbs-abi-dll_lib.ads.gp:39:06: file "interfaces_c.ads" not found
   ;;
   ;; gnu cc1:
   ;;   foo.c:2: `TRUE' undeclared here (not in a function)
   ;;   foo.c:2 : `TRUE' undeclared here (not in a function)
   "^\\(\\(.:\\)?[^ :\n]+\\):\\([0-9]+\\)\\s-?:?\\([0-9]+\\)?" 1 3 4))

;; gnatmake -gnatD generates files with .dg extensions. But we don't
;; need to navigate between them.
;;
;; There is no common convention for a file extension for gnatprep files.

(provide 'ada-gnat)
(provide 'ada-compiler)

;; end of file
