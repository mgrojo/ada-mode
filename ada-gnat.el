;; Ada mode functionality specific to the GNAT compiler
;;
;; GNAT is provided by AdaCore; see http://libre.adacore.com/
;;
;;; Copyright (C) 2012, 2013  Free Software Foundation, Inc.
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
(require 'compile)

;; We use cl-delete-if, defined in cl-seq.el. cl-seq.el has no
;; 'provide'.  autoload for cl-delete-if is defined in
;; cl-loaddefs.el, which is not pre-loaded, so we load it here.
;; FIXME: asking on emacs-devel if this is the right way
(eval-and-compile (load "cl-loaddefs.el"))

;;;;; code
;;;; gnatprep utils

(defun ada-gnatprep-indent ()
  "If point is on a gnatprep keyword, return indentation column
for it. Otherwise return nil.  Intended to be added to
`wisi-indent-calculate-functions' or other indentation function
list."
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
	  "^[ \t]*\\(#\\(?:if\\|else\\|elsif\\|end\\)\\)"; gnatprep keywords.
	  end t)
    (cond
     ((match-beginning 1)
      (put-text-property
       (match-beginning 1) (match-end 1) 'syntax-table '(11 . ?\n)))
     )
    ))

;;;; project file handling

(defun ada-gnat-prj-add-prj-dir (dir project)
  "Add DIR to 'prj_dir and ADA_PROJECT_PATH in 'proc_env. Return new project."
  (let ((prj-dir (plist-get project 'prj_dir))
	(proc-env (plist-get project 'proc_env)))

    (cond
     ((listp prj-dir)
	(add-to-list 'prj-dir dir))

     (prj-dir
      (setq prj-dir (list dir)))

     (t nil))

    (setq project (plist-put project 'prj_dir prj-dir))

    (add-to-list 'proc-env
		 (concat "ADA_PROJECT_PATH="
			 (mapconcat 'identity
				    (plist-get project 'prj_dir)
				    (plist-get project 'path_sep))))

    (setq project (plist-put project 'proc_env proc-env))

    project))

(defun ada-gnat-prj-parse-emacs-file (name value project)
  "Handle gnat-specific Emacs Ada project file settings.
Return new PROJECT if NAME recognized, nil otherwise.
See also `ada-gnat-parse-emacs-final'."
  (let ((process-environment (plist-get project 'proc_env))); for substitute-in-file-name
    (cond
     ((string= name "ada_project_path")
      ;; We maintain two project values for this;
      ;; 'prj_dir - a list of directories, for gpr-ff-special-with
      ;; ADA_PROJECT_PATH in 'proc_env, for ada-gnat-run
      (ada-gnat-prj-add-prj-dir (expand-file-name (substitute-in-file-name value)) project))

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
  (when (buffer-live-p (get-buffer (ada-gnat-run-buffer-name)))
    (kill-buffer (ada-gnat-run-buffer-name))); things may have changed, force re-create

  (if (ada-prj-get 'gpr_file project)
      (set 'project (ada-gnat-parse-gpr (ada-prj-get 'gpr_file project) project))

    ;; add the compiler libraries to src_dir, obj_dir
    (setq project (ada-gnat-get-paths project))
    )

  ;; This is only needed when actually running the gnat compiler;
  ;; parsing a gnat project is a crude proxy for that. Could set in an
  ;; 'ada-compile' function, but there's no good way to know when to
  ;; clear it. Same for compilation-error-regexp-alist. So we do this
  ;; here, and assume other modes will set these variables
  ;; appropriately.
  ;;
  ;; One possible approach is per-project compilation buffers; then
  ;; these variables could be buffer-local.
  ;;
  ;; Or can we use compilation-[start|finish]-functions to set and remove this?
  (setq compilation-filter-hook nil)
  (add-hook 'compilation-filter-hook 'ada-gnat-compilation-filter)

  (cond
   ((boundp 'compilation-filter-start)
    ;; emacs 24.x manages compilation-filter-start
    nil)

   (t
    ;; emacs 23.4
    (add-hook 'compilation-start-hook 'ada-gnat-compilation-start))
   )

  ;; ada-mode.el project file parser sets this to other compilers used
  ;; in the project, so we only add here.
  (add-to-list 'compilation-error-regexp-alist 'gnat)

  project)

(defun ada-gnat-get-paths (project)
  "Add project and/or compiler source, object paths to PROJECT src_dir, obj_dir."
  (with-current-buffer (ada-gnat-run-buffer)
    (let ((status (ada-gnat-run (list "list" "-v")))
	  (src-dirs (ada-prj-get 'src_dir project))
	  (obj-dirs (ada-prj-get 'obj_dir project))
	  (prj-dirs (ada-prj-get 'prj_dir project)))

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

	    ;; Project path
	    (search-forward "Project Search Path:")
	    (forward-line 1)
	    (while (not (looking-at "^$"))
	      (back-to-indentation)
	      (if (looking-at "<Current_Directory>")
		  (add-to-list 'prj-dirs ".")
		(add-to-list 'prj-dirs
			     (expand-file-name
			      (buffer-substring-no-properties (point) (point-at-eol)))))
	      (forward-line 1))

	    )
	('error
	 (pop-to-buffer (current-buffer))
	 ;; search-forward failed
	 (error "parse gpr failed")
	 ))

      (setq project (plist-put project 'src_dir (reverse src-dirs)))
      (setq project (plist-put project 'obj_dir (reverse obj-dirs)))
      (setq project (plist-put project 'prj_dir (reverse prj-dirs)))
      ))
  project)

(defun ada-gnat-parse-gpr (gpr-file project)
  "Append to src_dir, obj_dir and prj_dir in PROJECT by parsing GPR-FILE.
Return new value of PROJECT.
GPR-FILE must be full path to file, normalized.
src_dir, obj_dir, prj_dir will include compiler runtime."
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

;;;; command line tool interface

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
	)
      buffer)))

(defun ada-gnat-run (command &optional switches-args)
  "Run the gnat command line tool, as \"gnat COMMAND -P<prj> SWITCHES-ARGS\".
Return process status.
Assumes current buffer is (ada-gnat-run-buffer)"
  (set 'buffer-read-only nil)
  (erase-buffer)

  (let* ((project-file-switch
	  (when (ada-prj-get 'gpr_file)
	    (concat "-P" (file-name-nondirectory (ada-prj-get 'gpr_file)))))
	 (cmd (append command (list project-file-switch) switches-args)))

    (setq cmd (cl-delete-if 'null cmd))

    (insert (format "ADA_PROJECT_PATH=%s\ngnat " (getenv "ADA_PROJECT_PATH"))); for debugging
    (mapc (lambda (str) (insert (concat str " "))) cmd);; show command for debugging
    (newline)
    (let ((process-environment (ada-prj-get 'proc_env)))
      (apply 'call-process "gnat" nil t nil cmd)
    )))

(defun ada-gnat-run-no-prj (command &optional dir)
  "Run the gnat command line tool, as \"gnat COMMAND\", with DIR as current directory.
Return process status.  Assumes current buffer
is (ada-gnat-run-buffer)"
  (set 'buffer-read-only nil)
  (erase-buffer)

  (let ((default-directory (or dir default-directory)))

    (setq command (cl-delete-if 'null command))
    (mapc (lambda (str) (insert (concat str " "))) command)
    (newline)
    (apply 'call-process "gnat" nil t nil command)
    ))

;;;; uses of gnat tools

(defconst ada-gnat-file-line-col-regexp "\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)")

(defun ada-gnat-xref-other (identifier file line col parent)
  "For `ada-xref-other-function'."
  (let* ((arg (format "%s:%s:%d:%d" identifier file line col))
	 (switches (concat
		  (when parent "-d")
		  (when (ada-prj-get 'gpr_ext) (concat "--ext=" (ada-prj-get 'gpr_ext)))
		  ))
	 status
	 (result nil))
    (with-current-buffer (ada-gnat-run-buffer)
      (if (< 0 (length switches))
	  (setq status (ada-gnat-run (list "find" switches arg)))
	(setq status (ada-gnat-run (list "find" arg))))

      (cond
       ((= status 0); success
	(goto-char (point-min))
	(forward-line 2); skip ADA_PROJECT_PATH, 'gnat find'

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

(defun ada-gnat-file-name-from-ada-name (ada-name)
  "For `ada-file-name-from-ada-name'."
  (let* (status
	 (result nil))

    (while (string-match "\\." ada-name)
      (setq ada-name (replace-match "-" t t ada-name)))
    (setq ada-name (downcase ada-name))

    (with-current-buffer (ada-gnat-run-buffer)
      (setq status
	    (ada-gnat-run-no-prj
	     (list
	      "krunch"
	      ada-name
	      ;; "0" means only krunch GNAT library names
	      "0")))

      (cond
       ((= status 0); success
	(goto-char (point-min))
	(forward-line 1); skip  cmd
	(setq result (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
	)

       (t ; failure
	(pop-to-buffer (current-buffer))
	(error "gnat krunch failed"))
       ))
    result))

(defconst ada-gnat-predefined-package-alist
  '(("a-textio" . "Ada.Text_IO")
    ("a-chahan" . "Ada.Characters.Handling")
    ("a-comlin" . "Ada.Command_Line")
    ("a-except" . "Ada.Exceptions")
    ("a-numeri" . "Ada.Numerics")
    ("a-string" . "Ada.Strings")
    ("a-strmap" . "Ada.Strings.Maps")
    ("a-strunb" . "Ada.Strings.Unbounded")
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

(defun ada-gnat-make-package-body (body-file-name)
  "For `ada-make-package-body'."
  ;; WORKAROUND: gnat stub 7.1w does not accept aggregate project files,
  ;; and doesn't use the gnatstub package if it is in a 'with'd
  ;; project file; see AdaCore ticket LC30-001. On the other hand we
  ;; need a project file to specify the source dirs so the tree file
  ;; can be generated. So we use ada-gnat-run-no-prj, and the user
  ;; must specify the proper project file in gnat_stub_opts.
  ;;
  ;; gnatstub always creates the body in the current directory (in the
  ;; process where gnatstub is running); the -o parameter may not
  ;; contain path info. So we pass a directory to ada-gnat-run-no-prj.
  (let ((start-buffer (current-buffer))
	(start-file (buffer-file-name))
	;; can also specify gnat stub options/switches in .gpr file, in package 'gnatstub'.
	(opts (when (ada-prj-get 'gnat_stub_opts)
		(split-string (ada-prj-get 'gnat_stub_opts))))
	(switches (when (ada-prj-get 'gnat_stub_switches)
		    (split-string (ada-prj-get 'gnat_stub_switches))))
	status)

    ;; Make sure all relevant files are saved to disk. This also saves
    ;; the bogus body buffer created by ff-find-the-other-file, so we
    ;; need -f gnat stub option. We won't get here if there is an
    ;; existing body file.
    (save-some-buffers t)
    (add-to-list 'opts "-f")
    (with-current-buffer (ada-gnat-run-buffer)
      (setq status
	    (ada-gnat-run-no-prj
	     (append (list "stub") opts (list start-file "-cargs") switches)
	     (file-name-directory body-file-name)))

      (cond
       ((= status 0); success - fix indentation
	(find-file body-file-name)
	(indent-region (point-min) (point-max))
	(save-buffer)
	(set-buffer start-buffer))

       (t ; failure
	(pop-to-buffer (current-buffer))
	(delete-file body-file-name);; created by find-file
	(error "gnat stub failed"))
       ))
    nil))

;;;; compiler message handling

(defvar ada-compilation-filter-start (make-marker)
  "Implement `compilation-filter-start' for emacs 23.4.")

(defun ada-gnat-compilation-start (proc)
  "Implement `compilation-filter-start' for emacs 23.4."
  (set-marker ada-compilation-filter-start (point-max)))

(when (not (functionp 'compilation--put-prop))
  (defun compilation--put-prop (matchnum prop val)
    (when (and (integerp matchnum) (match-beginning matchnum))
      (put-text-property
       (match-beginning matchnum) (match-end matchnum)
       prop val)))
  )

(defun ada-gnat-compilation-filter ()
  "Filter to add text properties to secondary file references.
For `compilation-filter-hook'."
  (save-excursion
    (cond
     ((boundp 'compilation-filter-start)
      ;; emacs 24.x
      (goto-char compilation-filter-start))

     (t
      ;; emacs 23.4
      (goto-char ada-compilation-filter-start))
     )

    ;; compilation-filter might insert partial lines, or it might insert multiple lines
    (when (bolp)
      (while (not (eobp))
	;; We don't want 'next-error' to always go to secondary
	;; references, so we _don't_ set 'compilation-message text
	;; property. Instead, we set 'ada-secondary-error, so
	;; `ada-goto-secondary-error' will handle it. We also set
	;; fonts, so the user can see the reference.

	;; typical secondary references look like:
	;;
	;; trivial_productions_test.adb:57:77:   ==> in call to "Get" at \
	;;    opentoken-token-enumerated-analyzer.ads:88, instance at line 41
	;;
	;; c:/foo/bar/lookahead_test.adb:379:14: found type access to "Standard.String" defined at line 379
	;;
	;; lookahead_test.ads:23:09: "Name" has been inherited from subprogram at aunit-simple_test_cases.ads:47
	;;
	;; lalr.adb:668:37: non-visible declaration at analyzer.ads:60, instance at parser.ads:38
	;;
	;; save the file from the primary reference, look for "*.ad?:nn", "at line nnn"

	(let (file)
	  (when (looking-at "^\\(\\(.:\\)?[^ :\n]+\\):")
	    (setq file (match-string-no-properties 1)))

	  (skip-syntax-forward "^-"); space following primary reference

	  (while (search-forward-regexp "\\s-\\(\\([^[:blank:]]+\\.[[:alpha:]]+\\):\\([0-9]+\\)\\)"
					(line-end-position) t)

	    (goto-char (match-end 0))
	    (with-silent-modifications
	      (compilation--put-prop 2 'font-lock-face compilation-info-face); file
	      (compilation--put-prop 3 'font-lock-face compilation-line-face); line
	      (put-text-property
	       (match-beginning 0) (match-end 0)
	       'ada-secondary-error
	       (list
		(match-string-no-properties 2); file
		(string-to-number (match-string-no-properties 3)); line
		1)); column
	      ))

	  (when (search-forward-regexp "\\(at line \\)\\([0-9]+\\)" (line-end-position) t)
	    (with-silent-modifications
	      (compilation--put-prop 1 'font-lock-face compilation-info-face); "at line" instead of file
	      (compilation--put-prop 2 'font-lock-face compilation-line-face); line
	      (put-text-property
	       (match-beginning 1) (match-end 1)
	       'ada-secondary-error
	       (list
		file
		(string-to-number (match-string-no-properties 2)); line
		1)); column
	      ))
	  (forward-line 1))
	))

    (cond
     ((boundp 'compilation-filter-start)
      ;; emacs 24.x manages compilation-filter-start
      nil)

     (t
      ;; emacs 23.4
      (set-marker ada-compilation-filter-start (point)))
     )
    ))

(defun ada-gnat-debug-filter ()
  ;; call ada-gnat-compilation-filter with `compilation-filter-start' bound
  (interactive)
  (beginning-of-line)
  (let ((compilation-filter-start (point)))
    (ada-gnat-compilation-filter)))

;;;;; auto fix compilation errors

(defconst ada-gnat-quoted-name-regexp
  "\"\\([a-zA-Z0-9_.']+\\)\""
  "regexp to extract the quoted names in error messages")

(defconst ada-gnat-quoted-punctuation-regexp
  "\"\\([,:;=()|]+\\)\""
  "regexp to extract quoted punctuation in error messages")

(defvar ada-gnat-fix-error-hook nil
  "For `ada-fix-error-alist'.")

(defun ada-gnat-misspelling ()
  "Return correct spelling from current compiler error, if there are corrections offered.
Prompt user if more than one."
  ;; wisi-output.adb:115:41: no selector "Productions" for type "RHS_Type" defined at wisi.ads:77
  ;; wisi-output.adb:115:41: invalid expression in loop iterator
  ;; wisi-output.adb:115:42: possible misspelling of "Production"
  ;; wisi-output.adb:115:42: possible misspelling of "Production"
  ;;
  ;; column number can vary, so only check the line number

  ;; FIXME (emacs 23): compilation--message->loc not in Emacs 23.2
  (let ((line (progn (beginning-of-line) (nth 1 (compilation--message->loc (ada-get-compilation-message)))))
	done choices)
    (while (not done)
      (forward-line 1)
      (setq done (or (not (ada-get-compilation-message))
		     (not (equal line (nth 1 (compilation--message->loc (ada-get-compilation-message)))))))
      (when (and (not done)
		 (progn
		   (skip-syntax-forward "^-")
		   (forward-char 1)
		   (looking-at (concat "possible misspelling of " ada-gnat-quoted-name-regexp))))
	(push (match-string 1) choices)))

    ;; return correct spelling
    (cond
     ((= 0 (length choices))
      nil)

     ((= 1 (length choices))
      (car choices))

     (t ;; multiple choices
      (completing-read "correct spelling: " choices))
     )))

(defun ada-gnat-fix-error (msg source-buffer source-window)
  "For `ada-gnat-fix-error-hook'."
  (let ((start-pos (point))
	message-column
	result)
    ;; Move to start of error message text
    (skip-syntax-forward "^-")
    (forward-char 1)
    (setq message-column (current-column))

    ;; recognize it, handle it
    (setq
     result
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
	   (let ((ident (match-string 1))
		 (done nil)
		 (file-line-struct (progn (beginning-of-line) (ada-get-compilation-message)))
		 pos choices unit-name)
	     ;; next line may contain a reference to where ident is
	     ;; defined; if present, it will have been marked by
	     ;; ada-gnat-compilation-filter
	     ;;
	     ;; the lines after that may contain alternate matches;
	     ;; collect all, let user choose.
	     (while (not done)
	       (forward-line 1)
	       (setq done (not
			   (and
			    (equal file-line-struct (ada-get-compilation-message))
			    (let ((limit (1- (line-end-position))))
			      ;; 1- because next compilation error is at next line beginning
			      (setq pos (next-single-property-change (point) 'ada-secondary-error nil limit))
			      (< pos limit)))))
	       (when (not done)
		 (let* ((item (get-text-property pos 'ada-secondary-error))
			(unit-file (nth 0 item)))
		   (add-to-list 'choices (ada-gnat-ada-name-from-file-name unit-file))))
	       );; while

	     (cond
	      ((= 0 (length choices))
	       (setq unit-name nil))

	      ((= 1 (length choices))
	       (setq unit-name (car choices)))

	      (t ;; multiple choices
	       (setq unit-name
		     (completing-read "package name: " choices)))
	      );; cond

	     (when unit-name
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
	       (cond
		((looking-at (concat unit-name "\\."))
		 (ada-fix-add-with-clause unit-name))
		(t
		 (ada-fix-insert-unit-name unit-name)
		 (insert ".")))
	       t) ;; success, else nil => fail
	     ))

	  ((looking-at (concat ada-gnat-quoted-name-regexp " is undefined"))
	   ;; We either need to add a with_clause for a package, or
	   ;; something is spelled wrong.
	   (save-excursion
	     (let ((unit-name (match-string 1))
		   (correct-spelling (ada-gnat-misspelling)))
	       (if correct-spelling
		   (progn
		     (pop-to-buffer source-buffer)
		     (search-forward unit-name)
		     (replace-match correct-spelling))

		 ;; else assume missing with
		 (pop-to-buffer source-buffer)
		 (ada-fix-add-with-clause unit-name))))
	   t)

	  ((looking-at (concat ada-gnat-quoted-name-regexp " is undefined"))
	   ;; We either need to add a with_clause for a package, or
	   ;; something is spelled wrong.
	   (save-excursion
	     (let ((unit-name (match-string 1))
		   (correct-spelling (ada-gnat-misspelling)))
	       (if correct-spelling
		   (progn
		     (pop-to-buffer source-buffer)
		     (search-forward unit-name)
		     (replace-match correct-spelling))

		 ;; else assume missing with
		 (pop-to-buffer source-buffer)
		 (ada-fix-add-with-clause unit-name))))
	   t)

	  ((looking-at (concat ada-gnat-quoted-name-regexp " not declared in " ada-gnat-quoted-name-regexp))
	   (save-excursion
	     (let ((child-name (match-string 1))
		   (correct-spelling (ada-gnat-misspelling)))
	       (if correct-spelling
		   (progn
		     (setq correct-spelling (match-string 1))
		     (pop-to-buffer source-buffer)
		     (search-forward child-name)
		     (replace-match correct-spelling))

		 ;; else guess that "child" is a child package, and extend the with_clause
		 (pop-to-buffer source-buffer)
		 (ada-fix-extend-with-clause child-name))))
	   t)

	  ((looking-at (concat ada-gnat-quoted-punctuation-regexp
			       " should be "
			       ada-gnat-quoted-punctuation-regexp))
	   (let ((bad (match-string-no-properties 1))
		 (good (match-string-no-properties 2)))
	     (pop-to-buffer source-buffer)
	     (looking-at bad)
	     (delete-region (match-beginning 0) (match-end 0))
	     (insert good))
	   t)

;;;; strings
	  ((looking-at (concat "\"end " ada-name-regexp ";\" expected"))
	   (let ((expected-name (match-string 1)))
	     (pop-to-buffer source-buffer)
	     (if (looking-at (concat "end " ada-name-regexp ";"))
		 (progn
		   (goto-char (match-end 1))   ; just before ';'
		   (delete-region (match-beginning 1) (match-end 1)))
	       ;; else we have just 'end;'
	       (forward-word 1)
	       (insert " "))
	     (insert expected-name))
	   t)

	  ((looking-at "expected an access type")
	   (progn
	     (set-buffer source-buffer)
	     (backward-char 1)
	     (when (looking-at "\\.all")
	       (delete-char 4)
	       t)))

	  ((looking-at (concat "expected \\(private \\)?type " ada-gnat-quoted-name-regexp))
	   (let ((type (match-string 2)))
	     (forward-line 1)
	     (move-to-column message-column)
	     (when (or (looking-at "found type access")
		       (looking-at "found type .*_Access_Type"))
	       ;; assume just need '.all'
	       (pop-to-buffer source-buffer)
	       (forward-word 1)
	       (insert ".all")
	       t)))

	  ((looking-at "extra \".\" ignored")
	   (set-buffer source-buffer)
	   (delete-char 1)
	   t)

	  ((looking-at "\\(?:possible \\)?missing \"with \\([a-zA-Z0-9_.]+\\);")
	   ;; also 'possible missing "with Ada.Text_IO; use Ada.Text_IO"' - ignoring the 'use'
	   (let ((package-name (match-string-no-properties 1)))
	     (pop-to-buffer source-buffer)
	     ;; FIXME (later): should check if prefix is already with'd, extend it
	     (ada-fix-add-with-clause package-name))
	   t)

	  ;; must be after above
	  ((looking-at "missing \"\\(.+\\)\"")
	   (let ((stuff (match-string-no-properties 1)))
	     (set-buffer source-buffer)
	     (insert (concat stuff)));; if missing ")", don't need space; otherwise do?
	   t)

	  ((looking-at "No legal interpretation for operator")
	   (forward-line 1)
	   (move-to-column message-column)
	   (looking-at (concat "use clause on " ada-gnat-quoted-name-regexp))
	   (let ((package (match-string 1)))
	     (pop-to-buffer source-buffer)
	     (ada-fix-add-use package))
	   t)

	  ((looking-at (concat "no selector " ada-gnat-quoted-name-regexp))
	   ;; Check next line for spelling error.
	   (save-excursion
	     (let ((unit-name (match-string 1))
		   (correct-spelling (ada-gnat-misspelling)))
	       (when correct-spelling
		 (pop-to-buffer source-buffer)
		 (search-forward unit-name)
		 (replace-match correct-spelling)
		 t))))

	  ((looking-at (concat "operator for \\(private \\)?type " ada-gnat-quoted-name-regexp))
	   (let ((type (match-string 2)))
	     (pop-to-buffer source-buffer)
	     (ada-goto-declarative-region-start)
	     (newline-and-indent)
	     (insert "use type " type ";"))
	   t)

	  ((looking-at "parentheses required for unary minus")
	   (set-buffer source-buffer)
	   (insert "(")
	   (forward-word 1)
	   (insert ")")
	   t)

	  ((looking-at "prefix of dereference must be an access type")
	   (pop-to-buffer source-buffer)
	   ;; point is after '.' in '.all'
	   (delete-region (- (point) 1) (+ (point) 3))
	   t)

;;;; warnings
	  ((looking-at (concat "warning: " ada-gnat-quoted-name-regexp " is not modified, could be declared constant"))
	   (pop-to-buffer source-buffer)
	   (search-forward ":")
	   (forward-comment (- (point-max) (point)))
	   ;; "aliased" must be before "constant", so check for it
	   (when (looking-at "aliased")
	     (forward-word 1)
	     (forward-char 1))
	   (insert "constant ")
	   t)

	  ((looking-at (concat "warning: formal parameter " ada-gnat-quoted-name-regexp " is not referenced"))
	   (let ((param (match-string 1)))
	     (pop-to-buffer source-buffer)
	     (ada-goto-declarative-region-start)
	     (newline-and-indent)
	     (insert "pragma Unreferenced (" param ");"))
	   t)

	  ((or
	    (looking-at (concat "warning: no entities of " ada-gnat-quoted-name-regexp " are referenced$"))
	    (looking-at (concat "warning: unit " ada-gnat-quoted-name-regexp " is never instantiated$"))
	    (looking-at "warning: redundant with clause"))
	   ;; just delete the 'with'; assume it's on a line by itself.
	   (pop-to-buffer source-buffer)
	   (beginning-of-line)
	   (delete-region (point) (progn (forward-line 1) (point)))
	   t)

	  ((looking-at (concat "warning: variable " ada-gnat-quoted-name-regexp " is assigned but never read"))
	   (let ((param (match-string 1)))
	     (pop-to-buffer source-buffer)
	     (ada-goto-end)
	     (newline-and-indent)
	     (insert "pragma Unreferenced (" param ");"))
	   t)

	  ((looking-at (concat "warning: unit " ada-gnat-quoted-name-regexp " is not referenced$"))
	   ;; just delete the 'with'; assume it's on a line by itself.
	   (pop-to-buffer source-buffer)
	   (beginning-of-line)
	   (delete-region (point) (progn (forward-line 1) (point)))
	   t)

;;;; style errors
	  ((looking-at "(style) \".*\" in wrong column")
	   (progn
	     (set-buffer source-buffer)
	     (funcall indent-line-function))
	   t)

	  ((looking-at "(style) bad capitalization, mixed case required")
	   (progn
	     (set-buffer source-buffer)
	     (forward-word)
	     (ada-case-adjust-identifier)
	     t))

	  ((looking-at (concat "(style) bad casing of " ada-gnat-quoted-name-regexp))
	   (let ((correct (match-string-no-properties 1))
		 end)
	     ;; gnat leaves point on first bad character, but we need to replace the whole word
	     (set-buffer source-buffer)
	     (skip-syntax-backward "w_")
	     (setq end (point))
	     (skip-syntax-forward "w_")
	     (delete-region (point) end)
	     (insert correct))
	   t)

	  ((or
	    (looking-at "(style) bad column")
	    (looking-at "(style) bad indentation")
	    (looking-at "(style) incorrect layout"))
	   (set-buffer source-buffer)
	   (funcall indent-line-function)
	   t)

         ((looking-at "(style) missing \"overriding\" indicator")
          (set-buffer source-buffer)
          (cond
           ((looking-at "\\(procedure\\)\\|\\(function\\)")
            (insert "overriding ")
	    t)
           (t
            nil)))

	  ((looking-at "(style) space not allowed")
	   (set-buffer source-buffer)
	   ;; Error places point on space. More than one trailing space
	   ;; should be fixed by delete-trailing-whitespace in
	   ;; before-save-hook, once the file is modified.
	   (delete-char 1)
	   t)

	  ((looking-at "(style) space required")
	   (set-buffer source-buffer)
	   (insert " ")
	   t)
	  )));; end of setq unwind-protect cond
    (if result
	t
      (goto-char start-pos)
      nil)
    ))

;;;;; setup

(defun ada-gnat-setup ()
  (setq ada-compiler 'gnat)

  (set (make-local-variable 'ada-file-name-from-ada-name) 'ada-gnat-file-name-from-ada-name)
  (set (make-local-variable 'ada-ada-name-from-file-name) 'ada-gnat-ada-name-from-file-name)
  (set (make-local-variable 'ada-make-package-body) 'ada-gnat-make-package-body)

  (font-lock-add-keywords nil
   ;; gnatprep preprocessor line
   (list (list "^[ \t]*\\(#.*\n\\)"  '(1 font-lock-type-face t))))

  (add-hook 'ada-syntax-propertize-hook 'ada-gnat-syntax-propertize)

  (when (boundp 'smie-indent-functions)
    (add-to-list 'smie-indent-functions 'ada-gnatprep-indent))

  (when (boundp 'wisi-indent-calculate-functions)
    (add-to-list 'wisi-indent-calculate-functions 'ada-gnatprep-indent))
)

;; add at end, so it is after ada-smie-setup, and can modify smie-indent-functions
(add-hook 'ada-mode-hook 'ada-gnat-setup t)

(setq-default ada-compiler 'gnat)

;; don't need ada-prj-default-function
(add-to-list 'ada-xref-other-function  (cons 'gnat 'ada-gnat-xref-other))
(add-to-list 'ada-xref-all-function    (cons 'gnat 'ada-gnat-xref-all))
(add-to-list 'ada-prj-parser-alist     (cons "gpr" 'ada-gnat-parse-gpr))
(add-to-list 'ada-prj-parse-file-ext   (cons 'gnat 'ada-gnat-prj-parse-emacs-file))
(add-to-list 'ada-prj-parse-file-final (cons 'gnat 'ada-gnat-prj-parse-emacs-final))

(add-hook 'ada-gnat-fix-error-hook 'ada-gnat-fix-error)
(add-to-list 'ada-fix-error-alist (cons 'gnat 'ada-gnat-fix-error-hook))

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

; ignore gnat library files
(add-to-list 'completion-ignored-extensions ".ali")

;; gnatmake -gnatD generates files with .dg extensions. But we don't
;; need to navigate between them.
;;
;; There is no common convention for a file extension for gnatprep files.

(provide 'ada-gnat)
(provide 'ada-compiler)

;; end of file
